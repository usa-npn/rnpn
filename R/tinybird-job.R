# The async core: submit -> poll -> download.
#
# Product-agnostic. Everything here takes an endpoint path and a payload and
# knows nothing about status & intensity data specifically.

#' Base request against the NPN data service
#'
#' **Every request must have a timeout.** curl's default overall timeout is
#' infinite, so a connection that opens and then stalls blocks forever. That
#' matters most on the status poll: the job `timeout` is only checked *between*
#' polls, so a request that never returns hangs the whole call and no amount of
#' waiting produces the error that would tell the user what happened.
#'
#' The stall this actually guards against is a stale keep-alive connection ---
#' the submit POST returns 202, the server closes the connection, and curl
#' reuses the dead socket for the next request. `retry_on_failure = TRUE` is
#' what makes that self-healing: the timeout ends the stalled attempt and the
#' retry opens a fresh connection.
#' @noRd
tb_base_req <- function(timeout = getOption("rnpn.request_timeout", 30)) {
  req <- httr2::request(base_npn_data_url()) %>%
    httr2::req_timeout(timeout) %>%
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) %>%
    httr2::req_user_agent(npn_user_agent) %>%
    httr2::req_error(body = tb_error_body)

  # Escape hatch for networks where a pooled connection goes dead between the
  # submit and the first poll. Costs a TLS handshake (~850ms) per request, so
  # it is opt-in until there is evidence it is needed by default.
  if (isTRUE(getOption("rnpn.fresh_connections", FALSE))) {
    req <- httr2::req_options(req, fresh_connect = 1, forbid_reuse = 1)
  }
  req
}

#' Pull a useful message out of an error response body
#'
#' The service reports failures as `{"error": "..."}`; without this the user
#' sees only the status code.
#' @noRd
tb_error_body <- function(resp) {
  body <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(body)) {
    return(NULL)
  }
  msg <- body$error %|||% body$message %|||% body$error_message
  if (is.character(msg)) msg else NULL
}

#' Queue an export job
#'
#' @param product An entry of `tb_products`.
#' @param payload A payload from `tb_build_payload()`.
#' @returns The job id, as a character scalar.
#' @noRd
tb_submit_job <- function(product, payload, call = rlang::caller_env()) {
  resp <- tb_base_req() %>%
    httr2::req_url_path_append(product$path) %>%
    httr2::req_method("POST") %>%
    httr2::req_body_json(tb_json_payload(payload), auto_unbox = TRUE) %>%
    httr2::req_perform(error_call = call) %>%
    httr2::resp_body_json()

  job_id <- resp$job_id
  if (!is.character(job_id) || length(job_id) != 1) {
    rlang::abort(
      c(
        "The service accepted the export but did not return a job id.",
        "i" = "This is a server-side problem; please try again."
      ),
      call = call
    )
  }
  message("Export job submitted: ", job_id)
  job_id
}

#' Ask the service how a job is doing
#'
#' @returns A list with `status` and, when complete, `download_url`.
#' @noRd
tb_job_status <- function(job_id, call = rlang::caller_env()) {
  tb_base_req() %>%
    httr2::req_url_path_append("v1/data/job/status", job_id) %>%
    httr2::req_perform(error_call = call) %>%
    httr2::resp_body_json()
}

#' Block until a job finishes, and return its download URL
#'
#' The schedule --- first check at 5s, every 10s after --- is calibrated against
#' observed durations: 3 days of data finishes in under 3s, six species over a
#' year in ~10s, all species over a year in ~21s. It is not worth tuning
#' further against that distribution.
#'
#' @param first_delay Seconds to wait before the first check. Zero when
#'   resuming an existing job, which is usually already finished.
#' @param interval Seconds between subsequent checks.
#' @noRd
tb_await_job <- function(
  job_id,
  timeout = 300,
  first_delay = 5,
  interval = 10,
  call = rlang::caller_env()
) {
  deadline <- Sys.time() + timeout
  started <- Sys.time()
  delay <- first_delay
  next_report <- 30
  last_error <- NULL

  # Say that the wait has begun. Without this the console goes silent between
  # submission and the first 30s progress report, which is indistinguishable
  # from a hang.
  message(
    "Waiting for the export (checking every ", interval,
    "s, giving up after ", timeout, "s)..."
  )

  repeat {
    remaining <- as.numeric(difftime(deadline, Sys.time(), units = "secs"))
    if (remaining <= 0) {
      tb_abort_timeout(job_id, timeout, last_error = last_error, call = call)
    }
    if (delay > 0) {
      Sys.sleep(min(delay, remaining))
    }
    delay <- interval

    # A failed poll must not kill a job that is running perfectly well on the
    # server. Keep polling until the deadline and report the last transport
    # error only if we never manage a successful check.
    status <- tryCatch(
      tb_job_status(job_id, call = call),
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )
    if (!is.null(status)) {
      last_error <- NULL
    }

    if (identical(status$status, "complete")) {
      url <- status$download_url
      if (!is.character(url) || length(url) != 1) {
        rlang::abort(
          c(
            "The job completed but no download URL was returned.",
            "i" = paste0('Try again with: npn_get_job("', job_id, '")')
          ),
          call = call
        )
      }
      return(url)
    }

    if (identical(status$status, "error")) {
      rlang::abort(
        c(
          "The export job failed on the server.",
          "x" = status$error_message %|||% "No reason was given.",
          "i" = paste0("Job id: ", job_id)
        ),
        call = call
      )
    }

    elapsed <- as.numeric(difftime(Sys.time(), started, units = "secs"))
    if (elapsed >= next_report) {
      state <- if (is.null(status)) {
        "cannot reach the server, still trying"
      } else {
        status$status %|||% "unknown"
      }
      message(
        "Still working (", round(elapsed), "s elapsed, status: ", state, ")..."
      )
      next_report <- next_report + 30
    }
  }
}

#' Report a timeout as an error, never as a return value
#'
#' A function that promised a tibble must return one or raise; handing back a
#' job-id string instead is the same type instability the `as` contract exists
#' to prevent, just moved onto the failure path.
#'
#' The error is recoverable because the job does not die: the server regenerates
#' a fresh presigned URL on every status poll, and the artifact lives ~8 days.
#' @noRd
tb_abort_timeout <- function(
  job_id,
  timeout,
  last_error = NULL,
  call = rlang::caller_env()
) {
  # If every poll failed, the job is not the problem and saying "the server is
  # still working on it" would send the user looking in the wrong place.
  if (!is.null(last_error)) {
    rlang::abort(
      c(
        paste0("Gave up waiting for the export after ", timeout, "s."),
        "x" = paste0("Could not reach the job status endpoint: ", last_error),
        "i" = "The job itself may well have finished.",
        "i" = paste0('Try again with: npn_get_job("', job_id, '")')
      ),
      class = c("rnpn_unreachable_error", "rnpn_timeout_error"),
      call = call
    )
  }
  rlang::abort(
    c(
      paste0("Job did not finish within ", timeout, "s."),
      "i" = "It has not failed \u2014 the server is still working on it.",
      "i" = paste0('Collect it later with: npn_get_job("', job_id, '")')
    ),
    class = "rnpn_timeout_error",
    call = call
  )
}

#' Download a finished artifact to `dest`
#'
#' The two export paths deliver differently and neither header nor filename can
#' be trusted, so the bytes decide: `1f 8b` means gzip. Code written against
#' `Content-Encoding`, `Content-Type`, or the filename breaks on the other path.
#' Three filename variants have been observed so far.
#'
#' The cache entry is always named `.csv.gz`, because DuckDB infers compression
#' from the file extension and a gzip stream saved as `.csv` fails with a
#' confusing CSV-dialect sniffing error rather than a compression error. When
#' the response arrives already decompressed we compress it rather than lie
#' about the name.
#' @noRd
tb_fetch_artifact <- function(url, dest, call = rlang::caller_env()) {
  tmp <- tempfile(fileext = ".download")
  on.exit(unlink(tmp), add = TRUE)

  # No fixed timeout here: a legitimate export can be hundreds of MB and take
  # minutes. What must not be tolerated is a transfer that *stalls*, so curl is
  # told to give up if throughput stays under a byte a second for a minute.
  # That distinguishes "slow" from "hung", which a wall-clock timeout cannot.
  httr2::request(url) %>%
    httr2::req_retry(max_tries = 3, retry_on_failure = TRUE) %>%
    httr2::req_options(
      low_speed_limit = 1,
      low_speed_time = getOption("rnpn.stall_timeout", 60)
    ) %>%
    httr2::req_user_agent(npn_user_agent) %>%
    httr2::req_progress() %>%
    httr2::req_perform(path = tmp, error_call = call)

  tb_ensure_dir(dirname(dest))
  if (tb_is_gzip(tmp)) {
    file.copy(tmp, dest, overwrite = TRUE)
  } else {
    tb_gzip_file(tmp, dest)
  }
  dest
}

#' Does this file start with the gzip magic number?
#' @noRd
tb_is_gzip <- function(path) {
  con <- file(path, "rb")
  on.exit(close(con), add = TRUE)
  magic <- readBin(con, "raw", n = 2)
  length(magic) == 2 && identical(magic, as.raw(c(0x1f, 0x8b)))
}

#' Compress a file, in chunks, so the cache entry earns its `.csv.gz` name
#' @noRd
tb_gzip_file <- function(src, dest, chunk_size = 1024 * 1024) {
  input <- file(src, "rb")
  on.exit(close(input), add = TRUE)
  output <- gzfile(dest, "wb")
  on.exit(close(output), add = TRUE)

  repeat {
    chunk <- readBin(input, "raw", n = chunk_size)
    if (length(chunk) == 0) {
      break
    }
    writeBin(chunk, output)
  }
  dest
}
