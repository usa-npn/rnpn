# The pre-flight guard.
#
# Advisory only. It fires exactly one warning and can never block, refuse, or
# prompt.

#' How long the advisory guard may spend before giving up
#'
#' The count itself takes ~0.4s. Anything beyond a few seconds means something
#' is wrong, and the right response is to skip the warning rather than delay a
#' download for it.
#' @noRd
tb_guard_timeout <- function() {
  getOption("rnpn.guard_timeout", 10)
}

#' Fetch a Tinybird JWT, reusing the cached one until it expires
#'
#' Tokens last an hour, so fetching one per call would be pure latency. Cached
#' in `pkg.env` alongside the package's other in-memory caches.
#' @noRd
tb_token <- function() {
  cached <- pkg.env$tb_token
  if (!is.null(cached) && cached$expires_at > Sys.time() + 60) {
    return(cached$token)
  }

  # Short timeout on purpose. The guard is advisory and fails open, so it must
  # never be able to add meaningful latency to a download that is going to
  # happen anyway.
  body <- tb_base_req(timeout = tb_guard_timeout()) %>%
    httr2::req_url_path_append("v1/data/token") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  expires <- as.POSIXct(
    body$expires_at,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
  if (is.na(expires)) {
    expires <- Sys.time() + 300
  }
  pkg.env$tb_token <- list(token = body$token, expires_at = expires)
  body$token
}

#' Forget the cached Tinybird token
#'
#' Used by tests, which must not share state.
#' @noRd
tb_token_reset <- function() {
  pkg.env$tb_token <- NULL
  invisible(NULL)
}

#' Count the rows a payload would return
#'
#' **Fails open.** If the token fetch or the count call fails for any reason
#' this returns `NULL` and the caller proceeds as though the count were under
#' threshold. The guard is advisory and must never be able to block a download.
#'
#' @returns The row count, or `NULL` if it could not be determined.
#' @noRd
tb_count_rows <- function(product, payload) {
  tryCatch(
    {
      body <- httr2::request(base_tinybird_url()) %>%
        httr2::req_timeout(tb_guard_timeout()) %>%
        httr2::req_user_agent(npn_user_agent) %>%
        httr2::req_url_path_append("v0/pipes", paste0(product$count_pipe, ".json")) %>%
        httr2::req_url_query(!!!tb_count_params(payload)) %>%
        httr2::req_auth_bearer_token(tb_token()) %>%
        httr2::req_perform() %>%
        httr2::resp_body_json()

      count <- body$data[[1]]$total_records
      if (is.numeric(count)) count else NULL
    },
    error = function(e) NULL
  )
}

#' Warn when a query is too large to comfortably materialize
#'
#' Fires only for `as = "data"`, the sole mode that materializes rows. Running a
#' count for `"path"` or `"lazy"` would be pure latency for a warning that
#' cannot apply.
#'
#' Warn, never refuse, never prompt. One `rlang::warn()` behaves identically at
#' the console, inside `Rscript`, in a knitted document, and under `targets`.
#' An `if (interactive())` prompt would fire only at the console --- the one
#' place the user can already hit Ctrl-C --- and is actively harmful in CI,
#' where a hang is most expensive and least visible.
#' @noRd
tb_preflight <- function(product, payload, threshold = tb_row_warn_threshold) {
  n <- tb_count_rows(product, payload)
  if (is.null(n) || n < threshold) {
    return(invisible(NULL))
  }
  rlang::warn(tb_size_warning(n), class = "rnpn_large_query_warning")
  invisible(n)
}

#' The text of the large-query warning
#'
#' Split out so it can be tested without a network. The memory figures come from
#' a measured 176.1 bytes/row on a real 4.36M-row export.
#' @noRd
tb_size_warning <- function(n) {
  gb <- n * tb_bytes_per_row / 2^30
  paste0(
    "This query matches ",
    format(n, big.mark = ",", scientific = FALSE, trim = TRUE),
    " records \u2014 about ",
    formatC(gb, format = "f", digits = 1),
    " GB as a tibble, and roughly ",
    formatC(gb * tb_working_multiplier, format = "f", digits = 1),
    " GB while dplyr works on it. That may exceed available memory. Consider ",
    'as = "path" to download without loading, as = "lazy" to query it in ',
    "place, or narrowing your filters."
  )
}
