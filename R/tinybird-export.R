# The generic async core, and the one product wired to it.
#
# `tb_run_export()` knows nothing about status & intensity data. Adding the
# remaining three products (M3) is an entry in `tb_products` plus a wrapper that
# gathers arguments; the flow below does not change.

#' Ensure a cache entry exists, downloading it if it does not
#'
#' @param fetch A function of one argument (the destination path) that puts the
#'   artifact there. The submit/poll/download path and the resume-a-job path
#'   differ only in this closure.
#' @noRd
tb_obtain <- function(key, refresh = FALSE, fetch) {
  path <- tb_cache_path(key)
  if (!refresh && tb_cache_fresh(path)) {
    message(
      "Using a cached copy of this export. ",
      "Pass `refresh = TRUE` to download it again."
    )
    return(path)
  }
  tb_ensure_dir(npn_cache_dir())
  fetch(path)
  path
}

#' Keep a durable copy at a user-chosen path
#'
#' `file_path` is orthogonal to `as` and determines only *where the artifact is
#' kept*. The two compose: `as = "data", file_path = "status.csv.gz"` means
#' "give me a tibble **and** keep a permanent copy".
#' @noRd
tb_copy_to <- function(src, file_path, call = rlang::caller_env()) {
  dest <- path.expand(file_path)
  parent <- dirname(dest)
  if (!dir.exists(parent)) {
    rlang::abort(
      c(
        "`file_path` points into a directory that does not exist.",
        "x" = paste0("No such directory: ", parent),
        "i" = "Create it first, or choose another path."
      ),
      call = call
    )
  }
  if (!file.copy(src, dest, overwrite = TRUE)) {
    rlang::abort(
      paste0("Could not write the export to ", dest, "."),
      call = call
    )
  }
  dest
}

#' Return a cached artifact in the requested shape
#'
#' `as` is the sole determinant of the return type. Nothing else influences it
#' --- not data volume, not `file_path`, not whether the cache hit. A function
#' whose class depends on how much data came back breaks in production the day
#' someone widens a date range, with an error message that says nothing about
#' the cause.
#' @noRd
tb_deliver <- function(
  cache_path,
  key,
  as,
  file_path = NULL,
  types = status_col_types,
  call = rlang::caller_env()
) {
  out_path <- cache_path
  if (!is.null(file_path)) {
    out_path <- tb_copy_to(cache_path, file_path, call = call)
  }
  switch(
    as,
    path = out_path,
    data = tb_read_data(cache_path, types),
    lazy = tb_lazy_tbl(cache_path, key, types)
  )
}

#' Submit an export, wait for it, and return it in the requested shape
#'
#' The product-agnostic core: submit -> poll -> download -> ingest -> return.
#' @noRd
tb_run_export <- function(
  product,
  payload,
  as,
  file_path = NULL,
  wait = TRUE,
  timeout = 300,
  refresh = FALSE,
  types = status_col_types,
  call = rlang::caller_env()
) {
  tb_check_engine(as, call = call)

  if (!wait) {
    # A bare job-id string, no S3 class. The cache is bypassed: the promise here
    # is a job id, and there is no job behind a cache hit.
    return(tb_submit_job(product, payload, call = call))
  }

  key <- tb_query_key(payload)
  path <- tb_obtain(
    key,
    refresh = refresh,
    fetch = function(dest) {
      if (identical(as, "data")) {
        tb_preflight(product, payload)
      }
      job_id <- tb_submit_job(product, payload, call = call)
      url <- tb_await_job(job_id, timeout = timeout, first_delay = 5, call = call)
      tb_fetch_artifact(url, dest, call = call)
    }
  )
  tb_deliver(path, key, as, file_path, types = types, call = call)
}

#' Download status and intensity data
#'
#' Requests a status and intensity export, waits for the service to build it,
#' and returns it. The service builds exports asynchronously, so a call blocks
#' while the job runs --- typically a few seconds, longer for a wide date range.
#'
#' `r lifecycle::badge("experimental")` This is a prototype sibling of
#' [npn_download_status_data()], not a replacement for it. Its arguments may
#' change.
#'
#' @param start_date,end_date Optional bounds on the observation date, as `Date`
#'   objects or `"YYYY-MM-DD"` strings. Both are optional; omitting them
#'   requests the entire corpus, which is very large.
#' @param species_ids Optional vector of species ids to filter on. See
#'   [npn_species()].
#' @param phenophase_ids Optional vector of phenophase ids to filter on. See
#'   [npn_phenophases()].
#' @param site_ids Optional vector of site ids to filter on. These match the
#'   `site_id` column of the returned data. See [npn_stations()].
#' @param states Optional vector of two-letter state abbreviations.
#' @param phenophase_categories Optional vector of phenophase category names.
#' @param ... Additional filters, passed to the service as-is. Names are checked
#'   against the fields the service accepts and an unrecognized name is an
#'   error, because the service itself silently ignores anything it does not
#'   recognize --- an unchecked typo would widen your query rather than fail it.
#'   Useful ones include `pheno_class_ids`, `network_ids`, `functional_types`,
#'   and the bounding box fields `bottom_left_x1`, `bottom_left_y1`,
#'   `upper_right_x2`, `upper_right_y2`.
#' @param as What to return. `"data"` (the default) returns a tibble; `"path"`
#'   returns the path to the downloaded `.csv.gz` file without reading it;
#'   `"lazy"` returns a duckdb-backed table you can query with dplyr verbs
#'   without loading it into memory. This argument is the *only* thing that
#'   determines the return type. `"data"` and `"lazy"` use duckdb; set
#'   `options(rnpn.engine = "readr")` to parse with readr instead and skip
#'   duckdb entirely, which rules out `"lazy"` but needs nothing installed.
#' @param file_path Optional path at which to keep a durable copy of the
#'   downloaded `.csv.gz`. Independent of `as`: you can ask for a tibble and a
#'   saved file in the same call. The file is gzipped CSV, so name it
#'   accordingly. When `NULL` (the default) the download is kept only in the
#'   package cache.
#' @param wait If `TRUE` (the default), block until the export is ready. If
#'   `FALSE`, return the job id immediately and collect it later with
#'   [npn_get_job()].
#' @param timeout Seconds to wait before giving up. Timing out is an error, not
#'   a return value, and it does not cancel the job --- the error message tells
#'   you how to collect it later.
#' @param refresh If `TRUE`, ignore any cached copy of this exact query and
#'   download it again.
#'
#' @returns A tibble, a file path, or a lazy table, according to `as`. When
#'   `wait = FALSE`, the job id as a character scalar.
#'
#' @section Large queries:
#' When `as = "data"`, the number of matching records is checked before the
#' export is submitted and a warning is issued if the result is unlikely to fit
#' comfortably in memory. The check is advisory: it never blocks a download, and
#' if it cannot be performed the download proceeds regardless.
#'
#' @section Keeping a lazy table:
#' A lazy table is a live connection to a file on disk, not data. It cannot be
#' saved with [saveRDS()] and reloaded in another session --- doing so gives you
#' an object that errors with `Invalid connection`. This is how dplyr behaves
#' against every database backend. To keep results across sessions, either
#' [dplyr::collect()] the table into a tibble first, or use `as = "path"` and
#' keep the file.
#'
#' @seealso [npn_get_job()], [npn_cache_clear()]
#' @export
#' @examples \dontrun{
#' # a tibble
#' obs <- npn_export_status_data(
#'   start_date = "2024-05-01",
#'   end_date = "2024-05-03",
#'   species_ids = 3
#' )
#'
#' # the same query, kept on disk and queried in place
#' tbl <- npn_export_status_data(
#'   start_date = "2024-05-01",
#'   end_date = "2024-05-03",
#'   species_ids = 3,
#'   as = "lazy"
#' )
#' tbl %>%
#'   dplyr::group_by(state, common_name) %>%
#'   dplyr::summarise(n = dplyr::n()) %>%
#'   dplyr::collect()
#' }
npn_export_status_data <- function(
  start_date = NULL,
  end_date = NULL,
  species_ids = NULL,
  phenophase_ids = NULL,
  site_ids = NULL,
  states = NULL,
  phenophase_categories = NULL,
  ...,
  as = c("data", "path", "lazy"),
  file_path = NULL,
  wait = TRUE,
  timeout = 300,
  refresh = FALSE
) {
  as <- rlang::arg_match(as)
  product <- tb_products$status

  payload <- tb_build_payload(
    named = list(
      start_date = tb_parse_date(start_date),
      end_date = tb_parse_date(end_date),
      species_ids = species_ids,
      phenophase_ids = phenophase_ids,
      site_ids = site_ids,
      states = states,
      phenophase_categories = phenophase_categories
    ),
    dots = rlang::list2(...),
    download_type = product$download_type
  )

  tb_run_export(
    product = product,
    payload = payload,
    as = as,
    file_path = file_path,
    wait = wait,
    timeout = timeout,
    refresh = refresh,
    types = status_col_types
  )
}

#' Collect a previously submitted export job
#'
#' Downloads the result of an export job that was submitted earlier, either with
#' `wait = FALSE` or by a call that timed out. Timing out does not cancel the
#' job: the server keeps building it and the result stays available for about a
#' week.
#'
#' @param job_id The job id, as reported when the job was submitted or in the
#'   timeout error message.
#' @param as,file_path,timeout,refresh As documented in
#'   [npn_export_status_data()].
#'
#' @returns A tibble, a file path, or a lazy table, according to `as`.
#'
#' @details
#' The large-query warning is not issued here. It counts the records a *filter*
#' matches, and a job id does not carry the filters that produced it.
#'
#' @seealso [npn_export_status_data()]
#' @export
#' @examples \dontrun{
#' job <- npn_export_status_data(species_ids = 3, wait = FALSE)
#' obs <- npn_get_job(job)
#' }
npn_get_job <- function(
  job_id,
  as = c("data", "path", "lazy"),
  file_path = NULL,
  timeout = 300,
  refresh = FALSE
) {
  as <- rlang::arg_match(as)
  if (!is.character(job_id) || length(job_id) != 1 || is.na(job_id)) {
    rlang::abort("`job_id` must be a single job id string.")
  }
  tb_check_engine(as)

  key <- tb_job_key(job_id)
  path <- tb_obtain(
    key,
    refresh = refresh,
    fetch = function(dest) {
      # first_delay = 0: a resumed job is usually finished already, so there is
      # nothing to gain by sleeping before the first check.
      url <- tb_await_job(job_id, timeout = timeout, first_delay = 0)
      tb_fetch_artifact(url, dest)
    }
  )
  tb_deliver(path, key, as, file_path)
}
