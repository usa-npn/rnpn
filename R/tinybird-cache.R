# The download cache.
#
# This is a *request deduplicator, not a freshness mechanism*. The problem it
# solves is a user re-running a script and pummelling the server; serving
# slightly stale data is explicitly acceptable. That framing is why it needs no
# coherence guarantee, no invalidation signal, and no per-query TTL tuning.

#' Where cached exports are kept
#'
#' Defaults to the session temp directory, so the package writes nothing durable
#' unless asked. This package has never written a persistent file to disk ---
#' every existing cache is in-memory and every disk touch is a temp file --- and
#' CRAN policy bars writing outside the session temp directory without user
#' confirmation. Setting the option *is* that confirmation, so there is no
#' prompt and no policy exposure.
#'
#' The cost, stated plainly: an analyst who edits a script, restarts R, and
#' re-runs misses the cache every time. That is the price of a CRAN-clean
#' default, and it is one `options()` call away from being fixed.
#'
#' @returns The cache directory path.
#' @noRd
npn_cache_dir <- function() {
  getOption("rnpn.cache_dir", file.path(tempdir(), "rnpn-cache"))
}

#' Cache keys
#'
#' Two namespaces. A query is keyed by the hash of its normalized payload; a
#' resumed job is keyed by its job id, because [npn_get_job()] cannot know the
#' filters that produced it.
#' @noRd
tb_query_key <- function(payload) {
  paste0("query-", tb_payload_hash(payload))
}

#' @rdname tb_query_key
#' @noRd
tb_job_key <- function(job_id) {
  paste0("job-", job_id)
}

#' Path of a cache entry
#'
#' `as` is deliberately not part of the key: the `.csv.gz` is identical
#' whichever way it is returned, and the parquet sibling is the same key with a
#' different extension.
#' @noRd
tb_cache_path <- function(key, ext = ".csv.gz") {
  file.path(npn_cache_dir(), paste0(key, ext))
}

#' Is there a usable cache entry at this path?
#' @noRd
tb_cache_fresh <- function(path, ttl_days = getOption("rnpn.cache_ttl_days", 1)) {
  if (!file.exists(path)) {
    return(FALSE)
  }
  age <- difftime(Sys.time(), file.mtime(path), units = "days")
  as.numeric(age) < ttl_days
}

#' @noRd
tb_ensure_dir <- function(path) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = TRUE, showWarnings = FALSE)
  }
  path
}

#' Clear the rnpn download cache
#'
#' Removes every cached export from the directory given by the
#' `rnpn.cache_dir` option. The default cache lives in the session temp
#' directory and cleans itself up when R exits, so this is mainly of use when
#' you have opted into a persistent cache.
#'
#' @returns Invisibly, the number of files removed.
#' @export
#' @examples
#' npn_cache_clear()
npn_cache_clear <- function() {
  dir <- npn_cache_dir()
  if (!dir.exists(dir)) {
    return(invisible(0L))
  }
  files <- list.files(dir, full.names = TRUE)
  unlink(files, recursive = TRUE, force = TRUE)
  message("Removed ", length(files), " cached file(s) from ", dir)
  invisible(length(files))
}
