#creates an environment used for in-memory cacheing
pkg.env <- new.env(parent = emptyenv())

#' Tear down the session's duckdb connection when the package is unloaded
#'
#' `R CMD check` notices dangling connections. `shutdown = TRUE` stops the
#' embedded database as well as closing the handle.
#' @noRd
.onUnload <- function(libpath) {
  if (!is.null(pkg.env$duckdb_con) && requireNamespace("DBI", quietly = TRUE)) {
    npn_duckdb_reset()
  }
}
