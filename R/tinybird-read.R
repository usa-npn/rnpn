# Turning a cached `.csv.gz` into a tibble.
#
# Two engines sit behind one contract. DuckDB is the default and the one every
# measurement behind this design was taken against. readr is the escape hatch:
# it is already in `Imports`, so it needs nothing installed, and it never loads
# a threaded C++ library --- which matters when something in the environment
# cannot tolerate one (see `tb_engine()`).
#
# Whichever engine reads the bytes, `coerce_known_cols()` applies the declared
# types afterwards, so `status_col_types` remains the single source of truth and
# the type-stability contract holds identically on both paths.

#' Which engine reads the artifact
#'
#' DuckDB unless asked otherwise. `options(rnpn.engine = "readr")` switches to
#' the pure-R reader.
#'
#' The reason that switch exists: `DBI::dbConnect(duckdb::duckdb())` takes a
#' native lock and starts a thread pool, and some debuggers deadlock against it
#' --- the debugger inspects objects that need the same mutex the paused R code
#' is holding, and neither side can proceed. No timeout can rescue that, because
#' no R code is running to notice. Being able to take DuckDB out of the picture
#' entirely is the only reliable way to step through this package.
#' @noRd
tb_engine <- function(call = rlang::caller_env()) {
  engine <- getOption("rnpn.engine", "duckdb")
  if (!isTRUE(engine %in% c("duckdb", "readr"))) {
    rlang::abort(
      c(
        '`rnpn.engine` must be either "duckdb" or "readr".',
        "x" = paste0("You set it to: ", deparse(engine))
      ),
      call = call
    )
  }
  engine
}

#' Check for the read engine before submitting anything
#'
#' The check fires **before job submission**, not at parse time. DuckDB is a
#' large C++ build and a Linux user without binaries can wait ten-plus minutes;
#' nobody should discover the dependency after a 300s poll has already run.
#'
#' `as = "path"` never reads the file, so it needs nothing. `as = "lazy"` is
#' DuckDB-only whatever the engine option says: a lazy table is a database
#' connection, and readr has nothing to offer there.
#' @noRd
tb_check_engine <- function(as, call = rlang::caller_env()) {
  if (identical(as, "path")) {
    return(invisible(NULL))
  }

  if (identical(as, "lazy")) {
    if (identical(tb_engine(call = call), "readr")) {
      rlang::abort(
        c(
          '`as = "lazy"` needs DuckDB, but `rnpn.engine` is set to "readr".',
          "i" = 'A lazy table is a live database connection; readr cannot provide one.',
          "i" = 'Use `as = "data"` or `as = "path"`, or unset `rnpn.engine`.'
        ),
        call = call
      )
    }
    rlang::check_installed(
      c("duckdb", "dbplyr"),
      reason = 'to return `as = "lazy"`.',
      call = call
    )
    return(invisible(NULL))
  }

  if (identical(tb_engine(call = call), "duckdb")) {
    rlang::check_installed(
      "duckdb",
      reason = 'to return `as = "data"`.',
      call = call
    )
  }
  invisible(NULL)
}

#' Read a cached export into a tibble
#'
#' Dispatches on the configured engine. Both paths drop the bookkeeping columns
#' and end at [coerce_known_cols()], so the returned tibble is the same either
#' way for every column whose type is declared.
#' @noRd
tb_read_data <- function(path, types = status_col_types) {
  switch(
    tb_engine(),
    duckdb = tb_read_duckdb(path, types),
    readr = tb_read_readr(path, types)
  )
}

#' Read with readr, loading nothing that is not already a dependency
#'
#' Everything is read as character and then cast, rather than letting readr
#' guess. Guessing is what produces a `logical` column from an all-empty one,
#' which is the exact bug the type contract exists to prevent, and it would
#' reintroduce it for any column not in `status_col_types`.
#'
#' Both null sentinels are handled here, the same pair DuckDB is given: the
#' service writes `\\N` for some columns and `""` for others in the same file.
#' @noRd
tb_read_readr <- function(path, types = status_col_types) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("\\N", ""),
    progress = FALSE,
    show_col_types = FALSE
  )
  df <- df[, !names(df) %in% tb_metadata_cols, drop = FALSE]
  coerce_known_cols(df, types)
}

#' Apply the declared column types
#'
#' Works identically on zero rows, which is the outcome that happens most often:
#' 54% of real user searches return nothing, and an empty result still carries
#' its header row, so casting an empty character column yields a correctly-typed
#' empty column.
#'
#' [dplyr::any_of()] is what lets unknown and missing columns both pass, and it
#' matches the existing house fix for this same bug in `npn_get_data()`.
#' @noRd
coerce_known_cols <- function(df, types = status_col_types) {
  for (type in unique(unname(types))) {
    cols <- names(types)[types == type]
    df <- dplyr::mutate(
      df,
      dplyr::across(dplyr::any_of(cols), tb_coercers[[type]])
    )
  }
  tibble::as_tibble(df)
}
