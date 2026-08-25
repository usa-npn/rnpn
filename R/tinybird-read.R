# Turning a cached `.csv.gz` into a tibble.
#
# Two engines sit behind one contract. DuckDB is the default and the one every
# measurement behind this design was taken against. readr is the escape hatch:
# it is already in `Imports`, so it needs nothing installed, and it never loads
# a threaded C++ library --- which matters when something in the environment
# cannot tolerate one (see `tb_engine()`).
#
# Whichever engine reads the bytes, both apply the same two transformations in
# the same order: the header is renamed by `tb_output_names()`, then the
# product's declared types are applied. The product entry remains the single
# source of truth and the type-stability contract holds identically on both
# paths --- which the offline suite asserts by comparing the two.

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

#' The column names this product's data is returned under
#'
#' **The three metrics pipes emit legacy `PascalCase`; status data emits
#' `lower_snake_case`.** Existing users, every vignette and the legacy services
#' all use lowercase --- `first_yes_doy`, `mean_first_yes_doy` --- so the header
#' is lowercased on ingest and `as = "data"` / `as = "lazy"` hand back lowercase
#' names. `as = "path"` hands over the raw file untouched, the same deliberate
#' asymmetry as dropping `request_id` and `product` from the tibble.
#'
#' The transformation is a plain `tolower()`, which is not a simplification: the
#' legacy lowercase names *are* the PascalCase names lowercased. That was
#' measured on 2026-08-25 by diffing a real export header against a legacy
#' download of the same query, for all three products, and it was exact.
#' `name_fixups` is the escape hatch should a pipe ever rename a column, and it
#' is empty --- see `status_name_fixups` for the diff.
#'
#' **Hyphens are preserved.** `In-Phase_Sites_Sample_Size` becomes
#' `in-phase_sites_sample_size`, because legacy kept the hyphen too and
#' substituting `_` would be a divergence invented here.
#' @noRd
tb_output_names <- function(cols, product) {
  out <- if (isTRUE(product$lower_names)) tolower(cols) else cols
  fixups <- product$name_fixups
  hit <- out %in% names(fixups)
  out[hit] <- unname(fixups[out[hit]])
  out
}

#' The sink's `product` value, read out of a downloaded artifact
#'
#' Every export carries `request_id` then `product` as its first two columns,
#' and `product` is one of the four sink keys. That makes the file itself a
#' fallback answer to "which data product is this?" that survives a new session
#' and a cache hit --- see [npn_get_job()].
#'
#' Deliberately engine-free: two lines of gzipped text, no DuckDB connection and
#' no readr call, so it costs nothing and works under either engine. Returns
#' `NULL` for a header-only file, which has no `product` value to read.
#' @noRd
tb_file_product_key <- function(path) {
  tryCatch(
    {
      con <- gzfile(path, "rt")
      on.exit(close(con), add = TRUE)
      lines <- readLines(con, n = 2, warn = FALSE)
      if (length(lines) < 2) {
        return(NULL)
      }
      split_row <- function(line) {
        scan(
          text = line, what = "", sep = ",", quote = "\"",
          quiet = TRUE, strip.white = TRUE
        )
      }
      header <- tolower(split_row(lines[[1]]))
      row <- split_row(lines[[2]])
      at <- match("product", header)
      if (is.na(at) || at > length(row)) {
        return(NULL)
      }
      value <- row[[at]]
      if (!nzchar(value) || identical(value, "\\N")) NULL else value
    },
    error = function(e) NULL
  )
}

#' Read a cached export into a tibble
#'
#' Dispatches on the configured engine. Both paths rename the header, drop the
#' bookkeeping columns, and end at [coerce_known_cols()], so the returned tibble
#' is the same either way for every column whose type is declared.
#' @noRd
tb_read_data <- function(path, product) {
  switch(
    tb_engine(),
    duckdb = tb_read_duckdb(path, product),
    readr = tb_read_readr(path, product)
  )
}

#' Read with readr, loading nothing that is not already a dependency
#'
#' Everything is read as character and then cast, rather than letting readr
#' guess. Guessing is what produces a `logical` column from an all-empty one,
#' which is the exact bug the type contract exists to prevent, and it would
#' reintroduce it for any column not in the product's table.
#'
#' Both null sentinels are handled here, the same pair DuckDB is given: the
#' service writes `\\N` for some columns and `""` for others in the same file.
#' @noRd
tb_read_readr <- function(path, product) {
  df <- readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    na = c("\\N", ""),
    progress = FALSE,
    show_col_types = FALSE
  )
  # Rename before anything else, because the type table is keyed on the output
  # names. This is where the readr path meets the SELECT-list rename.
  names(df) <- tb_output_names(names(df), product)
  df <- df[, !names(df) %in% tb_metadata_cols, drop = FALSE]
  coerce_known_cols(df, product$col_types)
}

#' Apply the declared column types
#'
#' Works identically on zero rows, which is the outcome that happens most often:
#' 54% of real user searches return nothing, and an empty result still carries
#' its header row, so casting an empty character column yields a correctly-typed
#' empty column.
#'
#' [dplyr::any_of()] is what lets unknown and missing columns both pass, and it
#' matches the existing house fix for this same bug in `npn_get_data()`. It is
#' also what lets one superset table serve every `taxon` and `phenophase_grain`
#' combination without branching.
#' @noRd
coerce_known_cols <- function(df, types) {
  for (type in unique(unname(types))) {
    cols <- names(types)[types == type]
    df <- dplyr::mutate(
      df,
      dplyr::across(dplyr::any_of(cols), tb_coercers[[type]])
    )
  }
  tibble::as_tibble(df)
}
