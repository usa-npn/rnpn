# The DuckDB engine: connection, SQL, parquet, and the lazy table.
#
# DuckDB is a `Suggests`. It is chosen over arrow because every measurement
# behind this design is a DuckDB measurement; picking arrow would mean re-running
# the whole measurement suite to find out whether the design still holds.
#
# Nothing in this file is loaded unless the DuckDB engine is actually used.
# `R/tinybird-read.R` decides that, and offers a readr path that never touches
# any of it.

#' The session's DuckDB connection
#'
#' **One connection per session**, created lazily and held in `pkg.env`. Chosen
#' over a connection per call because it lets two lazy tables from two calls
#' participate in the same [dplyr::left_join()] --- joining status data against
#' a site or species lookup is an obvious thing for this audience to want, and
#' per-call connections make it fail.
#'
#' `options(rnpn.duckdb_con = )` supplies your own connection instead. Two uses:
#' joining exported data against tables you already have in DuckDB, and
#' stepping through this package in a debugger --- `DBI::dbConnect()` holds a
#' native lock across several R statements, so a debugger that pauses inside it
#' and then inspects the driver object deadlocks against a mutex only the
#' paused code can release. Creating the connection outside the stepped frame
#' avoids that entirely, as does calling `npn_duckdb_con()` once beforehand,
#' since this is a singleton.
#' @noRd
npn_duckdb_con <- function() {
  supplied <- getOption("rnpn.duckdb_con")
  if (!is.null(supplied)) {
    if (!inherits(supplied, "DBIConnection") || !DBI::dbIsValid(supplied)) {
      rlang::abort(c(
        "`rnpn.duckdb_con` is not a valid DBI connection.",
        "i" = "Set it to a live connection, or to NULL to let rnpn manage its own."
      ))
    }
    return(supplied)
  }

  con <- pkg.env$duckdb_con
  if (!is.null(con) && DBI::dbIsValid(con)) {
    return(con)
  }
  con <- DBI::dbConnect(duckdb::duckdb())
  pkg.env$duckdb_con <- con
  con
}

#' Disconnect and forget the session connection
#'
#' Required rather than optional: `R CMD check` notices dangling connections,
#' and tests must not share state.
#' @noRd
npn_duckdb_reset <- function() {
  con <- pkg.env$duckdb_con
  if (!is.null(con)) {
    try(DBI::dbDisconnect(con, shutdown = TRUE), silent = TRUE)
  }
  pkg.env$duckdb_con <- NULL
  invisible(NULL)
}

#' SQL quoting helpers
#' @noRd
tb_sql_string <- function(x) {
  paste0("'", gsub("'", "''", x, fixed = TRUE), "'")
}

#' @rdname tb_sql_string
#' @noRd
tb_sql_ident <- function(x) {
  paste0('"', gsub('"', '""', x, fixed = TRUE), '"')
}

#' @rdname tb_sql_string
#' @noRd
tb_sql_path <- function(path) {
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

#' A DuckDB table expression for a cached `.csv.gz`
#'
#' `nullstr` must be given as a **list**. `nullstr = '\\N'` alone silently
#' leaves `intensity_value`'s empty strings intact --- 2,378 non-null values
#' against 535 with the list. Both sentinels are in play: the service writes
#' `\\N` for some columns and `""` for others, in the same file.
#'
#' Compression is inferred from the `.csv.gz` extension, which is why cache
#' entries are named honestly.
#' @noRd
tb_csv_source <- function(path) {
  paste0(
    "read_csv(",
    tb_sql_string(tb_sql_path(path)),
    ", nullstr = ['\\N', ''], header = true)"
  )
}

#' Column names of a table expression, in file order
#'
#' Read from the file rather than assumed: the schema changed mid-session on
#' 2026-08-14, when the identical payload returned 20 columns in the morning and
#' 22 in the afternoon. Nothing here may reference a column by position.
#' @noRd
tb_describe_cols <- function(con, source) {
  described <- DBI::dbGetQuery(con, paste0("DESCRIBE SELECT * FROM ", source))
  described$column_name
}

#' Build a SELECT list from the columns actually present
#'
#' Unknown columns pass through untouched, which is the point: a strict list
#' enumerating an exact set would have broken on 2026-08-14.
#'
#' The rename lives here rather than in R, so that three things fall out for
#' free: the lazy path gets it too, which an R-side `names()<-` cannot reach;
#' the persisted parquet sibling is written with the output names, so it needs
#' no second pass; and the type lookup and the rename happen in one place,
#' against one list of file columns.
#'
#' @param product The product entry. Supplies the type table and the naming
#'   convention.
#' @param cast Whether to apply the declared types in SQL. The eager path casts
#'   in R through `coerce_known_cols()`; the lazy path has to cast here, because
#'   R cannot reach into the table. Both read the same table.
#' @noRd
tb_select_list <- function(
  cols,
  product,
  cast = FALSE,
  exclude = character()
) {
  cols <- setdiff(cols, exclude)
  if (length(cols) == 0) {
    return("*")
  }
  types <- product$col_types
  out <- tb_output_names(cols, product)

  parts <- vapply(
    seq_along(cols),
    function(i) {
      src <- tb_sql_ident(cols[[i]])
      dest <- tb_sql_ident(out[[i]])
      known <- out[[i]] %in% names(types)
      if (cast && known) {
        paste0(
          "CAST(", src, " AS ", tb_duckdb_types[[types[[out[[i]]]]]],
          ") AS ", dest
        )
      } else if (!identical(cols[[i]], out[[i]])) {
        paste0(src, " AS ", dest)
      } else {
        src
      }
    },
    character(1)
  )
  paste(parts, collapse = ", ")
}

#' Read a cached export into a tibble, with DuckDB
#'
#' `request_id` and `product` are excluded in SQL rather than dropped
#' afterwards, so their pointers are never allocated at all.
#' @noRd
tb_read_duckdb <- function(path, product) {
  con <- npn_duckdb_con()
  source <- tb_csv_source(path)
  cols <- tb_describe_cols(con, source)
  sql <- paste0(
    "SELECT ",
    tb_select_list(cols, product, cast = FALSE, exclude = tb_metadata_cols),
    " FROM ",
    source
  )
  coerce_known_cols(DBI::dbGetQuery(con, sql), product$col_types)
}

#' Convert a cached export to parquet, once
#'
#' Paid only for `as = "lazy"`. Against `as = "data"` a cache hit costs a full
#' parse *and* a materialization either way --- 6.6s to `collect()` against
#' 1.74s to scan the `.gz` --- so parquet shaves the smaller half. Its real
#' advantage lands here, where every dplyr verb re-scans: `group_by(state,
#' common_name)` in 0.17s on parquet against a full decompress-and-parse per
#' verb on gzipped CSV.
#'
#' Persisted as a sibling cache entry so re-running the same lazy query in a
#' script does not re-pay the ~3.1s conversion.
#' @noRd
tb_ensure_parquet <- function(csv_path, parquet_path, product) {
  if (
    file.exists(parquet_path) &&
      file.mtime(parquet_path) >= file.mtime(csv_path)
  ) {
    return(parquet_path)
  }
  con <- npn_duckdb_con()
  source <- tb_csv_source(csv_path)
  cols <- tb_describe_cols(con, source)
  sql <- paste0(
    "COPY (SELECT ",
    tb_select_list(cols, product, cast = TRUE, exclude = tb_metadata_cols),
    " FROM ",
    source,
    ") TO ",
    tb_sql_string(tb_sql_path(parquet_path)),
    " (FORMAT PARQUET, COMPRESSION ZSTD)"
  )
  DBI::dbExecute(con, sql)
  parquet_path
}

#' A lazy dplyr table over a cached export
#'
#' One view per cache key, so two lazy tables from two different calls can be
#' joined --- which is the whole reason the connection is shared.
#' @noRd
tb_lazy_tbl <- function(csv_path, key, product) {
  parquet_path <- tb_ensure_parquet(
    csv_path,
    tb_cache_path(key, ".parquet"),
    product
  )
  con <- npn_duckdb_con()
  view <- tb_view_name(key)
  DBI::dbExecute(
    con,
    paste0(
      "CREATE OR REPLACE VIEW ",
      tb_sql_ident(view),
      " AS SELECT * FROM read_parquet(",
      tb_sql_string(tb_sql_path(parquet_path)),
      ")"
    )
  )
  dplyr::tbl(con, view)
}

#' @noRd
tb_view_name <- function(key) {
  paste0("npn_", gsub("[^A-Za-z0-9]", "_", key))
}
