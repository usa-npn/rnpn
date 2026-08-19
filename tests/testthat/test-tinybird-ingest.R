# The type-stability and empty-result contracts, pinned against two committed
# fixtures captured from the live service.
#
# - status-sample.csv.gz: a few rows carrying **both** null sentinels, `\N` and
#   `""`, in the same file.
# - status-empty.csv.gz: the header-only response, byte for byte. Given that 54%
#   of real searches return nothing, this is not an edge case --- it is the
#   modal response.

sample_fixture <- function() test_path("fixtures", "status-sample.csv.gz")
empty_fixture <- function() test_path("fixtures", "status-empty.csv.gz")

test_that("gzip is detected from the bytes, not the name", {
  # Neither header nor filename can be trusted: the same query has been served
  # as text/csv + Content-Encoding: gzip and as application/gzip with none, and
  # three different filenames have been observed.
  expect_true(tb_is_gzip(sample_fixture()))

  plain <- withr::local_tempfile(fileext = ".csv.gz")
  writeLines("observation_id,site_id", plain)
  expect_false(tb_is_gzip(plain))

  empty <- withr::local_tempfile()
  file.create(empty)
  expect_false(tb_is_gzip(empty))
})

test_that("a plain CSV is compressed rather than misnamed", {
  src <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("a,b", "1,2"), src)
  dest <- withr::local_tempfile(fileext = ".csv.gz")

  tb_gzip_file(src, dest)

  expect_true(tb_is_gzip(dest))
  expect_equal(readLines(gzfile(dest)), c("a,b", "1,2"))
})

test_that("coerce_known_cols() declares types independent of the rows", {
  # the same code, the same columns, one populated and one entirely NA
  populated <- data.frame(
    observation_id = "1",
    latitude = "41.4",
    observation_date = "2024-05-01",
    intensity_value = "25-49%",
    abundance_value = "7",
    stringsAsFactors = FALSE
  )
  unpopulated <- data.frame(
    observation_id = NA_character_,
    latitude = NA_character_,
    observation_date = NA_character_,
    intensity_value = NA_character_,
    abundance_value = NA_character_,
    stringsAsFactors = FALSE
  )

  a <- coerce_known_cols(populated)
  b <- coerce_known_cols(unpopulated)

  expect_equal(vapply(a, function(x) class(x)[1], character(1)),
               vapply(b, function(x) class(x)[1], character(1)))
  expect_type(a$observation_id, "integer")
  expect_type(a$latitude, "double")
  expect_s3_class(a$observation_date, "Date")
  expect_type(a$intensity_value, "character")
  expect_type(a$abundance_value, "integer")
})

test_that("coerce_known_cols() leaves unknown columns untouched", {
  # the dev schema gained two columns mid-session on 2026-08-14; a strict list
  # enumerating an exact set would have broken that afternoon
  df <- data.frame(
    observation_id = "1",
    something_new = "hello",
    stringsAsFactors = FALSE
  )
  out <- coerce_known_cols(df)
  expect_type(out$observation_id, "integer")
  expect_type(out$something_new, "character")
})

test_that("coerce_known_cols() works on zero rows", {
  df <- data.frame(
    observation_id = character(0),
    observation_date = character(0),
    stringsAsFactors = FALSE
  )
  out <- coerce_known_cols(df)
  expect_equal(nrow(out), 0)
  expect_type(out$observation_id, "integer")
  expect_s3_class(out$observation_date, "Date")
})

test_that("coerce_known_cols() returns a tibble", {
  expect_s3_class(coerce_known_cols(data.frame(a = 1)), "tbl_df")
})

test_that("the SELECT list casts only columns it knows", {
  cols <- c("observation_id", "mystery")
  cast <- tb_select_list(cols, cast = TRUE)
  expect_match(cast, 'CAST\\("observation_id" AS INTEGER\\)')
  expect_match(cast, '"mystery"')
  expect_false(grepl('CAST\\("mystery"', cast))
})

test_that("the SELECT list can drop the bookkeeping columns", {
  sel <- tb_select_list(
    c("request_id", "product", "observation_id"),
    exclude = tb_metadata_cols
  )
  expect_equal(sel, '"observation_id"')
})

test_that("SQL literals are escaped", {
  expect_equal(tb_sql_string("it's"), "'it''s'")
  expect_equal(tb_sql_ident('a"b'), '"a""b"')
})

# --- these need the read engine ---------------------------------------------

test_that("a real export reads with both null sentinels honored", {
  skip_if_not_installed("duckdb")
  withr::defer(npn_duckdb_reset())

  out <- tb_read_data(sample_fixture())

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4)

  # `""` in state and intensity_value, `\N` in abundance_value: both are NA.
  # Passing nullstr as a bare string instead of a list silently leaves the
  # empty strings intact.
  expect_true(is.na(out$state[[4]]))
  expect_true(all(is.na(out$abundance_value)))
  expect_equal(sum(is.na(out$intensity_value)), 3)
  expect_equal(out$intensity_value[[2]], "25-49%")

  # declared types
  expect_type(out$observation_id, "integer")
  expect_type(out$latitude, "double")
  expect_s3_class(out$observation_date, "Date")
  expect_type(out$intensity_value, "character")
  expect_type(out$abundance_value, "integer")
})

test_that("bookkeeping columns are dropped from the tibble", {
  skip_if_not_installed("duckdb")
  withr::defer(npn_duckdb_reset())

  out <- tb_read_data(sample_fixture())
  # constant for every row: sink bookkeeping, not data
  expect_false("request_id" %in% names(out))
  expect_false("product" %in% names(out))
})

test_that("an empty result has the same columns and types as a full one", {
  skip_if_not_installed("duckdb")
  withr::defer(npn_duckdb_reset())

  full <- tb_read_data(sample_fixture())
  empty <- tb_read_data(empty_fixture())

  expect_equal(nrow(empty), 0)
  expect_equal(names(empty), names(full))
  expect_equal(
    vapply(empty, function(x) class(x)[1], character(1)),
    vapply(full, function(x) class(x)[1], character(1))
  )
})

test_that("the lazy table is queryable and matches the eager read", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  withr::defer(npn_duckdb_reset())
  dir <- withr::local_tempdir()
  withr::local_options(rnpn.cache_dir = dir)

  csv <- file.path(dir, "query-test.csv.gz")
  file.copy(sample_fixture(), csv)

  lazy <- tb_lazy_tbl(csv, "query-test")
  expect_s3_class(lazy, "tbl_lazy")
  # the parquet sibling is persisted so a second lazy query is free
  expect_true(file.exists(tb_cache_path("query-test", ".parquet")))

  collected <- dplyr::collect(lazy)
  expect_equal(nrow(collected), 4)
  expect_equal(names(collected), names(tb_read_data(csv)))
  expect_s3_class(collected$observation_date, "Date")
})

test_that("the readr engine needs nothing installed and gives the same answer", {
  # the point of this engine: it never loads a threaded C++ library, so it works
  # where DuckDB's native lock deadlocks a debugger
  withr::local_options(rnpn.engine = "readr")
  out <- tb_read_data(sample_fixture())

  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(out), 4)
  expect_false("request_id" %in% names(out))
  expect_false("product" %in% names(out))

  # both null sentinels honored, exactly as on the DuckDB path
  expect_true(is.na(out$state[[4]]))
  expect_true(all(is.na(out$abundance_value)))
  expect_equal(sum(is.na(out$intensity_value)), 3)
  expect_equal(out$intensity_value[[2]], "25-49%")

  # and the declared types
  expect_type(out$observation_id, "integer")
  expect_type(out$latitude, "double")
  expect_s3_class(out$observation_date, "Date")
  expect_type(out$abundance_value, "integer")
})

test_that("both engines agree, on data and on an empty result", {
  skip_if_not_installed("duckdb")
  withr::defer(npn_duckdb_reset())

  for (fixture in list(sample_fixture(), empty_fixture())) {
    duck <- withr::with_options(
      list(rnpn.engine = "duckdb"), tb_read_data(fixture)
    )
    readr_out <- withr::with_options(
      list(rnpn.engine = "readr"), tb_read_data(fixture)
    )

    expect_equal(names(readr_out), names(duck))
    expect_equal(
      vapply(readr_out, function(x) class(x)[1], character(1)),
      vapply(duck, function(x) class(x)[1], character(1))
    )
    expect_equal(readr_out, duck)
  }
})

test_that("the engine option is validated", {
  withr::local_options(rnpn.engine = "arrow")
  expect_error(tb_read_data(sample_fixture()), 'must be either "duckdb" or "readr"')
})

test_that("readr engine skips the duckdb check but lazy still demands it", {
  withr::local_options(rnpn.engine = "readr")
  # no duckdb needed to hand back a tibble
  expect_no_error(tb_check_engine("data"))
  expect_no_error(tb_check_engine("path"))
  # but a lazy table is a database connection, and readr has none to offer
  expect_error(tb_check_engine("lazy"), "needs DuckDB")
})

test_that("a caller-supplied connection is used instead of the session one", {
  skip_if_not_installed("duckdb")
  withr::defer(npn_duckdb_reset())

  mine <- DBI::dbConnect(duckdb::duckdb())
  withr::defer(DBI::dbDisconnect(mine, shutdown = TRUE))
  withr::local_options(rnpn.duckdb_con = mine)

  expect_identical(npn_duckdb_con(), mine)
  # and rnpn did not open one of its own
  expect_null(pkg.env$duckdb_con)

  # it still reads correctly through the supplied connection
  out <- tb_read_data(sample_fixture())
  expect_equal(nrow(out), 4)
})

test_that("a bogus supplied connection is rejected clearly", {
  withr::local_options(rnpn.duckdb_con = "not a connection")
  expect_error(npn_duckdb_con(), "not a valid DBI connection")
})

test_that("the duckdb connection is one per session and resettable", {
  skip_if_not_installed("duckdb")
  withr::defer(npn_duckdb_reset())

  a <- npn_duckdb_con()
  b <- npn_duckdb_con()
  # shared, so two lazy tables can be joined
  expect_identical(a, b)

  npn_duckdb_reset()
  expect_null(pkg.env$duckdb_con)
  expect_false(DBI::dbIsValid(a))
})
