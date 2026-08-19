# vcr cannot record this flow: it does not work with httr2 downloads
# (https://github.com/ropensci/vcr/issues/270) and the whole flow is
# `req_perform(path = )`. Offline unit tests carry the risk; the live tests at
# the bottom are gated the same way as the rest of the suite.

skip_long_tests <- as.logical(Sys.getenv(
  "RNPN_SKIP_LONG_TESTS",
  unset = "true"
))

test_that("base URLs are read through options", {
  withr::local_options(rnpn.data_url = "https://example.org")
  expect_equal(base_npn_data_url(), "https://example.org")

  withr::local_options(rnpn.tinybird_url = NULL)
  # the *regional* host is mandatory; api.tinybird.co returns 403
  expect_match(base_tinybird_url(), "us-west-2")
})

test_that("`as` is validated before anything is submitted", {
  expect_error(
    npn_export_status_data(species_ids = 3, as = "tibble"),
    "must be one of"
  )
})

test_that("bad dates fail before anything is submitted", {
  expect_error(
    npn_export_status_data(start_date = "May 1 2024"),
    "must be a Date"
  )
})

test_that("an unknown filter fails before anything is submitted", {
  # `format = "parquet"` is the verified trap: the service returns 202 and
  # delivers CSV, so nothing downstream would ever object
  expect_error(
    npn_export_status_data(format = "parquet"),
    "Unrecognized filter"
  )
  expect_error(
    npn_export_status_data(speceis_ids = 3),
    "Unrecognized filter"
  )
})

test_that("a near-miss of a named filter is matched, not sent as a filter", {
  # R matches arguments before `...` partially, so `species_id` binds to
  # `species_ids` and never reaches `...` validation. That is the helpful
  # outcome --- it is the filter the user meant --- but it is worth pinning,
  # because it means partial matches are resolved by R and never by us.
  expect_error(
    npn_export_status_data(species_id = 3, as = "nonsense"),
    "must be one of"
  )
})

test_that("npn_get_job() rejects a non-id", {
  expect_error(npn_get_job(123), "must be a single job id")
})

test_that("the read engine is only required for the modes that read", {
  # `as = "path"` never touches the file, so it needs nothing
  expect_no_error(tb_check_engine("path"))
})

test_that("a timeout is an error carrying the job id", {
  # returning a job-id string from a call that promised a tibble is the same
  # type instability the `as` contract exists to prevent, moved to the failure
  # path
  err <- expect_error(
    tb_abort_timeout("49e2e7a8-1111-4000-8000-000000000000", 300),
    class = "rnpn_timeout_error"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "300s")
  expect_match(msg, "49e2e7a8-1111-4000-8000-000000000000")
  expect_match(msg, "npn_get_job")
  expect_match(msg, "has not failed")
})

test_that("every request carries a timeout", {
  # curl's default overall timeout is infinite, and the job `timeout` is only
  # checked *between* polls, so a request that stalls would hang the call
  # forever with no error to explain it
  req <- tb_base_req()
  expect_true(is.numeric(req$options$timeout_ms))
  expect_gt(req$options$timeout_ms, 0)
})

test_that("a transient poll failure does not kill a running job", {
  calls <- 0
  local_mocked_bindings(
    tb_job_status = function(job_id, call = NULL) {
      calls <<- calls + 1
      if (calls == 1) {
        stop("Failed to perform HTTP request (timed out)")
      }
      list(status = "complete", download_url = "https://example.org/x.csv.gz")
    }
  )

  url <- tb_await_job("job-1", timeout = 30, first_delay = 0, interval = 0)
  expect_equal(url, "https://example.org/x.csv.gz")
  expect_equal(calls, 2)
})

test_that("polls that never succeed report the transport error, not a job problem", {
  local_mocked_bindings(
    tb_job_status = function(job_id, call = NULL) {
      stop("Could not resolve host: services2-dev.usanpn.org")
    }
  )

  err <- expect_error(
    tb_await_job("49e2e7a8-2222-4000-8000-000000000000",
                 timeout = 1, first_delay = 0, interval = 0),
    class = "rnpn_unreachable_error"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "Could not resolve host")
  # the job id survives, so the work is not lost
  expect_match(msg, "49e2e7a8-2222-4000-8000-000000000000")
  expect_match(msg, "npn_get_job")
  # and it must not claim the server is still working when we never reached it
  expect_false(grepl("still working on it", msg))
})

test_that("the large-query warning names the remedies", {
  msg <- tb_size_warning(19263424)
  expect_match(msg, "19,263,424 records")
  expect_match(msg, "3.2 GB as a tibble")
  expect_match(msg, "6.0 GB while dplyr works on it")
  expect_match(msg, 'as = "path"', fixed = TRUE)
  expect_match(msg, 'as = "lazy"', fixed = TRUE)
})

test_that("the guard warns above the threshold and is silent below", {
  local_mocked_bindings(tb_count_rows = function(...) 19263424)
  expect_warning(
    tb_preflight(tb_products$status, list(species_ids = 3)),
    class = "rnpn_large_query_warning"
  )

  local_mocked_bindings(tb_count_rows = function(...) 2378)
  expect_no_warning(tb_preflight(tb_products$status, list(species_ids = 3)))
})

test_that("the guard fails open", {
  # advisory only: it must never be able to block a download
  local_mocked_bindings(tb_count_rows = function(...) NULL)
  expect_no_warning(tb_preflight(tb_products$status, list(species_ids = 3)))
})

test_that("a durable copy is kept beside the returned value", {
  dir <- withr::local_tempdir()
  src <- file.path(dir, "cached.csv.gz")
  file.copy(test_path("fixtures", "status-sample.csv.gz"), src)
  dest <- file.path(dir, "keep-me.csv.gz")

  # `file_path` is orthogonal to `as`: it says where the artifact is kept, not
  # what comes back
  out <- tb_deliver(src, "query-x", as = "path", file_path = dest)
  expect_equal(out, dest)
  expect_true(file.exists(dest))
  expect_true(file.exists(src))
})

test_that("a bad file_path directory is a clear error", {
  dir <- withr::local_tempdir()
  src <- file.path(dir, "cached.csv.gz")
  file.copy(test_path("fixtures", "status-sample.csv.gz"), src)

  expect_error(
    tb_deliver(src, "query-x", as = "path",
               file_path = file.path(dir, "nope", "x.csv.gz")),
    "does not exist"
  )
})

test_that("`as` alone determines the return type", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  withr::defer(npn_duckdb_reset())
  dir <- withr::local_tempdir()
  withr::local_options(rnpn.cache_dir = dir)

  src <- tb_cache_path("query-shape")
  tb_ensure_dir(dir)
  file.copy(test_path("fixtures", "status-sample.csv.gz"), src)

  expect_s3_class(tb_deliver(src, "query-shape", "data"), "tbl_df")
  expect_type(tb_deliver(src, "query-shape", "path"), "character")
  expect_s3_class(tb_deliver(src, "query-shape", "lazy"), "tbl_lazy")

  # and it does not change when a durable copy is also requested
  keep <- file.path(dir, "keep.csv.gz")
  expect_s3_class(
    tb_deliver(src, "query-shape", "data", file_path = keep),
    "tbl_df"
  )
  expect_true(file.exists(keep))
})

# --- live service ------------------------------------------------------------

test_that("npn_export_status_data() returns a tibble", {
  skip_on_cran()
  skip_if(skip_long_tests)
  skip_if_not_installed("duckdb")
  skip_if_not(check_data_service(), "Service is down")
  withr::defer(npn_duckdb_reset())
  withr::local_options(rnpn.cache_dir = withr::local_tempdir())

  obs <- npn_export_status_data(
    start_date = "2024-05-01",
    end_date = "2024-05-03",
    species_ids = 3
  )

  expect_s3_class(obs, "tbl_df")
  expect_gt(nrow(obs), 0)
  expect_true(all(c("observation_id", "common_name") %in% names(obs)))
  expect_false("request_id" %in% names(obs))
  expect_s3_class(obs$observation_date, "Date")
})

test_that("an invalid filter value returns zero rows with stable types", {
  skip_on_cran()
  skip_if(skip_long_tests)
  skip_if_not_installed("duckdb")
  skip_if_not(check_data_service(), "Service is down")
  withr::defer(npn_duckdb_reset())
  withr::local_options(rnpn.cache_dir = withr::local_tempdir())

  # silent narrowing: a typo in a filter *value* returns a header and no data,
  # indistinguishable from a real empty result
  obs <- npn_export_status_data(
    start_date = "2024-05-01",
    end_date = "2024-05-03",
    species_ids = 3,
    phenophase_categories = "ZZZNOTREAL"
  )
  expect_equal(nrow(obs), 0)
  expect_type(obs$observation_id, "integer")
})

test_that("as = 'path' returns a gzipped CSV without reading it", {
  skip_on_cran()
  skip_if(skip_long_tests)
  skip_if_not(check_data_service(), "Service is down")
  withr::local_options(rnpn.cache_dir = withr::local_tempdir())

  path <- npn_export_status_data(
    start_date = "2024-05-01",
    end_date = "2024-05-03",
    species_ids = 3,
    as = "path"
  )
  expect_type(path, "character")
  expect_true(file.exists(path))
  expect_match(path, "\\.csv\\.gz$")
  expect_true(tb_is_gzip(path))
})

test_that("wait = FALSE returns a job id that npn_get_job() can collect", {
  skip_on_cran()
  skip_if(skip_long_tests)
  skip_if_not_installed("duckdb")
  skip_if_not(check_data_service(), "Service is down")
  withr::defer(npn_duckdb_reset())
  withr::local_options(rnpn.cache_dir = withr::local_tempdir())

  job_id <- npn_export_status_data(
    start_date = "2024-05-01",
    end_date = "2024-05-03",
    species_ids = 3,
    wait = FALSE
  )
  expect_type(job_id, "character")
  expect_length(job_id, 1)

  obs <- npn_get_job(job_id)
  expect_s3_class(obs, "tbl_df")
  expect_gt(nrow(obs), 0)
})

test_that("the count pipe agrees with the export it describes", {
  skip_on_cran()
  skip_if(skip_long_tests)
  skip_if_not_installed("duckdb")
  skip_if_not(check_data_service(), "Service is down")
  withr::defer(npn_duckdb_reset())
  withr::local_options(rnpn.cache_dir = withr::local_tempdir())
  tb_token_reset()

  payload <- tb_build_payload(
    named = list(
      start_date = "2024-05-01",
      end_date = "2024-05-03",
      species_ids = 3
    ),
    download_type = tb_products$status$download_type
  )

  n <- tb_count_rows(tb_products$status, payload)
  skip_if(is.null(n), "count pipe unavailable")

  obs <- npn_export_status_data(
    start_date = "2024-05-01",
    end_date = "2024-05-03",
    species_ids = 3
  )
  expect_equal(nrow(obs), n)
})

test_that("the cache serves a repeated query without re-submitting", {
  skip_on_cran()
  skip_if(skip_long_tests)
  skip_if_not(check_data_service(), "Service is down")
  withr::local_options(rnpn.cache_dir = withr::local_tempdir())

  args <- list(
    start_date = "2024-05-01",
    end_date = "2024-05-03",
    species_ids = 3,
    as = "path"
  )
  first <- do.call(npn_export_status_data, args)
  expect_message(second <- do.call(npn_export_status_data, args), "cached")
  expect_equal(first, second)
})
