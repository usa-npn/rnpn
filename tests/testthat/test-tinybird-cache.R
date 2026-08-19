test_that("the cache defaults to the session temp directory", {
  withr::local_options(rnpn.cache_dir = NULL)
  expect_equal(npn_cache_dir(), file.path(tempdir(), "rnpn-cache"))
})

test_that("the cache directory is redirectable", {
  dir <- withr::local_tempdir()
  withr::local_options(rnpn.cache_dir = dir)
  expect_equal(npn_cache_dir(), dir)
})

test_that("cache entries are named honestly", {
  dir <- withr::local_tempdir()
  withr::local_options(rnpn.cache_dir = dir)
  # duckdb infers compression from the extension; a gzip stream named .csv
  # fails with a confusing CSV-dialect error rather than a compression error
  expect_match(tb_cache_path("query-abc"), "\\.csv\\.gz$")
  # the parquet sibling is the same key with a different extension
  expect_match(tb_cache_path("query-abc", ".parquet"), "\\.parquet$")
})

test_that("the two key namespaces do not collide", {
  key_q <- tb_query_key(list(species_ids = 3))
  key_j <- tb_job_key("49e2e7a8-0000-4000-8000-000000000000")
  expect_match(key_q, "^query-")
  expect_match(key_j, "^job-")
  expect_false(identical(key_q, key_j))
})

test_that("`as` is not part of the cache key", {
  # the .csv.gz is identical regardless of how it is returned
  payload <- list(species_ids = 3)
  expect_equal(tb_query_key(payload), tb_query_key(payload))
})

test_that("cache freshness follows file mtime", {
  dir <- withr::local_tempdir()
  withr::local_options(rnpn.cache_dir = dir)
  path <- tb_cache_path("query-fresh")

  expect_false(tb_cache_fresh(path))

  writeLines("x", path)
  expect_true(tb_cache_fresh(path))

  # a day old
  Sys.setFileTime(path, Sys.time() - as.difftime(25, units = "hours"))
  expect_false(tb_cache_fresh(path))
  # ...unless the TTL says otherwise
  expect_true(tb_cache_fresh(path, ttl_days = 2))
})

test_that("npn_cache_clear() empties the cache", {
  dir <- withr::local_tempdir()
  withr::local_options(rnpn.cache_dir = dir)
  writeLines("x", tb_cache_path("query-a"))
  writeLines("x", tb_cache_path("query-b"))

  expect_message(n <- npn_cache_clear(), "Removed 2")
  expect_equal(n, 2L)
  expect_length(list.files(dir), 0)
})

test_that("npn_cache_clear() copes with a cache that was never created", {
  withr::local_options(
    rnpn.cache_dir = file.path(withr::local_tempdir(), "never-made")
  )
  expect_equal(npn_cache_clear(), 0L)
})

test_that("tb_obtain() reuses a fresh entry and re-downloads on refresh", {
  dir <- withr::local_tempdir()
  withr::local_options(rnpn.cache_dir = dir)
  calls <- 0
  fetch <- function(dest) {
    calls <<- calls + 1
    writeLines("data", dest)
  }

  path <- tb_obtain("query-x", refresh = FALSE, fetch = fetch)
  expect_equal(calls, 1)
  expect_true(file.exists(path))

  expect_message(tb_obtain("query-x", refresh = FALSE, fetch = fetch), "cached")
  expect_equal(calls, 1)

  tb_obtain("query-x", refresh = TRUE, fetch = fetch)
  expect_equal(calls, 2)
})
