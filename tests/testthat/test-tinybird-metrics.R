# Ingest for the three metrics products: the lowercase rename and the
# type-stability contract, pinned against six fixtures.
#
# All six are **real captures** from the dev host on 2026-08-25, trimmed to a
# few rows: a few-row `.csv.gz` and its header-only sibling per product,
# carrying the leading `request_id`/`product` columns, `\N` nulls, and the
# legacy `PascalCase` header the pipes actually emit. (`""` nulls appear on
# status data and are covered by `test-tinybird-ingest.R`.)
#
# The queries, so they can be re-captured:
#
# - individual: 2024, species 3, site 35433, `include_observation_detail = "1"`,
#   `include_submission = "1"`. The flags are the point --- the capture carries
#   the three array columns and pins them as `character`. Declaring `Dataset_ID`
#   as `integer`, which its name and its status-data namesake both suggest,
#   produces silent `NA` for every row.
# - site: 2024, species 3, site 35433. Carries an n=1 summary, where the SE
#   columns are genuinely NULL.
# - magnitude: 2019, species 3 (red maple), frequency 30. A **plant** query, so
#   the 13 core abundance columns, `NumSites_with_Yes_Record` and
#   `Proportion_Sites_with_Yes_Record` are entirely NULL. That is kingdom
#   blanking, it is the majority case, and it is the fixture that proves the
#   declared types are doing work.
#
# Note the windowed products append a `-w0` window suffix to `request_id`. It is
# dropped from the tibble either way.

fixture <- function(name) test_path("fixtures", paste0(name, ".csv.gz"))

products <- list(
  individual = tb_products$individual,
  site = tb_products$site,
  magnitude = tb_products$magnitude
)

# --- the rename, before any engine is involved -------------------------------

test_that("the metrics products lowercase their header and status data does not", {
  expect_equal(
    tb_output_names(c("First_Yes_DOY", "USDA_PLANTS_Symbol"), tb_products$individual),
    c("first_yes_doy", "usda_plants_symbol")
  )
  # hyphens are preserved: legacy kept them, so substituting `_` would be a
  # divergence invented here rather than a fix
  expect_equal(
    tb_output_names("Total_NumAnimals_In-Phase", tb_products$magnitude),
    "total_numanimals_in-phase"
  )
  # status data already arrives lower_snake_case and is left alone
  expect_equal(
    tb_output_names("observation_id", tb_products$status),
    "observation_id"
  )
})

test_that("a fixup overrides the mechanical tolower()", {
  # the vectors ship empty; this pins the mechanism they will be filled into
  patched <- tb_products$magnitude
  patched$name_fixups <- c(proportion_yes_records = "proportion_yes_record")
  expect_equal(
    tb_output_names(c("Proportion_Yes_Records", "Year"), patched),
    c("proportion_yes_record", "year")
  )
})

test_that("the SELECT list renames and casts to the output name", {
  sql <- tb_select_list(
    c("First_Yes_DOY", "Mystery_Column"),
    tb_products$individual,
    cast = TRUE
  )
  expect_match(sql, 'CAST\\("First_Yes_DOY" AS INTEGER\\) AS "first_yes_doy"')
  # unknown columns are renamed but not cast: a strict list enumerating an
  # exact set would have broken on 2026-08-14
  expect_match(sql, '"Mystery_Column" AS "mystery_column"', fixed = TRUE)
  expect_false(grepl("CAST(\"Mystery_Column\"", sql, fixed = TRUE))
})

# --- both engines ------------------------------------------------------------

test_that("both engines return identical names and types, on data and on empty", {
  skip_if_not_installed("duckdb")
  withr::defer(npn_duckdb_reset())

  for (name in names(products)) {
    for (file in c(paste0(name, "-sample"), paste0(name, "-empty"))) {
      duck <- withr::with_options(
        list(rnpn.engine = "duckdb"),
        tb_read_data(fixture(file), products[[name]])
      )
      readr_out <- withr::with_options(
        list(rnpn.engine = "readr"),
        tb_read_data(fixture(file), products[[name]])
      )

      expect_equal(names(readr_out), names(duck), info = file)
      expect_equal(
        vapply(readr_out, function(x) class(x)[1], character(1)),
        vapply(duck, function(x) class(x)[1], character(1)),
        info = file
      )
    }
  }
})

test_that("returned names are lowercase and the bookkeeping columns are gone", {
  for (name in names(products)) {
    out <- withr::with_options(
      list(rnpn.engine = "readr"),
      tb_read_data(fixture(paste0(name, "-sample")), products[[name]])
    )
    expect_equal(names(out), tolower(names(out)), info = name)
    expect_false("request_id" %in% names(out))
    expect_false("product" %in% names(out))
  }
})

test_that("an empty result has the same columns and types as a full one", {
  # 54% of real user searches return zero rows, so this is the modal response
  for (name in names(products)) {
    full <- withr::with_options(
      list(rnpn.engine = "readr"),
      tb_read_data(fixture(paste0(name, "-sample")), products[[name]])
    )
    empty <- withr::with_options(
      list(rnpn.engine = "readr"),
      tb_read_data(fixture(paste0(name, "-empty")), products[[name]])
    )

    expect_equal(nrow(empty), 0, info = name)
    expect_equal(names(empty), names(full), info = name)
    expect_equal(
      vapply(empty, function(x) class(x)[1], character(1)),
      vapply(full, function(x) class(x)[1], character(1)),
      info = name
    )
  }
})

# --- per-product specifics ---------------------------------------------------

test_that("individual phenometrics types its array columns as character", {
  withr::local_options(rnpn.engine = "readr")
  out <- tb_read_data(fixture("individual-sample"), tb_products$individual)

  # an island spans many observations, so these arrive as ClickHouse arrays
  # serialized into the CSV cell. `integer` would be a silent NA per row.
  expect_type(out$dataset_id, "character")
  expect_equal(out$dataset_id[[1]], "[3]")
  expect_type(out$observedby_person_id, "character")
  expect_type(out$lpl_certified_date, "character")

  # the third island spans eight observers, and `lpl_certified_date` is
  # positionally aligned to it --- the shape that makes these text, not numbers
  expect_equal(
    out$observedby_person_id[[3]],
    "[63100,46559,69109,63164,69128,73857,74899,73850]"
  )
  expect_equal(
    out$lpl_certified_date[[3]],
    "[NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL]"
  )

  expect_type(out$first_yes_doy, "integer")
  expect_equal(out$first_yes_doy, c(74L, 81L, 106L))
  expect_type(out$numdays_since_prior_no, "integer")
  expect_type(out$latitude, "double")

  # a `\N` column that is empty for every row still comes back character
  expect_type(out$observed_status_conflict_flag, "character")
  expect_true(all(is.na(out$observed_status_conflict_flag)))
})

test_that("site phenometrics mean_* and se_* columns are doubles", {
  # the single most likely typing mistake here: the names read as integers and
  # the status-data siblings genuinely are
  withr::local_options(rnpn.engine = "readr")
  out <- tb_read_data(fixture("site-sample"), tb_products$site)

  expect_type(out$mean_first_yes_doy, "double")
  expect_type(out$mean_first_yes_year, "double")
  expect_type(out$mean_first_yes_julian_date, "double")
  expect_type(out$se_first_yes_in_days, "double")
  expect_type(out$mean_numdays_since_prior_no, "double")
  expect_equal(out$mean_first_yes_doy, c(106, 73))

  # sample sizes are counts
  expect_type(out$first_yes_sample_size, "integer")
  expect_equal(out$first_yes_sample_size, c(1L, 3L))
  expect_type(out$last_yes_sample_size, "integer")

  # the SE columns are real NULL at n = 1, never -9999: the sample size column
  # already says n = 1 unambiguously
  expect_true(is.na(out$se_first_yes_in_days[[1]]))
  expect_equal(out$se_first_yes_in_days[[2]], 4.333333, tolerance = 1e-6)
})

test_that("magnitude's kingdom-blanked columns keep their declared types", {
  # every plant query blanks 20 of the 32 core columns. This is precisely the
  # all-NULL-column-gets-type-guessed failure the contract exists for.
  withr::local_options(rnpn.engine = "readr")
  out <- tb_read_data(fixture("magnitude-sample"), tb_products$magnitude)

  expect_true(all(is.na(out$numsites_with_yes_record)))
  expect_type(out$numsites_with_yes_record, "integer")
  expect_true(all(is.na(out$proportion_sites_with_yes_record)))
  expect_type(out$proportion_sites_with_yes_record, "double")

  # the hyphenated abundance columns, reached with backticks
  expect_true(all(is.na(out$`total_numanimals_in-phase`)))
  expect_type(out$`total_numanimals_in-phase`, "double")
  expect_type(out$`in-phase_sites_sample_size`, "integer")

  # populated columns, for contrast
  expect_type(out$proportion_yes_records, "double")
  expect_equal(out$proportion_yes_records[[1]], 0.04836415, tolerance = 1e-6)
  expect_type(out$num_yes_records, "integer")
  expect_equal(out$num_yes_records, c(34L, 0L, 3L))
  expect_equal(unique(out$kingdom), "Plantae")

  # the only Date columns any of the three products emits
  expect_s3_class(out$start_date, "Date")
  expect_s3_class(out$end_date, "Date")
  expect_type(out$year, "integer")
})

test_that("the lazy path applies the same rename and types", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dbplyr")
  withr::defer(npn_duckdb_reset())
  dir <- withr::local_tempdir()
  withr::local_options(rnpn.cache_dir = dir)

  csv <- file.path(dir, "query-magnitude.csv.gz")
  file.copy(fixture("magnitude-sample"), csv)

  lazy <- tb_lazy_tbl(csv, "query-magnitude", tb_products$magnitude)
  collected <- dplyr::collect(lazy)

  eager <- tb_read_data(csv, tb_products$magnitude)
  expect_equal(names(collected), names(eager))
  expect_s3_class(collected$start_date, "Date")
  # the cast happened in SQL, so the parquet sibling already carries the type
  expect_type(collected$`total_numanimals_in-phase`, "double")
})

# --- the type tables themselves ----------------------------------------------

test_that("every type table is keyed on lowercase names and declares real types", {
  tables <- list(
    status = status_col_types,
    individual = ipm_col_types,
    site = site_col_types,
    magnitude = magnitude_col_types
  )
  for (name in names(tables)) {
    types <- tables[[name]]
    expect_equal(names(types), tolower(names(types)), info = name)
    expect_true(all(types %in% names(tb_coercers)), info = name)
    expect_true(all(types %in% names(tb_duckdb_types)), info = name)
    expect_false(any(duplicated(names(types))), info = name)
  }
})

test_that("each product's table is the one wired to it", {
  expect_identical(tb_products$individual$col_types, ipm_col_types)
  expect_identical(tb_products$site$col_types, site_col_types)
  expect_identical(tb_products$magnitude$col_types, magnitude_col_types)
})

# --- live service ------------------------------------------------------------

skip_long_tests <- as.logical(Sys.getenv(
  "RNPN_SKIP_LONG_TESTS",
  unset = "true"
))

live_args <- list(
  individual = list(
    fn = npn_export_individual_phenometrics,
    known = "first_yes_doy",
    type = "integer"
  ),
  site = list(
    fn = npn_export_site_phenometrics,
    known = "mean_first_yes_doy",
    type = "double"
  ),
  magnitude = list(
    fn = npn_export_magnitude_phenometrics,
    known = "proportion_yes_records",
    type = "double"
  )
)

for (name in names(live_args)) {
  local({
    spec <- live_args[[name]]
    product <- name

    test_that(paste0("npn_export_", product, "_phenometrics() returns lowercase names"), {
      skip_on_cran()
      skip_if(skip_long_tests)
      skip_if_not_installed("duckdb")
      skip_if_not(check_data_service(), "Service is down")
      withr::defer(npn_duckdb_reset())
      withr::local_options(rnpn.cache_dir = withr::local_tempdir())

      out <- spec$fn(
        start_date = "2019-01-01",
        end_date = "2019-12-31",
        species_ids = 3
      )

      expect_s3_class(out, "tbl_df")
      expect_equal(names(out), tolower(names(out)))
      expect_false("request_id" %in% names(out))
      expect_true(spec$known %in% names(out))
      expect_type(out[[spec$known]], spec$type)
    })

    test_that(paste0("a known-typed ", product, " column survives an empty result"), {
      skip_on_cran()
      skip_if(skip_long_tests)
      skip_if_not_installed("duckdb")
      skip_if_not(check_data_service(), "Service is down")
      withr::defer(npn_duckdb_reset())
      withr::local_options(rnpn.cache_dir = withr::local_tempdir())

      # a filter *value* the service does not recognize returns a header and no
      # rows, which is where an undeclared type would be guessed
      out <- spec$fn(
        start_date = "2019-01-01",
        end_date = "2019-12-31",
        species_ids = 3,
        phenophase_categories = "ZZZNOTREAL"
      )
      expect_equal(nrow(out), 0)
      expect_type(out[[spec$known]], spec$type)
    })
  })
}
