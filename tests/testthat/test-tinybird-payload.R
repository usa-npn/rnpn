# Offline tests for request building. vcr cannot record this flow (it does not
# work with httr2 downloads, https://github.com/ropensci/vcr/issues/270), so
# these unit tests carry the risk.

test_that("the mapping table translates R names to payload fields", {
  payload <- tb_build_payload(
    named = list(
      start_date = "2024-05-01",
      end_date = "2024-05-03",
      species_ids = c(3, 6),
      site_ids = 4881,
      states = "AZ",
      phenophase_categories = "Leaves"
    ),
    download_type = "Status and Intensity"
  )

  expect_equal(payload$startDate, "2024-05-01")
  expect_equal(payload$endDate, "2024-05-03")
  expect_equal(payload$species_ids, c(3, 6))
  # three competing spellings; the payload wants `stations`
  expect_equal(payload$stations, 4881)
  # `state`, an array despite the singular name
  expect_equal(payload$state, "AZ")
  expect_equal(payload$downloadType, "Status and Intensity")
  expect_null(payload$site_ids)
  expect_null(payload$states)
})

test_that("phenophase_categories is sent snake_case only", {
  payload <- tb_build_payload(
    named = list(phenophase_categories = "Leaves"),
    download_type = "Status and Intensity"
  )
  # snake_case is the only spelling that works on both the export endpoint and
  # the count pipe
  expect_equal(payload$phenophase_categories, "Leaves")
  expect_null(payload$phenophaseCategories)
})

test_that("empty and NULL filters are dropped", {
  payload <- tb_build_payload(
    named = list(
      start_date = NULL,
      species_ids = integer(0),
      states = "AZ"
    ),
    download_type = "Status and Intensity"
  )
  expect_named(payload, c("state", "downloadType"), ignore.order = TRUE)
})

test_that("`...` accepts documented payload fields", {
  payload <- tb_build_payload(
    named = list(species_ids = 3),
    dots = list(pheno_class_ids = c(1, 2), bottom_left_x1 = -110),
    download_type = "Status and Intensity"
  )
  expect_equal(payload$pheno_class_ids, c(1, 2))
  expect_equal(payload$bottom_left_x1, -110)
})

test_that("`...` rejects unrecognized fields", {
  # This is the guard that matters most: `additionalProperties: false` is in the
  # spec but not enforced, so an unchecked typo widens the query silently.
  expect_error(
    tb_build_payload(
      dots = list(specie_ids = 3),
      download_type = "Status and Intensity"
    ),
    "Unrecognized filter"
  )
  expect_error(
    tb_build_payload(
      dots = list(format = "parquet"),
      download_type = "Status and Intensity"
    ),
    "Unrecognized filter"
  )
})

test_that("a near-miss field name gets a suggestion", {
  expect_error(
    tb_build_payload(
      dots = list(network_id = 1),
      download_type = "Status and Intensity"
    ),
    "network_ids"
  )
})

test_that("`...` rejects unnamed arguments", {
  expect_error(
    tb_build_payload(
      dots = list(3),
      download_type = "Status and Intensity"
    ),
    "must be named"
  )
})

test_that("`...` cannot set the product or duplicate a named argument", {
  expect_error(
    tb_build_payload(
      dots = list(downloadType = "something else"),
      download_type = "Status and Intensity"
    ),
    "identifies the data product"
  )
  expect_error(
    tb_build_payload(
      named = list(site_ids = 1),
      dots = list(stations = 2),
      download_type = "Status and Intensity"
    ),
    "supplied twice"
  )
})

test_that("dates accept Date objects and YYYY-MM-DD strings", {
  expect_equal(tb_parse_date(as.Date("2024-05-01")), "2024-05-01")
  expect_equal(tb_parse_date("2024-05-01"), "2024-05-01")
  expect_null(tb_parse_date(NULL))
})

test_that("dates reject anything else", {
  expect_error(tb_parse_date("05-01-2024"), "must be a Date")
  expect_error(tb_parse_date("2024-13-01"), "must be a Date")
  expect_error(tb_parse_date(2024), "must be a Date")
  expect_error(tb_parse_date(c("2024-05-01", "2024-05-02")), "single date")
})

test_that("hashing normalizes element order", {
  # without element sorting, species_ids = c(3, 5) and c(5, 3) are the same
  # query but miss each other in the cache
  a <- tb_build_payload(
    named = list(species_ids = c(3, 5)),
    download_type = "Status and Intensity"
  )
  b <- tb_build_payload(
    named = list(species_ids = c(5, 3)),
    download_type = "Status and Intensity"
  )
  expect_equal(tb_payload_hash(a), tb_payload_hash(b))
})

test_that("hashing normalizes key order and value type", {
  a <- list(startDate = "2024-05-01", species_ids = 3)
  b <- list(species_ids = "3", startDate = "2024-05-01")
  expect_equal(tb_payload_hash(a), tb_payload_hash(b))
})

test_that("different queries hash differently", {
  a <- list(species_ids = 3)
  b <- list(species_ids = 4)
  expect_false(identical(tb_payload_hash(a), tb_payload_hash(b)))
})

test_that("array fields survive JSON encoding as arrays", {
  payload <- tb_json_payload(list(species_ids = 3, startDate = "2024-05-01"))
  json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE))
  expect_match(json, '"species_ids":\\[3\\]')
  expect_match(json, '"startDate":"2024-05-01"')
})

test_that("count params use the count pipe's spellings", {
  payload <- tb_build_payload(
    named = list(
      start_date = "2024-05-01",
      states = c("AZ", "NM"),
      site_ids = 4881
    ),
    dots = list(bottom_left_x1 = -110, bottom_left_y1 = 31),
    download_type = "Status and Intensity"
  )
  params <- tb_count_params(payload)

  expect_equal(params$start_date, "2024-05-01")
  # plural here; `state=` singular is not a parameter at all
  expect_equal(params$states, "AZ,NM")
  # sent even though the pipe ignores it today: harmless now, free later
  expect_equal(params$site_ids, "4881")
  expect_equal(params$bottom_left_lng, "-110")
  expect_equal(params$bottom_left_lat, "31")
  # the pipe has no notion of the product
  expect_null(params$downloadType)
})

test_that("the property list is the one the spec declares", {
  expect_length(tb_export_properties, 42)
  expect_true(all(tb_payload_names %in% names(tb_export_properties)))
  expect_true(all(names(tb_count_names) %in% names(tb_export_properties)))
})
