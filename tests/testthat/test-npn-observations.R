# vcr doesn't work with httr2 downloads yet, so many of these tests are not
# mocked and are skipped
# https://github.com/ropensci/vcr/issues/270

skip_long_tests <- as.logical(Sys.getenv(
  "RNPN_SKIP_LONG_TESTS",
  unset = "true"
))

test_that("no request source blocked", {
  skip_on_cran()

  expect_error(npn_download_status_data(request_source = NULL, years = 2013))
  expect_error(npn_download_individual_phenometrics(
    request_source = NULL,
    years = 2013
  ))
  expect_error(npn_download_site_phenometrics(
    request_source = NULL,
    years = 2013
  ))
  expect_error(npn_download_magnitude_phenometrics(
    request_source = NULL,
    years = 2013
  ))
})

test_that("npn_download_status_data() works", {
  # Can't use local_casette here because `vcr` doesn't work with downloads yet
  # (https://github.com/ropensci/vcr/issues/270) so it would error when
  # download_path is set
  vcr::use_cassette("npn_download_status_data", {
    some_data <- npn_download_status_data(
      request_source = "Unit Test",
      years = 2013,
      species_ids = c(6)
    )
  })
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_type(some_data$species_id, "integer")

  skip_on_cran()
  skip_if_not(check_service(), "Service is down")

  ## Would be ideal to capture this with vcr, but doesn't work with httr2 downloads yet: https://github.com/ropensci/vcr/issues/270
  some_data_file <- npn_download_status_data(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6),
    download_path = withr::local_tempfile(fileext = ".csv")
  )

  expect_equal(some_data[1, ]$species_id, 6)
  expect_true(file.exists(some_data_file))
  expect_equal(
    read.csv(some_data_file) %>%
      tibble::as_tibble() %>%
      dplyr::mutate(
        update_datetime = as.POSIXct(update_datetime, tz = "UTC"),
        abundance_value = as.character(abundance_value)
      ),
    some_data
  )
})

test_that("phenometrics downloads work", {
  vcr::local_cassette("npn_download_x_phenometrics")

  some_data <- npn_download_individual_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6)
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$species_id, 6)

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6)
  )
  num_site_default <- nrow(some_data)
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$species_id, 6)

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6),
    num_days_quality_filter = "5"
  )
  num_site_custom <- nrow(some_data)
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 1)
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$species_id, 6)
  expect_gt(num_site_default, num_site_custom)

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6)
  )
  num_mag_default <- nrow(some_data)
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$species_id, 6)

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6),
    period_frequency = "14"
  )
  num_mag_custom <- nrow(some_data)
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 25)
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$species_id, 6)

  expect_gt(num_mag_custom, num_mag_default)

  expect_message(
    dl <- npn_download_individual_phenometrics(
      request_source = 'erinz',
      years = 2009,
      species_ids = 1612
    ),
    "No records in 2009"
  )

  expect_equal(dl, dplyr::tibble())
})

test_that("custom period works", {
  vcr::local_cassette("custom_period")
  expect_error(
    npn_download_individual_phenometrics(
      request_source = "unit test",
      years = 2016,
      period_start = "20-20",
      species_id = 201
    ),
    "Please provide `period_start` as 'MM-DD'."
  )

  indiv_standard <-
    npn_download_individual_phenometrics(
      request_source = "unit test",
      years = 2016,
      species_id = 201
    )
  indiv_wateryr <-
    npn_download_individual_phenometrics(
      request_source = "unit test",
      years = 2016,
      period_start = "10-01",
      period_end = "09-30",
      species_id = 201
    )
  site_standard <-
    npn_download_site_phenometrics(
      request_source = "unit test",
      years = 2010,
      species_id = 210
    )
  site_wateryr <-
    npn_download_site_phenometrics(
      request_source = "unit test",
      years = 2010,
      period_start = "10-01",
      period_end = "09-30",
      species_id = 210
    )

  #just crude checks that they aren't identical
  expect_false(
    all(
      indiv_standard$last_yes_month[1:20] == indiv_wateryr$last_yes_month[1:20]
    )
  )
  expect_false(
    all(
      site_standard$mean_last_yes_doy[1:20] ==
        site_wateryr$mean_last_yes_doy[1:20]
    )
  )
})


test_that("file download works", {
  skip_on_cran()
  skip_if(skip_long_tests, "Skipping long tests")
  skip_if_not(check_service(), "Service is down")
  # these can't be mocked currently due to limitations of vcr
  # vcr::local_cassette("downloads")
  test_download_path <- withr::local_tempfile(fileext = ".csv")

  status_dl_file <- npn_download_status_data(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6),
    download_path = test_download_path
  )

  expect_true(file.exists(status_dl_file))
  some_data <- read.csv(status_dl_file)

  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$species_id, 6)

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6),
    download_path = test_download_path
  )
  expect_equal(file.exists(test_download_path), TRUE)
  some_data <- read.csv(test_download_path)

  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$species_id, 6)
})

test_that("climate data flag works", {
  skip_on_cran()
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_download_x_climate")

  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6),
    climate_data = TRUE
  )
  expect_s3_class(some_data, "data.frame")
  expect_type(some_data$tmin_winter, "double")

  some_data <- npn_download_individual_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6),
    climate_data = TRUE
  )
  expect_s3_class(some_data, "data.frame")
  expect_type(some_data$tmin_winter, "double")

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    species_ids = c(6),
    climate_data = TRUE
  )
  expect_s3_class(some_data, "data.frame")
  expect_false("tmin_winter" %in% colnames(some_data))
})

test_that("higher taxonomic ordering works for status data", {
  skip_on_cran()
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_download_status_data_taxa")

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(109),
    additional_fields = c("Family_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 100)
  expect_type(some_data$family_id, "integer")
  expect_equal(some_data[1, ]$family_id, 109)

  less_data <- subset(some_data, species_id == 229)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)

  #Order_ID
  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    years = 2013,
    order_ids = c(94),
    additional_fields = c("Order_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 500)
  expect_type(some_data$order_id, "integer")
  expect_equal(some_data[1, ]$order_id, 94)

  less_data <- subset(some_data, species_id == 1211)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)

  #class_ID
  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    years = 2013,
    class_ids = c(11),
    additional_fields = c("Class_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 100)
  expect_type(some_data$class_id, "integer")
  expect_equal(some_data[1, ]$class_id, 11)

  less_data <- subset(some_data, species_id == 390)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)
})


test_that("higher taxonomic ordering works for individual phenometrics", {
  skip_on_cran()
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_download_individual_phenometrics_taxa")

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  some_data <- npn_download_individual_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    additional_fields = c("Family_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 100)
  expect_type(some_data$family_id, "integer")
  expect_equal(some_data[1, ]$family_id, 322)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)

  #Order_ID
  some_data <- npn_download_individual_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    order_ids = c(95),
    additional_fields = c("Order_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 100)
  expect_type(some_data$order_id, "integer")
  expect_equal(some_data[1, ]$order_id, 95)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)

  # #class_ID
  some_data <- npn_download_individual_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    class_ids = c(15),
    additional_fields = c("Class_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_type(some_data$class_id, "integer")
  expect_equal(some_data[1, ]$class_id, 15)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)
})


test_that("higher taxonomic ordering works for site phenometrics", {
  skip_on_cran()
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_download_site_phenometrics_taxa")

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    additional_fields = c("Family_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$family_id, "integer")
  expect_equal(some_data[1, ]$family_id, 322)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)

  #Order_ID
  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    order_ids = c(95),
    additional_fields = c("Order_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$order_id, "integer")
  expect_equal(some_data[1, ]$order_id, 95)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)

  # #class_ID
  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    class_ids = c(15),
    additional_fields = c("Class_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_type(some_data$class_id, "integer")
  expect_equal(some_data[1, ]$class_id, 15)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)
})


test_that("higher taxonomic ordering works for magnitude phenometrics", {
  skip_on_cran()
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_download_magnitude_phenometrics_taxa")

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    additional_fields = c("Family_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$family_id, "integer")
  expect_equal(some_data[1, ]$family_id, 322)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)

  #Order_ID
  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    order_ids = c(95),
    additional_fields = c("Order_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 100)
  expect_type(some_data$order_id, "integer")
  expect_equal(some_data[1, ]$order_id, 95)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)

  # #class_ID
  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    class_ids = c(15),
    additional_fields = c("Class_ID")
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_type(some_data$class_id, "integer")
  expect_equal(some_data[1, ]$class_id, 15)

  less_data <- subset(some_data, species_id == 6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data), 0)
})


test_that("higher level taxonomic agg and pheno agg works for site level", {
  skip_on_cran()
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_download_site_phenometrics_agg")

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    taxonomy_aggregate = TRUE
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$family_id, "integer")
  expect_equal(some_data[1, ]$family_id, 322)
  expect_false("species_id" %in% colnames(some_data))

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    pheno_class_aggregate = TRUE,
    pheno_class_ids = c(1, 3, 6)
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_false("phenophase_id" %in% colnames(some_data))

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    order_ids = c(78),
    pheno_class_ids = c(1, 3, 6),
    pheno_class_aggregate = TRUE,
    taxonomy_aggregate = TRUE
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 100)
  expect_type(some_data$pheno_class_name, "character")
  expect_type(some_data$order_id, "integer")
  expect_false("phenophase_id" %in% colnames(some_data))
})

test_that("higher level taxonomic agg works for magnitude", {
  skip_on_cran()
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_download_magnitude_phenometrics_agg")

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    taxonomy_aggregate = TRUE
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$family_id, "integer")
  expect_equal(some_data[1, ]$family_id, 322)
  expect_false("species_id" %in% colnames(some_data))

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    pheno_class_aggregate = TRUE,
    pheno_class_ids = c(1, 3, 6)
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_false("phenophase_id" %in% colnames(some_data))

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    order_ids = c(78),
    pheno_class_ids = c(1, 3, 6),
    pheno_class_aggregate = TRUE,
    taxonomy_aggregate = TRUE
  )
  expect_s3_class(some_data, "data.frame")
  expect_gt(nrow(some_data), 10)
  expect_type(some_data$pheno_class_name, "character")
  expect_type(some_data$order_id, "integer")
  expect_false("phenophase_id" %in% colnames(some_data))
})

test_that("six concordance works for status", {
  skip_on_cran()
  skip_if_not(check_service(), "Service is down")
  skip_if(skip_long_tests)

  #doesn't use vcr just because I suspect the fixture file will be very large
  # vcr::local_casette("npn_download_status_data_six_concord_1")
  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(6),
    six_leaf_layer = TRUE,
    six_bloom_layer = TRUE,
    agdd_layer = 32,
    additional_layers = data.frame(
      name = c("si-x:30yr_avg_4k_leaf"),
      param = c("365")
    )
  )

  expect_s3_class(some_data, "data.frame")
  expect_type(some_data$`SI-x_Bloom_Value`, "double")
  expect_type(some_data$`SI-x_Leaf_Value`, "double")
  expect_type(some_data$`si-x:30yr_avg_4k_leaf`, "double")
  expect_type(some_data$`gdd:agdd`, "double")
  expect_gt(some_data[1, "SI-x_Bloom_Value"], -1)
  expect_lt(some_data[1, "SI-x_Bloom_Value"], 250)

  expect_gt(some_data[1, "SI-x_Leaf_Value"], -1)
  expect_lt(some_data[1, "SI-x_Leaf_Value"], 250)

  expect_gt(some_data[1, "si-x:30yr_avg_4k_leaf"], -1)
  expect_lt(some_data[1, "si-x:30yr_avg_4k_leaf"], 250)

  expect_gt(some_data[1, "gdd:agdd"], -1)

  avg_leaf_data <- some_data$`SI-x_Leaf_Value`

  # Test sub-model functionality
  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(6),
    six_leaf_layer = TRUE,
    six_sub_model = "lilac"
  )

  expect_gt(some_data[1, "SI-x_Leaf_Value"], -1)
  expect_lt(some_data[1, "SI-x_Leaf_Value"], 250)
  expect_false(identical(some_data$`SI-x_Leaf_Value`, avg_leaf_data))

  # This is testing that the implicit
  # reconciliation with different SI-x
  # layers is happening based on the date
  #
  # In this case get NCEP data
  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    c(2019),
    species_ids = c(3),
    six_leaf_layer = TRUE,
    six_sub_model = "lilac"
  )
  expect_gt(some_data[1, "SI-x_Leaf_Value"], -1)
  expect_lt(some_data[1, "SI-x_Leaf_Value"], 250)

  # In this case get PRISM data
  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    c(2009),
    species_ids = c(3),
    six_leaf_layer = TRUE,
    six_sub_model = "lilac"
  )
  expect_gt(some_data[1, "SI-x_Leaf_Value"], -1)
  expect_lt(some_data[1, "SI-x_Leaf_Value"], 250)
})


test_that("wkt filter works", {
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_download_x_wkt")
  #wkt is for CO
  wkt_def <- "POLYGON ((-102.04224 36.993083,-109.045223 36.999084,-109.050076 41.000659,-102.051614 41.002377,-102.04224 36.993083))"

  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(35)
  )

  rows_wo_filter <- nrow(some_data)

  some_data <- npn_download_status_data(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(35),
    wkt = wkt_def
  )

  rows_w_filter <- nrow(some_data)

  expect_s3_class(some_data, "data.frame")
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$state, "CO")
  expect_gt(rows_wo_filter, rows_w_filter)

  some_data <- npn_download_individual_phenometrics(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(35)
  )
  rows_wo_filter <- nrow(some_data)

  some_data <- npn_download_individual_phenometrics(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(35),
    wkt = wkt_def
  )
  rows_w_filter <- nrow(some_data)

  expect_s3_class(some_data, "data.frame")
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$state, "CO")
  expect_gt(rows_wo_filter, rows_w_filter)

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(35)
  )
  rows_wo_filter <- nrow(some_data)

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(35),
    wkt = wkt_def
  )
  rows_w_filter <- nrow(some_data)
  expect_s3_class(some_data, "data.frame")
  expect_type(some_data$species_id, "integer")
  expect_equal(some_data[1, ]$state, "CO")
  expect_gt(rows_wo_filter, rows_w_filter)

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(35)
  )

  rows_wo_filter <- nrow(some_data)

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    c(2016),
    species_ids = c(35),
    wkt = wkt_def
  )
  rows_w_filter <- nrow(some_data)

  expect_s3_class(some_data, "data.frame")
  expect_type(some_data$species_id, "integer")
  expect_gt(rows_wo_filter, rows_w_filter)
})


test_that("frequency params work", {
  vcr::local_cassette("npn_download_x_frequency")

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    num_days_quality_filter = "30"
  )
  rows_month_filter <- nrow(some_data)

  some_data <- npn_download_site_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    num_days_quality_filter = "15"
  )
  rows_fortnight_filter <- nrow(some_data)
  expect_gt(rows_month_filter, rows_fortnight_filter)

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    period_frequency = "months"
  )
  rows_month_freq <- nrow(some_data)

  some_data <- npn_download_magnitude_phenometrics(
    request_source = "Unit Test",
    years = 2013,
    family_ids = c(322),
    period_frequency = "14"
  )
  rows_fortnight_freq <- nrow(some_data)
  expect_gt(rows_fortnight_freq, rows_month_freq)
})
