context("npn_observations")


test_that("no request source blocked", {
  expect_error(npn_download_status_data(NULL,c(2013)))
  expect_error(npn_download_individual_phenometrics(NULL,c(2013)))
  expect_error(npn_download_site_phenometrics(NULL,c(2013)))
  expect_error(npn_download_magnitude_phenometrics(NULL,c(2013)))

})

test_that("basic function works", {

  some_data <- npn_download_status_data(
    "Unit Test",
    c(2013),
    species_ids = c(6)
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  some_data <- npn_download_individual_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6)
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6)
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    num_days_quality_filter = "5"
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6)
  )
  num_mag_default <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    period_frequency = "14"
  )
  num_mag_custom <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 25)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  expect_gt(num_mag_custom,num_mag_default)


})
