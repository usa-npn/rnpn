test_that("npn_species returns a data frame", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_species_1", {
    species <- npn_species()
  })

  expect_s3_class(species, "data.frame")

  #should expect a large data frame with at least this many records
  expect_gt(nrow(species), 500)
})


test_that("npn_species_id working", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_species_id_1", {
    species <- npn_species_id(3)
  })

  expect_s3_class(species, "data.frame")
  expect_equal(species$common_name, "red maple")
})

test_that("npn_species_state works", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_species_state_1", {
    species_state <- npn_species_state("AZ")
  })

  expect_s3_class(species_state, "data.frame")
  #should see at least this many species
  expect_gt(nrow(species_state), 10)
  expect_type(species_state$species, "character")

  vcr::use_cassette("npn_species_state_2", {
    species_state <- npn_species_state("AZ", "Animalia")
  })

  expect_s3_class(species_state, "data.frame")
  #should see at least this many species
  expect_gt(nrow(species_state), 10)
  expect_type(species_state$species, "character")

  expect_error(npn_species_state("AZ", "Something else"))
  #TODO would be ideal if this was an error, but currently not easy to validate state (more than just state.abb are allowed)
  expect_identical(npn_species_state("ZZ"), tibble::tibble())
})

test_that("npn_species_search works", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_species_search_1", {
    species <- npn_species_search(
      start_date = "2023-01-01",
      end_date = "2023-05-15"
    )
  })

  expect_s3_class(species, "data.frame")
  expect_gt(nrow(species), 10)

  vcr::use_cassette("npn_species_search_2", {
    species <- npn_species_search(
      start_date = "2013-01-01",
      end_date = "2013-05-15"
    )
  })

  expect_s3_class(species, "data.frame")
  expect_gt(nrow(species), 10)
})


test_that("npn_species_types", {
  skip_if_not(check_service(), "Service is down")

  #There's at least 15 things in any of these cases
  vcr::use_cassette("npn_species_types_1", {
    t <- npn_species_types()
  })

  expect_s3_class(t, "data.frame")
  expect_gt(nrow(t), 30)

  vcr::use_cassette("npn_species_types_2", {
    t <- npn_species_types("Plantae")
  })

  expect_s3_class(t, "data.frame")
  expect_gt(nrow(t), 15)

  vcr::use_cassette("npn_species_types_3", {
    t <- npn_species_types("Animalia")
  })
  expect_s3_class(t, "data.frame")
  expect_gt(nrow(t), 15)

  expect_error(npn_species_types("foo"))
})
