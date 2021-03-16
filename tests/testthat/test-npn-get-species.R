context("npn_species")



test_that("npn_species returns a data frame", {
  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  vcr::use_cassette("npn_species_1", {
    species <- npn_species()
  })

  expect_is(species, "data.frame")

  #should expect a large data frame with at least this many records
  expect_equal(nrow(species) > 500, TRUE)

})

test_that("npn_species_id working", {
  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  vcr::use_cassette("npn_species_id_1", {
    species <- npn_species_id(3)
  })


  expect_is(species, "data.frame")
  expect_equal(species$common_name, "red maple")
})


test_that("npn_species_state works",{
  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  vcr::use_cassette("npn_species_state_1", {
    species_state <- npn_species_state("AZ")
  })


  expect_is(species_state, "data.frame")
  #should see at least this many species
  expect_gt(nrow(species_state),10)
  expect_is(species_state$species, "character")

  vcr::use_cassette("npn_species_state_2", {
    species_state <- npn_species_state("AZ", "Animalia")
  })

  expect_is(species_state, "data.frame")
  #should see at least this many species
  expect_gt(nrow(species_state),10)
  expect_is(species_state$species, "character")

  expect_null(npn_species_state("AZ", "Something else"))
  expect_null(npn_species_state("ZZ"))
})

test_that("npn_species_search works",{
  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  vcr::use_cassette("npn_species_search_1", {
    species <- npn_species_search()
  })

  expect_is(species, "data.frame")
  expect_gt(nrow(species),10)

  vcr::use_cassette("npn_species_search_2", {
    species <- npn_species_search(start_date="2013-01-01",end_date="2013-05-15")
  })

  expect_is(species, "data.frame")
  expect_gt(nrow(species),10)


})

test_that("npn_species_types",{
  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }

  #There's at least 15 things in any of these cases
  vcr::use_cassette("npn_species_types_1", {
    t <- npn_species_types()
  })

  expect_is(t, "data.frame")
  expect_gt(nrow(t), 15)


  vcr::use_cassette("npn_species_types_2", {
    t <- npn_species_types("Plantae")
  })

  expect_is(t, "data.frame")
  expect_gt(nrow(t), 15)

  vcr::use_cassette("npn_species_types_3", {
    t <- npn_species_types("Animalia")
  })
  expect_is(t, "data.frame")
  expect_gt(nrow(t), 15)

  #Actually, combine the animal/plant list if input is something else
  vcr::use_cassette("npn_species_types_4", {
    t <- npn_species_types("foo")
  })

  expect_is(t, "data.frame")
  expect_gt(nrow(t), 30)

})

