context("npn_defunct")


test_that("npn_indsatstations defunct", {
  npn_set_env(get_test_env())

  expect_error(npn_indsatstations())
  expect_error(npn_indsatstations(stationid = c(507, 523)))
})


test_that("npn_indspatstations defunct", {
  npn_set_env(get_test_env())

  expect_error(npn_indspatstations())
  expect_error(npn_indspatstations(speciesid = 35,
                                   stationid = c(60, 259), year = 2009))
})

test_that("npn_obsspbyday", {
  npn_set_env(get_test_env())

  expect_error(npn_obsspbyday())
  expect_error(npn_obsspbyday(speciesid=357, startdate='2010-04-01', enddate='2012-01-05'))
})

test_that("npn_stationsbystate", {
  npn_set_env(get_test_env())

  expect_error(npn_stationsbystate())
  expect_error(npn_stationsbystate("foo"))
})


test_that("npn_stationswithspp", {
  npn_set_env(get_test_env())

  expect_error(npn_stationswithspp())
  expect_error(npn_stationswithspp(speciesid = c(52,53,54)))
})
