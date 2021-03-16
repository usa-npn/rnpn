context("npn_datasets")


test_that("datasets endpoint working", {
  npn_set_env(get_test_env())
  check_service()
  vcr::use_cassette("npn_datasets_1", {
    datasets <- npn_datasets()
  })



  expect_is(datasets, "data.frame")
  expect_is(datasets$dataset_name, "character")
  expect_gt(nrow(datasets), 5)

})
