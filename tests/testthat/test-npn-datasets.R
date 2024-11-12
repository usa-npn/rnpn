test_that("datasets endpoint working", {
  if(!check_service()){
    skip("Service is down")
  }
  vcr::use_cassette("npn_datasets_1", {
    datasets <- npn_datasets()
  })

  expect_s3_class(datasets, "data.frame")
  expect_type(datasets$dataset_name, "character")
  expect_gt(nrow(datasets), 5)
})
