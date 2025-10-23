test_that("datasets endpoint working", {
  vcr::local_cassette("npn_datasets")
  
  datasets <- npn_datasets()
  expect_s3_class(datasets, "data.frame")
  expect_type(datasets$dataset_name, "character")
  expect_gt(nrow(datasets), 5)
})
