test_that("npn_groups works", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_groups_1", {
    groups <- npn_groups()
  })

  expect_s3_class(groups, "data.frame")
  expect_type(groups$network_name, "character")
  expect_gt(nrow(groups),50)

  groups <- npn_groups(TRUE)
  expect_s3_class(groups, "data.frame")
  expect_type(groups$network_name, "character")
})
