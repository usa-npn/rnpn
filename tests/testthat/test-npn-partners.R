context("npn_partners")

skip("Moving on")

test_that("npn_groups works",{
  groups <- npn_groups()

  expect_is(groups,"data.frame")
  expect_is(groups$network_name, "character")
  expect_gt(nrow(groups),50)

  groups <- npn_groups(TRUE)
  expect_is(groups,"data.frame")
  expect_is(groups$network_name, "character")

})
