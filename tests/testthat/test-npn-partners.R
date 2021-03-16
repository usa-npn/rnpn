context("npn_partners")


test_that("npn_groups works",{
  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  vcr::use_cassette("npn_groups_1", {
    groups <- npn_groups()
  })


  expect_is(groups,"data.frame")
  expect_is(groups$network_name, "character")
  expect_gt(nrow(groups),50)

  groups <- npn_groups(TRUE)
  expect_is(groups,"data.frame")
  expect_is(groups$network_name, "character")

})
