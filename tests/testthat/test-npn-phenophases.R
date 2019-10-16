context("npn_phenophases")

skip("moving on")

test_that("npn_phenophases works", {
  pp <- npn_phenophases()

  expect_is(pp, "data.frame")
  expect_is(pp$phenophase_name, "character")
  expect_equal(trimws(pp[1,"phenophase_name"]),"First leaf")
  expect_gt(nrow(pp),100)

})


test_that("npn_phenophase_definitions works", {
  pp <- npn_phenophase_definitions()

  expect_is(pp, "data.frame")
  expect_is(pp$phenophase_name, "character")
  expect_equal(trimws(pp[1,"phenophase_name"]),"First leaf")
  expect_gt(nrow(pp),100)

})

test_that("npn_phenophase_details works", {
  pd <- npn_phenophase_details(56)

  expect_is(pd, "data.frame")
  expect_is(pd$phenophase_names, "character")
  expect_equal(trimws(pd[1,"phenophase_names"]),"First leaf")

  pd <- npn_phenophase_details("56,61")

  expect_is(pd, "data.frame")
  expect_is(pd$phenophase_names, "character")
  expect_equal(trimws(pd[1,"phenophase_names"]),"First leaf")

  expect_error(npn_phenophase_details())
  expect_error(npn_phenophase_details(56,61))

})


test_that("npn_phenophases_by_species works",{
  pp <- npn_phenophases_by_species(3,"2018-05-05")

  expect_is(pp,"data.frame")
  expect_is(pp$species_name,"character")

  #TODO: Fix this when underlying code fixed

})

test_that("npn_pheno_classes works",{
  pc <- npn_pheno_classes()

  expect_is(pc,"data.frame")
  expect_is(pc$name,"character")
  expect_gt(nrow(pc),50)
})

test_that("npn_abundance_categories works",{
  ac <- npn_abundance_categories()

  expect_is(ac,"data.frame")
  expect_is(ac$category_name,"character")
  expect_gt(nrow(ac),50)
})


test_that("npn_get_phenophases_for_taxon works", {
  npn_set_env("dev")
  pp <- npn_get_phenophases_for_taxon(class_ids=5,date="2018-05-05")

  expect_is(pp,"list")
  expect_is(pp[[1]]$class_name, "character")
  expect_length(pp,1)

  pp <- npn_get_phenophases_for_taxon(class_ids=c(5,6),date="2018-05-05")

  expect_is(pp,"list")
  expect_is(pp[[1]]$class_name, "character")
  expect_gt(length(pp),1)

  pp <- npn_get_phenophases_for_taxon(family_ids=c(267,268),date="2018-05-05")

  expect_is(pp,"list")
  expect_is(pp[[1]]$family_name, "character")
  expect_gt(length(pp),1)

  pp <- npn_get_phenophases_for_taxon(order_ids=c(74,75),date="2018-05-05", return_all = 0)

  expect_is(pp,"list")
  expect_is(pp[[1]]$order_name, "character")
  expect_gt(length(pp),1)


  pp <- npn_get_phenophases_for_taxon(order_ids=c(74,75),return_all = 1)

  expect_is(pp,"list")
  expect_is(pp[[1]]$order_name, "character")
  expect_gt(length(pp),1)


  pp <- npn_get_phenophases_for_taxon(order_ids=c(74,75),return_all = TRUE)

  expect_is(pp,"list")
  expect_length(pp,0)

})





