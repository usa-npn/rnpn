test_that("npn_phenophases works", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_phenophases_1", {
    pp <- npn_phenophases()
  })

  expect_s3_class(pp, "data.frame")
  expect_type(pp$phenophase_name, "character")
  expect_equal(trimws(pp[1,"phenophase_name"]), "First leaf")
  expect_gt(nrow(pp), 100)
})


test_that("npn_phenophase_definitions works", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_phenophase_definitions_1", {
    pp <- npn_phenophase_definitions()
  })

  expect_s3_class(pp, "data.frame")
  expect_type(pp$phenophase_name, "character")
  expect_equal(trimws(pp[1,"phenophase_name"]),"First leaf")
  expect_gt(nrow(pp),100)
})

test_that("npn_phenophase_details works", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_phenophase_details_1", {
    pd <- npn_phenophase_details(56)
  })

  expect_s3_class(pd, "data.frame")
  expect_type(pd$phenophase_names, "character")
  expect_equal(trimws(pd[1,"phenophase_names"]), "First leaf")

  pd <- npn_phenophase_details(c(56, 57))

  expect_s3_class(pd, "data.frame")
  expect_type(pd$phenophase_names, "character")
  expect_equal(trimws(pd[1,"phenophase_names"]), "First leaf")

  expect_identical(npn_phenophase_details("56,61"), tibble::tibble())
})


test_that("npn_phenophases_by_species works",{
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_phenophases_by_species_1", {
    pp <- npn_phenophases_by_species(3,"2018-05-05")
  })

  expect_s3_class(pp,"data.frame")
  expect_type(pp$species_name,"character")
})

test_that("npn_pheno_classes works",{
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_pheno_classes_1", {
    pc <- npn_pheno_classes()
  })

  expect_s3_class(pc, "data.frame")
  expect_type(pc$name, "character")
  expect_gt(nrow(pc), 50)
})

test_that("npn_abundance_categories works",{
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_abundance_categories_1", {
    ac <- npn_abundance_categories()
  })

  expect_s3_class(ac, "data.frame")
  expect_type(ac$category_name, "character")
  expect_gt(nrow(ac), 50)
})


test_that("npn_get_phenophases_for_taxon works", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_get_phenophases_for_taxon_1", {
    pp <- npn_get_phenophases_for_taxon(class_ids=5, date="2018-05-05")
  })

  expect_s3_class(pp, "data.frame")
  expect_type(pp$class_name, "character")
  expect_equal(nrow(pp), 21)

  vcr::use_cassette("npn_get_phenophases_for_taxon_2", {
    pp <- npn_get_phenophases_for_taxon(class_ids=c(5,6), date="2018-05-05")
  })

  expect_s3_class(pp, "data.frame")
  expect_type(pp$class_name, "character")
  expect_equal(nrow(pp), 28)

  vcr::use_cassette("npn_get_phenophases_for_taxon_3", {
    pp <- npn_get_phenophases_for_taxon(family_ids=c(267,268),date="2018-05-05")
  })

  expect_s3_class(pp, "data.frame")
  expect_type(pp$family_name, "character")
  expect_equal(nrow(pp), 24)

  vcr::use_cassette("npn_get_phenophases_for_taxon_4", {
    pp <- npn_get_phenophases_for_taxon(order_ids=c(74,75), date="2018-05-05", return_all = 0)
  })

  expect_s3_class(pp, "data.frame")
  expect_type(pp$order_name, "character")
  expect_equal(nrow(pp), 21)


  vcr::use_cassette("npn_get_phenophases_for_taxon_5", {
    pp <- npn_get_phenophases_for_taxon(order_ids=c(74,75), return_all = 1)
  })

  expect_s3_class(pp, "data.frame")
  expect_type(pp$order_name, "character")
  expect_equal(nrow(pp), 155)

  skip("unclear if this last one is supposed to work or error")
  vcr::use_cassette("npn_get_phenophases_for_taxon_6", {
    pp <- npn_get_phenophases_for_taxon(order_ids=c(74,75), return_all = TRUE)
  })

  expect_s3_class(pp, "data.frame")
  expect_type(pp$order_name, "character")
  expect_equal(nrow(pp), 2)
})
