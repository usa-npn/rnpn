test_that("npn_phenophases works", {
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_phenophases")

  pp <- npn_phenophases()
  expect_s3_class(pp, "data.frame")
  expect_type(pp$phenophase_name, "character")
  expect_equal(trimws(pp[1, "phenophase_name"]), "First leaf")
  expect_gt(nrow(pp), 100)
})


test_that("npn_phenophase_definitions works", {
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_phenophase_definitions")

  pp <- npn_phenophase_definitions()
  expect_s3_class(pp, "data.frame")
  expect_type(pp$phenophase_name, "character")
  expect_equal(trimws(pp[1, "phenophase_name"]), "First leaf")
  expect_gt(nrow(pp), 100)
})

test_that("npn_phenophase_details works", {
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_phenophase_details")

  pd <- npn_phenophase_details(56)
  expect_s3_class(pd, "data.frame")
  expect_type(pd$phenophase_names, "character")
  expect_equal(trimws(pd[1, "phenophase_names"]), "First leaf")

  pd <- npn_phenophase_details(c(56, 57))

  expect_s3_class(pd, "data.frame")
  expect_type(pd$phenophase_names, "character")
  expect_equal(trimws(pd[1, "phenophase_names"]), "First leaf")

  expect_identical(npn_phenophase_details("56,61"), tibble::tibble())
})


test_that("npn_phenophases_by_species works", {
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_phenophases_by_species")

  pp <- npn_phenophases_by_species(species_ids = 3, date = "2018-05-05")
  expect_s3_class(pp, "data.frame")
  expect_type(pp$species_name, "character")
})

test_that("npn_pheno_classes works", {
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_pheno_classes")

  pc <- npn_pheno_classes()
  expect_s3_class(pc, "data.frame")
  expect_type(pc$name, "character")
  expect_gt(nrow(pc), 50)
})

test_that("npn_abundance_categories works", {
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_abundance_categories")

  ac <- npn_abundance_categories()
  expect_s3_class(ac, "data.frame")
  expect_type(ac$category_name, "character")
  expect_gt(nrow(ac), 50)
})

test_that("npn_get_phenophases_for_taxon works", {
  skip_if_not(check_service(), "Service is down")
  vcr::local_cassette("npn_get_phenophases_for_taxon")

  expect_error(
    npn_get_phenophases_for_taxon(
      class_ids = 5,
      date = c("2018-05-05", "2018-05-06")
    )
  )

  pp <- npn_get_phenophases_for_taxon(class_ids = 5, date = "2018-05-05")
  pp_date <- npn_get_phenophases_for_taxon(
    class_ids = 5,
    date = as.Date("2018-05-05")
  )
  expect_s3_class(pp, "data.frame")
  expect_type(pp$class_name, "character")
  expect_equal(nrow(pp), 21)
  expect_identical(pp, pp_date)

  pp <- npn_get_phenophases_for_taxon(
    class_ids = c(5, 6),
    date = "2018-05-05"
  )
  expect_s3_class(pp, "data.frame")
  expect_type(pp$class_name, "character")
  expect_equal(nrow(pp), 28)

  pp <- npn_get_phenophases_for_taxon(
    family_ids = c(267, 268),
    date = "2018-05-05"
  )
  expect_s3_class(pp, "data.frame")
  expect_type(pp$family_name, "character")
  expect_equal(nrow(pp), 24)

  pp <- npn_get_phenophases_for_taxon(order_ids = c(74, 75), date = "all")
  expect_s3_class(pp, "data.frame")
  expect_type(pp$order_name, "character")
  expect_equal(nrow(pp), 155)
})
