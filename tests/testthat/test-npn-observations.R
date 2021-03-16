context("npn_observations")
# Important to note that these test make up the bulk of the package's functionality
# And yet many of these tests are skipped by default.
# Because these all make HTTP requests to the npn data services, they are either
# stubbed or skipped to keep CRAN from failing on a service outage, etc.
# The first pack of tests use stubs to test general service functionality, and
# make sure responses are parsed correctly. Because the underlying code uses
# the r curl package, those stubs where manually generated. Any future stubs
# need to be manually generated in the same way.
# The other tests are skipped for brevity's sake, as they are generally just
# variations on the basic tests. Those tests are ALWAYS skipped on CRAN and can
# be enabled to run on a workstation by flipping the skip_long_tests flag that
# is setup in the R/zzz.R file.
#
# The references to VCR are still in place, even though they generally don't work
# just to be consistent with the rest of the code which does use VCR correctly.
# Maybe someday it will be adapted to work with curl.

skip_long_tests <- get_skip_long_tests()

test_that("no request source blocked", {
  npn_set_env(get_test_env())

  expect_error(npn_download_status_data(NULL,c(2013)))
  expect_error(npn_download_individual_phenometrics(NULL,c(2013)))
  expect_error(npn_download_site_phenometrics(NULL,c(2013)))
  expect_error(npn_download_magnitude_phenometrics(NULL,c(2013)))

})


test_that("basic function works", {
  npn_set_env(get_test_env())

  if(!check_service()){
    skip("Service is down")
  }

  vcr::use_cassette("npn_download_status_data_basic_1", {
    some_data <- npn_download_status_data(
      "Unit Test",
      c(2013),
      species_ids = c(6)
    )
  })


  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  vcr::use_cassette("npn_download_individual_phenometrics_basic_1", {
    some_data <- npn_download_individual_phenometrics(
      "Unit Test",
      c(2013),
      species_ids = c(6)
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  vcr::use_cassette("npn_download_site_phenometrics_basic_1", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      species_ids = c(6)
    )
  })

  num_site_default <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  vcr::use_cassette("npn_download_site_phenometrics_basic_2", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      species_ids = c(6),
      num_days_quality_filter = "5"
    )
  })
  num_site_custom <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)
  expect_gt(num_site_default, num_site_custom)

  vcr::use_cassette("npn_download_magnitude_phenometrics_basic_1", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      species_ids = c(6)
    )
  })
  num_mag_default <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  vcr::use_cassette("npn_download_magnitude_phenometrics_basic_2", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      species_ids = c(6),
      period_frequency = "14"
    )
  })
  num_mag_custom <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 25)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  expect_gt(num_mag_custom,num_mag_default)

})


test_that("file download works", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }

  test_download_path <- "unit-test-download.csv"

  some_data <- npn_download_status_data(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    download_path = test_download_path
  )


  expect_equal(file.exists(test_download_path), TRUE)
  some_data <- read.csv(test_download_path)

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  file.remove(test_download_path)


  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    download_path = test_download_path
  )
  expect_equal(file.exists(test_download_path), TRUE)
  some_data <- read.csv(test_download_path)

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  file.remove(test_download_path)

})


test_that("climate data flag works", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  vcr::use_cassette("npn_download_status_data_climate_flag_1", {
    some_data <- npn_download_status_data(
      "Unit Test",
      c(2013),
      species_ids = c(6),
      climate_data = TRUE
    )
  })

  expect_is(some_data,"data.frame")
  expect_is(some_data$tmin_winter, "numeric")

  vcr::use_cassette("npn_download_individual_phenometrics_climate_flag_1", {
    some_data <- npn_download_individual_phenometrics(
      "Unit Test",
      c(2013),
      species_ids = c(6),
      climate_data = TRUE
    )
  })

  expect_is(some_data,"data.frame")
  expect_is(some_data$tmin_winter, "numeric")

  vcr::use_cassette("npn_download_magnitude_phenometrics_climate_flag_1", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      species_ids = c(6),
      climate_data = TRUE
    )
  })

  expect_is(some_data,"data.frame")
  expect_null(some_data$tmin_winter)

})


test_that("higher taxonomic ordering works for status data", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  vcr::use_cassette("npn_download_status_data_tax_1", {
    some_data <- npn_download_status_data(
      "Unit Test",
      c(2013),
      family_ids = c(322),
      additional_fields = c("Family_ID")
    )
  })
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #Order_ID
  vcr::use_cassette("npn_download_status_data_tax_2", {
    some_data <- npn_download_status_data(
      "Unit Test",
      c(2013),
      order_ids = c(95),
      additional_fields = c("Order_ID")
    )
  })
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$order_id, "integer")
  expect_equal(some_data[1,]$order_id,95)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #class_ID
  vcr::use_cassette("npn_download_status_data_tax_3", {
    some_data <- npn_download_status_data(
     "Unit Test",
     c(2013),
     class_ids = c(15),
     additional_fields = c("Class_ID")
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$class_id, "integer")
  expect_equal(some_data[1,]$class_id,15)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


})


test_that("higher taxonomic ordering works for individual phenometrics", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  vcr::use_cassette("npn_download_individual_phenometrics_tax_1", {
    some_data <- npn_download_individual_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322),
      additional_fields = c("Family_ID")
    )
  })
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 100)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #Order_ID
  vcr::use_cassette("npn_download_individual_phenometrics_tax_2", {
    some_data <- npn_download_individual_phenometrics(
      "Unit Test",
      c(2013),
      order_ids = c(95),
      additional_fields = c("Order_ID")
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 100)
  expect_is(some_data$order_id, "integer")
  expect_equal(some_data[1,]$order_id,95)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


  # #class_ID
  vcr::use_cassette("npn_download_individual_phenometrics_tax_3", {
    some_data <- npn_download_individual_phenometrics(
     "Unit Test",
     c(2013),
     class_ids = c(15),
     additional_fields = c("Class_ID")
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$class_id, "integer")
  expect_equal(some_data[1,]$class_id,15)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


})


test_that("higher taxonomic ordering works for site phenometrics", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  #Check the different taxonomic levels for
  #status data

  #Family_ID
  vcr::use_cassette("npn_download_site_phenometrics_tax_1", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322),
      additional_fields = c("Family_ID")
    )
  })
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #Order_ID
  vcr::use_cassette("npn_download_site_phenometrics_tax_2", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      order_ids = c(95),
      additional_fields = c("Order_ID")
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$order_id, "integer")
  expect_equal(some_data[1,]$order_id,95)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


  # #class_ID
  vcr::use_cassette("npn_download_site_phenometrics_tax_3", {
    some_data <- npn_download_site_phenometrics(
     "Unit Test",
     c(2013),
     class_ids = c(15),
     additional_fields = c("Class_ID")
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$class_id, "integer")
  expect_equal(some_data[1,]$class_id,15)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


})


test_that("higher taxonomic ordering works for magnitude phenometrics", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  vcr::use_cassette("npn_download_magnitude_phenometrics_tax_1", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322),
      additional_fields = c("Family_ID")
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #Order_ID
  vcr::use_cassette("npn_download_magnitude_phenometrics_tax_2", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      order_ids = c(95),
      additional_fields = c("Order_ID")
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 100)
  expect_is(some_data$order_id, "integer")
  expect_equal(some_data[1,]$order_id,95)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  # #class_ID
  vcr::use_cassette("npn_download_magnitude_phenometrics_tax_3", {
    some_data <- npn_download_magnitude_phenometrics(
     "Unit Test",
     c(2013),
     class_ids = c(15),
     additional_fields = c("Class_ID")
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$class_id, "integer")
  expect_equal(some_data[1,]$class_id,15)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


})


test_that("higher level taxonomic agg and pheno agg works for site level",{

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }

  vcr::use_cassette("npn_download_site_phenometrics_pheno_agg_1", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322),
      taxonomy_aggregate = TRUE
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)
  expect_null(some_data$species_id)


  vcr::use_cassette("npn_download_site_phenometrics_pheno_agg_2", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322),
      pheno_class_aggregate = TRUE, pheno_class_ids = c(1,3,6)
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_null(some_data$phenophase_id)

  vcr::use_cassette("npn_download_site_phenometrics_pheno_agg_3", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      order_ids = c(78),
      pheno_class_ids = c(1,3,6),
      pheno_class_aggregate = TRUE,
      taxonomy_aggregate = TRUE
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 100)
  expect_is(some_data$pheno_class_name, "character")
  expect_is(some_data$order_id, "integer")
  expect_null(some_data$phenophase_id)


})

test_that("higher level taxonomic agg works for magnitude", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }

  vcr::use_cassette("npn_download_magnitude_phenometrics_pheno_agg_1", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322),
      taxonomy_aggregate = TRUE
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)
  expect_null(some_data$species_id)

  vcr::use_cassette("npn_download_magnitude_phenometrics_pheno_agg_2", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322),
      pheno_class_aggregate = TRUE, pheno_class_ids = c(1,3,6)
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_null(some_data$phenophase_id)

  vcr::use_cassette("npn_download_magnitude_phenometrics_pheno_agg_3", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      order_ids = c(78),
      pheno_class_ids = c(1,3,6),
      pheno_class_aggregate = TRUE,
      taxonomy_aggregate = TRUE
    )
  })

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$pheno_class_name, "character")
  expect_is(some_data$order_id, "integer")
  expect_null(some_data$phenophase_id)

})

test_that("six concordance works for status", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  vcr::use_cassette("npn_download_status_data_six_concord_1", {
    some_data <- npn_download_status_data(
     "Unit Test",
     c(2016),
     species_ids = c(6),
     six_leaf_layer = TRUE,
     six_bloom_layer = TRUE,
     agdd_layer = 32,
     additional_layers = data.frame(name=c("si-x:30yr_avg_4k_leaf"),param=c("365"))
    )
  })

  expect_is(some_data,"data.frame")
  expect_is(some_data$`SI-x_Bloom_Value`,"numeric")
  expect_is(some_data$`SI-x_Leaf_Value`,"numeric")
  expect_is(some_data$`si-x:30yr_avg_4k_leaf`,"numeric")
  expect_is(some_data$`gdd:agdd`,"numeric")
  expect_gt(some_data[1,"SI-x_Bloom_Value"],-1)
  expect_lt(some_data[1,"SI-x_Bloom_Value"],250)

  expect_gt(some_data[1,"SI-x_Leaf_Value"],-1)
  expect_lt(some_data[1,"SI-x_Leaf_Value"],250)

  expect_gt(some_data[1,"si-x:30yr_avg_4k_leaf"],-1)
  expect_lt(some_data[1,"si-x:30yr_avg_4k_leaf"],250)

  expect_gt(some_data[1,"gdd:agdd"],-1)

  avg_leaf_data <- some_data$`SI-x_Leaf_Value`

  # Test sub-model functionality
  vcr::use_cassette("npn_download_status_data_six_concord_2", {
    some_data <- npn_download_status_data(
     "Unit Test",
     c(2016),
     species_ids = c(6),
     six_leaf_layer = TRUE,
     six_sub_model = "lilac"
    )
  })

  expect_gt(some_data[1,"SI-x_Leaf_Value"],-1)
  expect_lt(some_data[1,"SI-x_Leaf_Value"],250)
  expect_equal(identical(some_data$`SI-x_Leaf_Value`,avg_leaf_data),FALSE)


  # This is testing that the implicit
  # reconciliation with different SI-x
  # layers is happening based on the date
  #
  # In this case get NCEP data
  vcr::use_cassette("npn_download_status_data_six_concord_3", {
    some_data <- npn_download_status_data(
      "Unit Test",
      c(2019),
      species_ids = c(3),
      six_leaf_layer = TRUE,
      six_sub_model = "lilac"
    )
  })

  expect_gt(some_data[1,"SI-x_Leaf_Value"],-1)
  expect_lt(some_data[1,"SI-x_Leaf_Value"],250)

  # In this case get PRISM data
  vcr::use_cassette("npn_download_status_data_six_concord_4", {
    some_data <- npn_download_status_data(
      "Unit Test",
      c(2009),
      species_ids = c(3),
      six_leaf_layer = TRUE,
      six_sub_model = "lilac"
    )
  })

  expect_gt(some_data[1,"SI-x_Leaf_Value"],-1)
  expect_lt(some_data[1,"SI-x_Leaf_Value"],250)

})


test_that("wkt filter works", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }


  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }
  #wkt is for CO
  wkt_def <- "POLYGON ((-102.04224 36.993083,-109.045223 36.999084,-109.050076 41.000659,-102.051614 41.002377,-102.04224 36.993083))"

  vcr::use_cassette("npn_download_status_data_wkt_1", {
    some_data <- npn_download_status_data(
      "Unit Test",
      c(2016),
      species_ids = c(35)
    )
  })

  rows_wo_filter <- nrow(some_data)

  vcr::use_cassette("npn_download_status_data_wkt_2", {
    some_data <- npn_download_status_data(
      "Unit Test",
      c(2016),
      species_ids = c(35),
      wkt = wkt_def
    )
  })
  rows_w_filter <- nrow(some_data)

  expect_is(some_data, "data.frame")
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$state,"CO")
  expect_gt(rows_wo_filter, rows_w_filter)

  vcr::use_cassette("npn_download_individual_phenometrics_wkt_1", {
    some_data <- npn_download_individual_phenometrics(
      "Unit Test",
      c(2016),
      species_ids = c(35)
    )
  })
  rows_wo_filter <- nrow(some_data)

  vcr::use_cassette("npn_download_individual_phenometrics_wkt_2", {
    some_data <- npn_download_individual_phenometrics(
      "Unit Test",
      c(2016),
      species_ids = c(35),
      wkt = wkt_def
    )
  })
  rows_w_filter <- nrow(some_data)

  expect_is(some_data, "data.frame")
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$state,"CO")
  expect_gt(rows_wo_filter, rows_w_filter)


  vcr::use_cassette("npn_download_site_phenometrics_wkt_1", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2016),
      species_ids = c(35)
    )
  })
  rows_wo_filter <- nrow(some_data)

  vcr::use_cassette("npn_download_site_phenometrics_wkt_2", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2016),
      species_ids = c(35),
      wkt = wkt_def
    )
    rows_w_filter <- nrow(some_data)
  })

  expect_is(some_data, "data.frame")
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$state,"CO")
  expect_gt(rows_wo_filter, rows_w_filter)


  vcr::use_cassette("npn_download_magnitude_phenometrics_wkt_1", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2016),
      species_ids = c(35)
    )
  })
  rows_wo_filter <- nrow(some_data)

  vcr::use_cassette("npn_download_magnitude_phenometrics_wkt_2", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2016),
      species_ids = c(35),
      wkt = wkt_def
    )
  })
  rows_w_filter <- nrow(some_data)

  expect_is(some_data, "data.frame")
  expect_is(some_data$species_id, "integer")
  expect_gt(rows_wo_filter, rows_w_filter)

})


test_that("frequency params work", {

  skip_on_cran()
  if(skip_long_tests){
    skip("Skipping long tests")
  }

  npn_set_env(get_test_env())
  if(!check_service()){
    skip("Service is down")
  }

  vcr::use_cassette("npn_download_site_phenometrics_frequency_1", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322), num_days_quality_filter = "30"
    )
  })

  rows_month_filter <- nrow(some_data)

  vcr::use_cassette("npn_download_site_phenometrics_frequency_2", {
    some_data <- npn_download_site_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322), num_days_quality_filter = "15"
    )
  })

  rows_fortnight_filter <- nrow(some_data)

  expect_gt(rows_month_filter, rows_fortnight_filter)

  vcr::use_cassette("npn_download_magnitude_phenometrics_frequency_1", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322), period_frequency = "months"
    )
  })

  rows_month_freq <- nrow(some_data)

  vcr::use_cassette("npn_download_magnitude_phenometrics_frequency_2", {
    some_data <- npn_download_magnitude_phenometrics(
      "Unit Test",
      c(2013),
      family_ids = c(322), period_frequency = "14"
    )
  })
  rows_fortnight_freq <- nrow(some_data)

  expect_gt(rows_fortnight_freq, rows_month_freq)

})
