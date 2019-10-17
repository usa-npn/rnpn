context("npn_observations")


test_that("no request source blocked", {
  npn_set_env(get_test_env())

  expect_error(npn_download_status_data(NULL,c(2013)))
  expect_error(npn_download_individual_phenometrics(NULL,c(2013)))
  expect_error(npn_download_site_phenometrics(NULL,c(2013)))
  expect_error(npn_download_magnitude_phenometrics(NULL,c(2013)))

})


test_that("basic function works", {
  npn_set_env(get_test_env())

  some_data <- npn_download_status_data(
    "Unit Test",
    c(2013),
    species_ids = c(6)
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  some_data <- npn_download_individual_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6)
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6)
  )
  num_site_default <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)


  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    num_days_quality_filter = "5"
  )
  num_site_custom <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)
  expect_gt(num_site_default, num_site_custom)


  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6)
  )
  num_mag_default <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    period_frequency = "14"
  )
  num_mag_custom <- nrow(some_data)
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 25)
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$species_id,6)

  expect_gt(num_mag_custom,num_mag_default)


})


test_that("file download works", {
  npn_set_env(get_test_env())

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
  npn_set_env(get_test_env())

  some_data <- npn_download_status_data(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    climate_data = TRUE
  )

  expect_is(some_data,"data.frame")
  expect_is(some_data$tmin_winter, "numeric")

  some_data <- npn_download_individual_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    climate_data = TRUE
  )

  expect_is(some_data,"data.frame")
  expect_is(some_data$tmin_winter, "numeric")

  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    species_ids = c(6),
    climate_data = TRUE
  )

  expect_is(some_data,"data.frame")
  expect_null(some_data$tmin_winter)

})


test_that("higher taxonomic ordering works for status data", {
  npn_set_env(get_test_env())

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  some_data <- npn_download_status_data(
    "Unit Test",
    c(2013),
    family_ids = c(322),
    additional_fields = c("Family_ID")
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #Order_ID
  some_data <- npn_download_status_data(
    "Unit Test",
    c(2013),
    order_ids = c(95),
    additional_fields = c("Order_ID")
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$order_id, "integer")
  expect_equal(some_data[1,]$order_id,95)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  skip("Takes too long")
  #class_ID
  some_data <- npn_download_status_data(
    "Unit Test",
    c(2013),
    class_ids = c(15),
    additional_fields = c("Class_ID")
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$class_id,15)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


})


test_that("higher taxonomic ordering works for individual phenometrics", {
  npn_set_env(get_test_env())


  #Check the different taxonomic levels for
  #status data

  #Family_ID
  some_data <- npn_download_individual_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322),
    additional_fields = c("Family_ID")
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 100)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #Order_ID
  some_data <- npn_download_individual_phenometrics(
    "Unit Test",
    c(2013),
    order_ids = c(95),
    additional_fields = c("Order_ID")
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 100)
  expect_is(some_data$order_id, "integer")
  expect_equal(some_data[1,]$order_id,95)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  skip("Takes too long")
  #class_ID
  some_data <- npn_download_individual_phenometrics(
    "Unit Test",
    c(2013),
    class_ids = c(15),
    additional_fields = c("Class_ID")
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$class_id,15)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


})


test_that("higher taxonomic ordering works for site phenometrics", {
  npn_set_env(get_test_env())

  #Check the different taxonomic levels for
  #status data

  #Family_ID
  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322),
    additional_fields = c("Family_ID")
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #Order_ID
  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    order_ids = c(95),
    additional_fields = c("Order_ID")
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$order_id, "integer")
  expect_equal(some_data[1,]$order_id,95)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  skip("Takes too long")
  #class_ID
  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    class_ids = c(15),
    additional_fields = c("Class_ID")
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$class_id,15)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


})


test_that("higher taxonomic ordering works for magnitude phenometrics", {
  npn_set_env(get_test_env())


  #Check the different taxonomic levels for
  #status data

  #Family_ID
  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322),
    additional_fields = c("Family_ID")
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  #Order_ID
  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    order_ids = c(95),
    additional_fields = c("Order_ID")
  )
  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 100)
  expect_is(some_data$order_id, "integer")
  expect_equal(some_data[1,]$order_id,95)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)

  skip("Takes too long")
  #class_ID
  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    class_ids = c(15),
    additional_fields = c("Class_ID")
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 1000)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$class_id,15)

  less_data <- subset(some_data,species_id==6)
  expect_lt(nrow(less_data), nrow(some_data))
  expect_gt(nrow(less_data),0)


})


test_that("higher level taxonomic agg works for site level",{
  npn_set_env(get_test_env())

  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322),
    taxonomy_aggregate = TRUE
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)
  expect_null(some_data$species_id)


  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322),
    pheno_class_aggregate = TRUE, pheno_class_ids = c(1,3,6)
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_null(some_data$phenophase_id)


  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    order_ids = c(78),
    pheno_class_ids = c(1,3,6),
    pheno_class_aggregate = TRUE,
    taxonomy_aggregate = TRUE
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 100)
  expect_is(some_data$pheno_class_name, "character")
  expect_is(some_data$order_id, "integer")
  expect_null(some_data$phenophase_id)


})

test_that("higher level taxonomic agg works for magnitude", {
  npn_set_env(get_test_env())


  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322),
    taxonomy_aggregate = TRUE
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$family_id, "integer")
  expect_equal(some_data[1,]$family_id,322)
  expect_null(some_data$species_id)


  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322),
    pheno_class_aggregate = TRUE, pheno_class_ids = c(1,3,6)
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_null(some_data$phenophase_id)


  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    order_ids = c(78),
    pheno_class_ids = c(1,3,6),
    pheno_class_aggregate = TRUE,
    taxonomy_aggregate = TRUE
  )

  expect_is(some_data,"data.frame")
  expect_gt(nrow(some_data), 10)
  expect_is(some_data$pheno_class_name, "character")
  expect_is(some_data$order_id, "integer")
  expect_null(some_data$phenophase_id)

})

test_that("six concordance works for status", {
  npn_set_env(get_test_env())


  some_data <- npn_download_status_data(
   "Unit Test",
   c(2016),
   species_ids = c(6),
   six_leaf_layer = TRUE,
   six_bloom_layer = TRUE,
   agdd_layer = 32,
   additional_layers = data.frame(name=c("si-x:30yr_avg_4k_leaf"),param=c("365"))
  )

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
  some_data <- npn_download_status_data(
   "Unit Test",
   c(2016),
   species_ids = c(6),
   six_leaf_layer = TRUE,
   six_sub_model = "lilac"
  )

  expect_gt(some_data[1,"SI-x_Leaf_Value"],-1)
  expect_lt(some_data[1,"SI-x_Leaf_Value"],250)
  expect_equal(identical(some_data$`SI-x_Leaf_Value`,avg_leaf_data),FALSE)


  # This is testing that the implicit
  # reconciliation with different SI-x
  # layers is happening based on the date
  #
  # In this case get NCEP data
  some_data <- npn_download_status_data(
    "Unit Test",
    c(2019),
    species_ids = c(3),
    six_leaf_layer = TRUE,
    six_sub_model = "lilac"
  )

  expect_gt(some_data[1,"SI-x_Leaf_Value"],-1)
  expect_lt(some_data[1,"SI-x_Leaf_Value"],250)

  # In this case get PRISM data
  some_data <- npn_download_status_data(
    "Unit Test",
    c(2009),
    species_ids = c(3),
    six_leaf_layer = TRUE,
    six_sub_model = "lilac"
  )

  expect_gt(some_data[1,"SI-x_Leaf_Value"],-1)
  expect_lt(some_data[1,"SI-x_Leaf_Value"],250)

})


test_that("wkt filter works", {
  npn_set_env(get_test_env())

  #wkt is for CO
  wkt_def <- "POLYGON ((-102.04224 36.993083,-109.045223 36.999084,-109.050076 41.000659,-102.051614 41.002377,-102.04224 36.993083))"

  some_data <- npn_download_status_data(
    "Unit Test",
    c(2016),
    species_ids = c(35)
  )
  rows_wo_filter <- nrow(some_data)

  some_data <- npn_download_status_data(
    "Unit Test",
    c(2016),
    species_ids = c(35),
    wkt = wkt_def
  )
  rows_w_filter <- nrow(some_data)

  expect_is(some_data, "data.frame")
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$state,"CO")
  expect_gt(rows_wo_filter, rows_w_filter)


  some_data <- npn_download_individual_phenometrics(
    "Unit Test",
    c(2016),
    species_ids = c(35)
  )
  rows_wo_filter <- nrow(some_data)


  some_data <- npn_download_individual_phenometrics(
    "Unit Test",
    c(2016),
    species_ids = c(35),
    wkt = wkt_def
  )
  rows_w_filter <- nrow(some_data)

  expect_is(some_data, "data.frame")
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$state,"CO")
  expect_gt(rows_wo_filter, rows_w_filter)



  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2016),
    species_ids = c(35)
  )
  rows_wo_filter <- nrow(some_data)


  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2016),
    species_ids = c(35),
    wkt = wkt_def
  )
  rows_w_filter <- nrow(some_data)

  expect_is(some_data, "data.frame")
  expect_is(some_data$species_id, "integer")
  expect_equal(some_data[1,]$state,"CO")
  expect_gt(rows_wo_filter, rows_w_filter)



  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2016),
    species_ids = c(35)
  )
  rows_wo_filter <- nrow(some_data)


  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2016),
    species_ids = c(35),
    wkt = wkt_def
  )
  rows_w_filter <- nrow(some_data)

  expect_is(some_data, "data.frame")
  expect_is(some_data$species_id, "integer")
  expect_gt(rows_wo_filter, rows_w_filter)

})


test_that("frequency params work", {
  npn_set_env(get_test_env())


  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322), num_days_quality_filter = "30"
  )
  rows_month_filter <- nrow(some_data)

  some_data <- npn_download_site_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322), num_days_quality_filter = "15"
  )
  rows_fortnight_filter <- nrow(some_data)

  expect_gt(rows_month_filter, rows_fortnight_filter)


  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322), period_frequency = "months"
  )
  rows_month_freq <- nrow(some_data)

  some_data <- npn_download_magnitude_phenometrics(
    "Unit Test",
    c(2013),
    family_ids = c(322), period_frequency = "14"
  )
  rows_fortnight_freq <- nrow(some_data)

  expect_gt(rows_fortnight_freq, rows_month_freq)

})
