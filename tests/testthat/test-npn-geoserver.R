context("npn_geospatial")


is_geo_service_up <- check_geo_service()

test_that("npn_get_layer_details works",{

  npn_set_env(get_test_env())
  if(!is_geo_service_up){
    skip("Geo Service is down")
  }
  #vcr::use_cassette("npn_get_layer_details_1", {
    layers <- npn_get_layer_details()
  #})



  expect_is(layers,"data.frame")
  expect_gt(nrow(layers),50)

})


test_that("npn_download_geospatial works", {

  skip("No file downloads")

  npn_set_env(get_test_env())
  library(raster)

  ras <- npn_download_geospatial("gdd:agdd",date="2018-05-05")


  expect_is(ras,"RasterLayer")


  npn_download_geospatial("gdd:agdd",date="2018-05-05",output_path = "testing.tiff")
  expect_equal(file.exists("testing.tiff"),TRUE)
  file_raster <- raster("testing.tiff")

  expect_equal(cellStats(ras,max),cellStats(file_raster,max))
  file.remove("testing.tiff")

  ras <- npn_download_geospatial("gdd:30yr_avg_agdd",date=50)
  expect_is(ras,"RasterLayer")

  #This layer not on DEV
  npn_set_env("ops")
  ras <- npn_download_geospatial("inca:midgup_median_nad83_02deg",date=NULL)
  expect_is(ras,"RasterLayer")

})

test_that("npn_download_geospatial format param working", {
  #skip_on_cran()
  skip("No file downloads")
  npn_set_env("ops")

  npn_download_geospatial(
    "gdd:30yr_avg_agdd_50f",
    date="5",
    output_path = "testing.tiff"
  )

  npn_download_geospatial(
    "gdd:30yr_avg_agdd_50f",
    date="1,3",
    format="application/x-netcdf",
    output_path = "testing.netcdf"
  )

  tiff_size <- file.size("testing.tiff")
  netcdf_size <- file.size("testing.netcdf")

  file.remove("testing.tiff")
  file.remove("testing.netcdf")

  #GeoTIFF and NetCDF are similar enough foramts that they
  # are nearly 1:1 in like sized rasters but there is some margin
  # of difference. This tests that a NETCDF file containg 3 times
  # as much data as a similar GeoTIFF is the same size within 25K.

  # This is useful as a test because if the URL is malformed or the
  # format is wrong, even if the request specifies a larger
  # date/elevation subset, still only one such raster will be
  # returned.
  #
  # EDIT: This changed circa 3/2020 when we updated the NetCDF libs
  # on Geoserver. This test "works", but since the two formats aren't
  # that comparable any more, it's a little dodgier, and this mostly
  # just checks that the NetCDF isn't empty or something (which happened
  # during our install of the new Geoserver), so still a useful test to have.

  expect_lt(abs((tiff_size * 2) - netcdf_size), 700000)



})


test_that("npn_get_point_data functions", {

  npn_set_env(get_test_env())
  if(!is_geo_service_up){
    skip("Geo Service is down")
  }
  vcr::use_cassette("npn_get_point_data_1", {
    value <- npn_get_point_data("gdd:agdd",38.8,-110.5,"2019-05-05")
  })
  expect_lt(round(value), 1235)
  expect_gt(round(value), 1232)

  vcr::use_cassette("npn_get_point_data_2", {
    value <- npn_get_point_data("si-x:average_leaf_prism",38.8,-110.5,"1990-01-01")
  })
  expect_equal(value, 83)

  #No data in Canada
  expect_error(npn_get_point_data("si-x:average_leaf_prism",60.916600, -123.037793,"1990-01-01"))

})


test_that("npn_custom_agdd functions",{

  npn_set_env(get_test_env())

  vcr::use_cassette("npn_get_custom_agdd_time_series_1", {
    res <- npn_get_custom_agdd_time_series(
      "double-sine",
      "2019-01-01",
      "2019-01-15",
      25,
      "NCEP",
      "fahrenheit",
      39.7,
      -107.5,
      upper_threshold=90
    )
  })

  expect_is(res,"data.frame")
  expect_equal(round(res[15,"agdd"]),34)

})

test_that("npn_get_agdd_point_data works",{

  npn_set_env(get_test_env())

  if(!check_service()){
    skip("Data Service is down")
  }

  res <- npn_get_agdd_point_data("gdd:agdd",32.4,-110,"2020-01-15")

  expect_is(res,"numeric")
  if(res > 0){
    expect_equal(round(res), 146)
  }
})


test_that("npn_get_custom_agdd_raster works",{

  npn_set_env(get_test_env())

  if(!check_data_service()){
    skip("Data Service is down")
  }

  res <- npn_get_custom_agdd_raster("simple","NCEP","Fahrenheit","2020-01-01","2020-01-15",32)

  expect_is(res,"RasterLayer")
})


