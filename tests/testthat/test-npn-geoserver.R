skip_long_tests <- as.logical(Sys.getenv("RNPN_SKIP_LONG_TESTS", unset = "true"))

test_that("npn_get_layer_details works", {
  skip_on_cran()
  skip_if_not(check_geo_service(), "Geo Service is down")

  vcr::use_cassette("npn_get_layer_details_1", {
    layers <- npn_get_layer_details()
  })
  expect_s3_class(layers, "data.frame")
  expect_gt(nrow(layers), 50)
})


test_that("npn_download_geospatial works", {
  skip_on_cran()
  skip_if_not(check_geo_service(), "Geo Service is down")

  ras <- npn_download_geospatial(coverage_id = "gdd:agdd", date = "2018-05-05")
  expect_s4_class(ras, "SpatRaster")

  withr::with_tempfile("test_tiff", {
    npn_download_geospatial("gdd:agdd", date="2018-05-05", output_path = test_tiff)
    expect_true(file.exists(test_tiff))
    file_raster <- terra::rast(test_tiff)
    expect_equal(
      max(terra::values(ras), na.rm = TRUE),
      max(terra::values(file_raster), na.rm = TRUE)
    )
  })

  ras <- npn_download_geospatial("gdd:30yr_avg_agdd", date = "50")
  expect_s4_class(ras, "SpatRaster")

  ras <- npn_download_geospatial("inca:midgup_median_nad83_02deg", date = NULL)
  expect_s4_class(ras, "SpatRaster")
})


test_that("npn_download_geospatial format param working", {
  skip_on_cran()
  skip_if_not(check_geo_service(), "Geo Service is down")

  withr::with_tempdir({
    npn_download_geospatial(
      "gdd:30yr_avg_agdd_50f",
      date="5",
      output_path = "testing.tiff"
    )
    tiff_size <- file.size("testing.tiff")
    npn_download_geospatial(
      "gdd:30yr_avg_agdd_50f",
      date="1,3",
      format="application/x-netcdf",
      output_path = "testing.netcdf"
    )
    netcdf_size <- file.size("testing.netcdf")
  })

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
  skip_on_cran()
  skip_if_not(check_geo_service(), "Geo Service is down")

  vcr::use_cassette("npn_get_point_data_1", {
    value <- npn_get_point_data("gdd:agdd", 38.8, -110.5, "2022-05-05")
  })
  expect_lt(round(value), 1201)
  expect_gt(round(value), 1198)

  vcr::use_cassette("npn_get_point_data_2", {
    value <- npn_get_point_data("si-x:average_leaf_prism", 38.8, -110.5, "1990-01-01")
  })
  expect_equal(value, 83)

  #No data in Canada
  expect_error(
    vcr::use_cassette("npn_get_point_data_3", {
      npn_get_point_data("si-x:average_leaf_prism", 60.916600, -123.037793, "1990-01-01")
    })
  )
})


test_that("npn_custom_agdd functions",{
  skip_on_cran()

  vcr::use_cassette("npn_get_custom_agdd_time_series_1", {
    res <- npn_get_custom_agdd_time_series(
      method = "double-sine",
      start_date = "2019-01-01",
      end_date = "2019-01-15",
      base_temp = 25,
      climate_data_source = "NCEP",
      temp_unit = "fahrenheit",
      lat = 39.7,
      long = -107.5,
      upper_threshold = 90
    )
  })

  expect_s3_class(res, "data.frame")
  expect_equal(round(res$agdd[15]), 34)
})


test_that("npn_get_agdd_point_data works",{
  skip_on_cran()
  skip_if_not(check_service(), "Data Service is down")
  vcr::use_cassette("npn_get_agdd_point_data", {
    res <- npn_get_agdd_point_data(
      layer = "gdd:agdd",
      lat = 32.4,
      long = -110,
      date = "2020-01-15"
    )
  })

  expect_type(res, "double")
  if(res > 0){
    expect_equal(round(res), 146)
  }
})


test_that("npn_get_custom_agdd_raster works", {
  skip_on_cran()
  skip_if(skip_long_tests)
  # skip_if_not(check_data_service(), "Data Service is down")

  res <- npn_get_custom_agdd_raster(
    method = "simple",
    climate_data_source = "NCEP",
    temp_unit = "Fahrenheit",
    start_date = "2020-01-01",
    end_date = "2020-01-15",
    base_temp = 32
  )

  expect_s4_class(res, "SpatRaster")
})


