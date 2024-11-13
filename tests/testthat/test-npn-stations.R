test_that("npn_stations functions", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_stations_1", {
    stations <- npn_stations()
  })

  expect_s3_class(stations, "data.frame")
  expect_gt(nrow(stations), 1000)
  expect_type(stations$station_name, "character")

  vcr::use_cassette("npn_stations_2", {
    stations <- npn_stations("AZ")
  })

  expect_s3_class(stations, "data.frame")
  expect_gt(nrow(stations), 1000)
  expect_type(stations$station_name, "character")

  expect_error(npn_stations("foo"))
})


test_that("npn_stations_by_state functions", {
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_stations_by_state", {
   res <- npn_stations_by_state()
  })

  expect_s3_class(res, "data.frame")
  expect_gt(nrow(res), 45)
  expect_type(res$state, "character")
})


test_that("npn_stations_by_location functions",{
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_stations_by_location_1", {
    stations <- npn_stations_by_location(wkt="POLYGON((-110.94484396954107 32.23623109416672,-110.96166678448247 32.23594069208043,-110.95960684795904 32.21328646993733,-110.94244071026372 32.21343170728929,-110.93935080547857 32.23216538049456,-110.94484396954107 32.23623109416672))")
  })

  expect_s3_class(stations, "data.frame")
  expect_gt(nrow(stations), 50)
  expect_type(stations$station_id, "integer")

  expect_error(npn_stations_by_location())
})

test_that("npn_stations_with_spp functions",{
  skip_if_not(check_service(), "Service is down")

  vcr::use_cassette("npn_stations_with_spp_1", {
    stations <- npn_stations_with_spp(3)
  })

  expect_s3_class(stations, "data.frame")
  expect_gt(nrow(stations), 50)
  expect_type(stations$station_id, "integer")

  expect_error(npn_stations_with_spp())

  vcr::use_cassette("npn_stations_with_spp_2", {
    stations <- npn_stations_with_spp(9000)
  })
  expect_null(stations)

})
