#' Get Geospatial Data Layer Details
#'
#' This function will return information about the various data layers available
#' via the NPN's geospatial web services. Specifically, this function will query
#' the NPN's GetCapabilities endpoint and parse the information on that page
#' about the layers. For each layer, this function will retrieve the layer name
#' (as to be specified elsewhere programmatically), the title (human readable),
#' the abstract, which describes the data in the layer, the dimension name and
#' dimension range for specifying specific date values from the layer.
#'
#' Information about the layers can also be viewed at the getCapbilities page
#' directly: <https://geoserver.usanpn.org/geoserver/wms?request=GetCapabilities>
#'
#'
#' @returns A tibble containing all layer details as specified in function
#'   description.
#' @export
#' @examples \dontrun{
#' layers <- npn_get_layer_details()
#' }
npn_get_layer_details <- function() {
  #TODO handle http errors with httr2 instead of tryCatch()
  tryCatch({
    req <- base_req_geoserver %>%
      httr2::req_url_path_append("ows") %>%
      httr2::req_url_query(service = "wms",
                           version = "1.3.0",
                           request = "GetCapabilities")
    resp <- httr2::req_perform(req)
    out <- httr2::resp_body_xml(resp, encoding = "UTF-8")

    capability_list <- xml2::as_list(out)[[1]][["Capability"]]
    layer_list <- capability_list[["Layer"]]
    layers <- layer_list[names(layer_list) == "Layer"]

    # pulls out and flattens a layer by name while ensuring all NULLs and empty
    # vectors get replaced with NA instead of being dropped silently
    unnest_layer <- function(layers, name) {
      name_list <-
        lapply(layers, function(x) {
          x[[name]] %|||% NA_character_ #replace NULL and length 0 vectors with NA
        })
      unlist(name_list) %|||% NA_character_
    }

    name.vector <- unnest_layer(layers, "Name")
    title.vector <- unnest_layer(layers, "Title")
    abstract.vector <- unnest_layer(layers, "Abstract")
    dimension.range.vector <- unnest_layer(layers, "Dimension")

    dimension.name.vector <- unlist(lapply(layers, function(x) {
      attr(x[["Dimension"]], "name") %|||% NA_character_
    }))

    out <- tibble::tibble(
      name = name.vector,
      title = title.vector,
      abstract = abstract.vector,
      dimension.name = dimension.name.vector,
      dimension.range = dimension.range.vector
    )
    return(out)
  }, error = function(msg) {
    message("Geodata service not available. Please try again later")
    NULL
  })
}

#' Download Geospatial Data
#'
#' Function for directly downloading any arbitrary Geospatial layer data from
#' the NPN Geospatial web services.
#'
#' Information about the layers can also be viewed at the getCapbilities page
#' directly: <https://geoserver.usanpn.org/geoserver/wms?request=GetCapabilities>
#'
#' @param coverage_id The coverage id (machine name) of the layer for which to
#'   retrieve. Applicable values can be found via the [npn_get_layer_details()]
#'   function under the `name` column.
#' @param date Specify the date param for the layer retrieved. This can be a
#'   calendar date formatted YYYY-mm-dd or it could be a string integer
#'   representing day of year. It can also be `NULL` in some cases. Which to use
#'   depends entirely on the layer being requested. More information available
#'   from the [npn_get_layer_details()] function.
#' @param format The output format of the raster layer retrieved. Defaults to
#'   `"GeoTIFF"`.
#' @param output_path Optional value. When set, the raster will be piped to the
#'   file path specified. When left unset, this function will return a
#'   [terra::SpatRaster] object.
#' @returns returns nothing when `output_path` is set, otherwise a
#'   [terra::SpatRaster] object meeting the `coverage_id`, `date` and `format`
#'   parameters specified.
#' @examples \dontrun{
#' ras <- npn_download_geospatial("si-x:30yr_avg_six_bloom", "255")
#' }
#' @export
npn_download_geospatial <- function (coverage_id,
                                     date,
                                     format = "geotiff",
                                     output_path = NULL) {

  #logic to handle `date` being possibly a date or possible an integer DOY
  if (!is.null(date) && toString(date) != "") {
    param <- tryCatch({
      as.Date(date)
      paste0("time(\"", date, "T00:00:00.000Z\")")
    }, error = function(msg) {
      paste0("elevation(", date, ")")
    })
  } else {
    param <- NULL
  }
  req <- base_req_geoserver %>%
    httr2::req_url_path_append("wcs") %>%
    httr2::req_url_query(
      service = "WCS",
      version = "2.0.1",
      request = "GetCoverage",
      format = format,
      coverageId = coverage_id,
      SUBSET = param
    )

  tryCatch({
    if (is.null(output_path)) {
      rlang::check_installed("terra", reason = "when `output_path` is `NULL`")
      z <- tempfile()
      resp <- httr2::req_perform(req, path = z)
      ras <- terra::rast(z)
      return(ras)
    } else {
      resp <- httr2::req_perform(req, path = output_path)
      #TODO return output_path?
    }
  }, error = function(msg) { #TODO use httr2 for error handling
    message(
      "There was an issue downloading data from the Geoservice. It's possible the server is temporarily down. Please try again later."
    )
  })
}


#' Get AGDD Point Value
#'
#' This function is for requesting AGDD point values. Because the NPN has a
#' separate data service that can provide AGDD values which is more accurate
#' than Geoserver this function is ideal when requested AGDD point values.
#'
#' As this function only works for AGDD point values, if it's necessary to
#' retrieve point values for other layers please try the [npn_get_point_data()]
#' function.
#'
#' @param layer The name of the queried layer.
#' @param lat The latitude of the queried point.
#' @param long The longitude of the queried point.
#' @param date The queried date.
#' @param store_data Boolean value. If set `TRUE` then the value retrieved will
#'   be stored in a global variable named `point_values` for later use.
#' @returns Returns a numeric value of the AGDD value at the specified
#'   lat/long/date. If no value can be retrieved, then `-9999` is returned.
#' @export
#' @examples \dontrun{
#' npn_get_agdd_point_data(
#'   layer = "gdd:agdd",
#'   lat = 32.4,
#'   long = -110,
#'   date = "2020-01-15"
#' )
#' }
#'
npn_get_agdd_point_data <- function(
    layer,
    lat,
    long,
    date,
    store_data = TRUE) {
  # If we already have this value stored in global memory then
  # pull it from there.
  cached_value <- npn_check_point_cached(layer, lat, long, date)
  if (!is.null(cached_value)) {
    return(cached_value$value)
  }
  tryCatch({
    req <- base_req %>%
      httr2::req_url_path_append("stations/getTimeSeries.json") %>%
      httr2::req_url_query(
        latitude = lat,
        longitude = long,
        start_date = as.Date(date) - 1,
        end_date = date,
        layer = layer
      )
    resp <- httr2::req_perform(req)
  }, error = function(msg) { #TODO: use httr2 to handle errors
    message(
      "Unable to download AGDD data. The service is temporarily down, please try again later."
    )
    return(NULL)
  })

  # If the server returns an error then in that case, just return the -9999
  # value.
  json_data <- tryCatch({ #TODO use httr2 to handle errors
    httr2::resp_body_json(resp, simplifyVector = TRUE)
  }, error = function(msg) {
    message("Unable to parse server response. Please try again later.")
    return(-9999)
  })

  # If the server returns an unexpected value, also return -9999.
  v <- tryCatch({
    as.numeric(json_data[json_data$date == date, "point_value"])
  }, error = function(msg) {
    message("Unable to parse server response. Please try again later.")
    return(-9999)
  })

  # Once the value is known, then cache it in global memory so the script
  # doesn't try to ask for the same data point more than once.
  #
  # TODO: Break this into it's own function or possibly cache the whole response
  # with `httr2::req_cache()`
  if (isTRUE(store_data)) {
    if (is.null(pkg.env$point_values)) {
      pkg.env$point_values <- data.frame(
        layer = layer,
        lat = lat,
        long = long,
        date = date,
        value = v
      )
    } else {
      pkg.env$point_values <- rbind(
        pkg.env$point_values,
        data.frame(
          layer = layer,
          lat = lat,
          long = long,
          date = date,
          value = v
        )
      )
    }
  }
  return(v)
}


#' Get Point Data Value
#'
#' This function can get point data about any of the NPN geospatial layers.
#'
#' Please note that this function pulls this from the NPN's WCS service so the
#' data may not be totally precise. If you need precise AGDD values try using
#' the [npn_get_agdd_point_data()] function.
#' @param layer The coverage id (machine name) of the layer for which to
#'   retrieve. Applicable values can be found via the [npn_get_layer_details()]
#'   function under the `name` column.
#' @param lat The latitude of the point.
#' @param long The longitude of the point.
#' @param date The date for which to get a value.
#' @param store_data Boolean value. If set `TRUE` then the value retrieved will
#'   be stored in a global variable named `point_values` for later use.
#' @returns Returns a numeric value for any NPN geospatial data layer at the
#'   specified lat/long/date. If no value can be retrieved, then `-9999` is
#'   returned.
#' @export
#' @examples \dontrun{
#' value <-
#'   npn_get_point_data(
#'     layer = "gdd:agdd",
#'     lat = 38.8,
#'     long = -110.5,
#'     date = "2022-05-05"
#'   )
#' }
npn_get_point_data <- function(layer, lat, long, date, store_data = TRUE) {
  #TODO cached value is data frame, not numeric
  cached_value <- npn_check_point_cached(layer, lat, long, date)
  if (!is.null(cached_value)) {
    return(cached_value)
  }
  resp <- tryCatch({
    req <- base_req_geoserver %>%
      httr2::req_url_path_append("wcs") %>%
      httr2::req_url_query(
        service = "WCS",
        version = "2.0.1",
        request = "GetCoverage",
        coverageId = layer,
        format = "application/gml+xml",
        subset = paste0("http://www.opengis.net/def/axis/OGC/0/Long(", long, ")"),
        subset = paste0("http://www.opengis.net/def/axis/OGC/0/Lat(", lat, ")"),
        subset = paste0(
          "http://www.opengis.net/def/axis/OGC/0/time(\"",
          date,
          "T00:00:00.000Z\")"
        )
      ) %>%
      httr2::req_progress("down")
    httr2::req_perform(req)
  }, error = function(msg) {
    message("Geoserver is temporarily unavailable. Please try again later.")
    return(NULL)
  })
  #Download the data as XML and store it as an XML doc
  out <- httr2::resp_body_xml(resp)
  l <-
    xml2::xml_find_all(out,
                       "//gml:RectifiedGridCoverage/gml:rangeSet/gml:DataBlock/tupleList") %>% xml2::as_list()

  v <- as.numeric(unlist(l))

  if (store_data) {
    if (!is.null(pkg.env$point_values)) {
      pkg.env$point_values <-
        data.frame(
          layer = layer,
          lat = lat,
          long = long,
          date = date,
          value = v
        )
    } else {
      pkg.env$point_values <-
        rbind(pkg.env$point_values,
              data.frame(
                layer = layer,
                lat = lat,
                long = long,
                date = date,
                value = v
              ))
    }
  }
  return(v)
}


#' Resolve SIX Raster
#'
#' Utility function used to resolve the appropriate SI-x layer to use based on
#' the year being retrieved, the phenophase and sub-model being requested.
#'
#' If the year being requested is more than two years older than the current
#' year then use the prism based layers rather than the NCEP based layers. This
#' is because the PRISM data is not available in whole until midway through the
#' year after it was initially recorded. Hence, the 'safest' approach is to only
#' refer to the PRISM data when we knows for sure it's available in full, i.e.
#' two years prior.
#'
#' Sub-model and phenophase on the other hand are appended to the name of the
#' layer to request, no special logic is present in making the decision which
#' layer to retrieve based on those parameters.
#'
#' @param year String representation of the year being requested.
#' @param phenophase The SI-x phenophase being requested, `'leaf'` or `'bloom'`;
#'   defaults to `'leaf'`.
#' @param sub_model The SI-x sub model to use. Defaults to `NULL` (no
#'   sub-model).
#' @returns Returns a [terra::SpatRaster] object of the appropriate SI-x layer.
#' @keywords internal
resolve_six_raster <- function(year,
                               phenophase = "leaf",
                               sub_model = NULL) {
  current_year <- as.numeric(format(Sys.Date(), '%Y'))
  num_year <- as.numeric(year)
  src <- NULL
  date <- NULL

  if (num_year < current_year - 1) {
    src <- "prism"
    date <- paste0(year, "-01-01")
  } else {
    src <- "ncep"
    if (num_year != current_year) {
      date <- paste0(year, "-12-29")
    } else {
      date <- Sys.Date()
    }
  }

  if (is.null(sub_model)) {
    sub_model = "average"
  }

  if (is.null(phenophase) ||
      (phenophase != 'leaf'  && phenophase != 'bloom')) {
    phenophase = 'leaf'
  }

  layer_name = paste0("si-x:", sub_model, "_", phenophase, "_", src)

  raster <- npn_download_geospatial(layer_name, date, "tiff")
}

#' Merge Geo Data
#'
#' Utility function to intersect point based observational data with Geospatial
#' data values. This will take a data frame and append a new column to it.
#' @param ras Raster containing geospatial data
#' @param col_label The name of the column to append to the data frame
#' @param df The data frame which to append the new column of geospatial point
#'   values. For this function to work, `df` must contain two columns:
#'   `longitude`, and `latitude`.
#' @returns The data frame, now appended with a new column for geospatial data
#'   numeric values.
#' @keywords internal
npn_merge_geo_data <- function(ras, col_label, df) {
  rlang::check_installed("terra")
  # Convert the lat/long coordinates, presumed present in the input data frame
  # into coordinate objects
  coords <- data.frame(lon = df[, "longitude"], lat = df[, "latitude"])

  # Use the raster library's extract function to pull out the relevant
  # geospatial values, then add them to the the data frame as a new column.
  values <- terra::extract(x = ras, y = coords, ID = FALSE)
  names(values) <- col_label
  out <- dplyr::bind_cols(df, values)
  return(out)
}


resolve_agdd_raster <- function(agdd_layer) {
  if (!is.null(agdd_layer)) {
    if (agdd_layer == 32) {
      agdd_layer <- "gdd:agdd"
    } else if (agdd_layer == 50) {
      agdd_layer <- "gdd:agdd_50f"
    }
  } #TODO explicitly return something
}


#' Check Point Cached
#'
#' Checks in the global variable "point values" to see if the exact data point
#' being requested has already been asked for and returns the value if it's
#' already saved.
#' @param layer The name of the queried layer.
#' @param lat The latitude of the queried point.
#' @param long The longitude of the queried point.
#' @param date The queried date.
#' @returns The numeric value of the cell located at the specified coordinates
#'   and date if the value has been queried, otherwise `NULL`.
#' @keywords internal
npn_check_point_cached <- function(layer, lat, long, date) {
  val = NULL
  if (!is.null(pkg.env$point_values)) {
    val <- pkg.env$point_values[pkg.env$point_values$layer == layer &
                                  pkg.env$point_values$lat == lat &
                                  pkg.env$point_values$long == long &
                                  pkg.env$point_values$date == date, ]['value']
    if (!is.null(val) && nrow(val) == 0) {
      val <- NULL
    }
  }
  return(val)
}


#' Get Additional Layers
#'
#' Utility function to easily take arbitrary layer name parameters as a data
#' frame and return the raster data from NPN Geospatial data services.
#' @param data Data frame with first column named `name` and containing the
#'   names of the layer for which to retrieve data and the second column named
#'   `param` and containing string representations of the time/elevation subset
#'   parameter to pass.
#' @returns Returns a data frame containing the raster objects related to the
#'   specified layers.
#' @keywords internal
get_additional_rasters <- function(data) {
  rasters <- apply(data, 1, function(df) {
    npn_download_geospatial(df['name'], df['param'])
  })
}


#' Get Custom AGDD Time Series
#'
#' This function takes a series of variables used in calculating AGDD and
#' returns an AGDD time series, based on start and end date, for a given
#' location in the continental US. This function leverages the USA-NPN geo web
#' services
#'
#' @param method Takes `"simple"` or `"double-sine"` as input. This is the AGDD
#'   calculation method to use for each data point. Simple refers to simple
#'   averaging.
#' @param start_date Date at which to begin the AGDD calculations.
#' @param end_date Date at which to end the AGDD calculations.
#' @param base_temp This is the lowest temperature for each day for it to be
#'   considered in the calculation.
#' @param upper_threshold This parameter is only applicable for the double-sine
#'   method. This sets the highest temperature to be considered in any given
#'   day's AGDD calculation.
#' @param climate_data_source Specified the climate data set to use. Takes
#'   either `"PRISM"` or `"NCEP"` as input.
#' @param temp_unit The unit of temperature to use in the calculation. Takes
#'   either `"Fahrenheit"` or `"Celsius"` as input.
#' @param lat The latitude of the location for which to calculate the time
#'   series.
#' @param long The longitude of the location for which to calculate the time
#'   series.
#' @returns A data frame containing the numeric AGDD values for each day for the
#'   specified time period/location/method/base temp/data source.
#'
#' @export
#' @examples \dontrun{
#' res <- npn_get_custom_agdd_time_series(
#'   method = "double-sine",
#'   start_date = "2019-01-01",
#'   end_date = "2019-01-15",
#'   base_temp = 25,
#'   climate_data_source = "NCEP",
#'   temp_unit = "fahrenheit",
#'   lat = 39.7,
#'   long = -107.5,
#'   upper_threshold = 90
#' )
#' }
#'
npn_get_custom_agdd_time_series <- function(method,
                                            start_date,
                                            end_date,
                                            base_temp,
                                            climate_data_source,
                                            temp_unit,
                                            lat,
                                            long,
                                            upper_threshold = NULL) {
  climate_data_source <- toupper(climate_data_source)
  temp_unit <- tolower(temp_unit)
  method <- tolower(method)
  req <- base_req_geoservices %>%
    httr2::req_url_path_append("agdd", method, "pointTimeSeries") %>%
    httr2::req_url_query(
      climateProvider = climate_data_source,
      temperatureUnit = temp_unit,
      startDate = start_date,
      endDate = end_date,
      latitude = lat,
      longitude = long
    ) %>%
    httr2::req_progress(type = "down")

  if (method == "simple") {
    req <- req %>%
      httr2::req_url_query(base = base_temp)
  } else {
    req <- req %>%
      httr2::req_url_query(
        lowerThreshold = base_temp,
        upperThreshold = upper_threshold
      )
  }

  tryCatch({ #TODO use httr2 to handle errors
    resp <- httr2::req_perform(req)
  }, error = function(msg) {
    message("Service is temporarily unavailable. Please try again later.")
    return(NULL)
  })
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)$timeSeries
  return(tibble::as_tibble(out))
}

#' Get Custom AGDD Raster Map
#'
#' This function takes a series of variables used in calculating AGDD and
#' returns a raster of the continental USA with each pixel representing the
#' calculated AGDD value based on start and end date. This function leverages
#' the USA-NPN geo web services.
#' @param method Takes `"simple"` or `"double-sine"` as input. This is the AGDD
#'   calculation method to use for each data point. Simple refers to simple
#'   averaging.
#' @param start_date Date at which to begin the AGDD calculations.
#' @param end_date Date at which to end the AGDD calculations.
#' @param base_temp This is the lowest temperature for each day  for it to be
#'   considered in the calculation.
#' @param upper_threshold This parameter is only applicable for the double-sine
#'   method. This sets the highest temperature to be considered in any given
#'   day's AGDD calculation.
#' @param climate_data_source Specified the climate data set to use. Takes
#'   either `"PRISM"` or `"NCEP"` as input.
#' @param temp_unit The unit of temperature to use in the calculation. Takes
#'   either `"Fahrenheit"` or `"Celsius"` as input.
#' @returns A [terra::SpatRaster] object of each calculated AGDD numeric values
#'   based on specified time period/method/base temp/data source.
#' @export
#' @examples \dontrun{
#' res <- npn_get_custom_agdd_raster(
#'   method = "simple",
#'   climate_data_source = "NCEP",
#'   temp_unit = "Fahrenheit",
#'   start_date = "2020-01-01",
#'   end_date = "2020-01-15",
#'   base_temp = 32
#' )
#' }
npn_get_custom_agdd_raster <- function(method,
                                       climate_data_source,
                                       temp_unit,
                                       start_date,
                                       end_date,
                                       base_temp,
                                       upper_threshold = NULL) {
  rlang::check_installed("terra")
  climate_data_source <- toupper(climate_data_source)
  temp_unit <- tolower(temp_unit)
  method <- tolower(method)

  req <- base_req_geoservices %>%
    httr2::req_url_path_append("agdd", method, "map") %>%
    httr2::req_progress(type = "down") %>% # doesn't actually work because downloaded json is small despite taking a long time to generate on server
    httr2::req_url_query(
      climateProvider = climate_data_source,
      temperatureUnit = temp_unit,
      startDate = start_date,
      endDate = end_date
    )

  if (method == "simple") {
    req <- req %>%
      httr2::req_url_query(
        base = base_temp
      )
  } else {
    req <- req %>%
      httr2::req_url_query(
        lowerThreshold = base_temp,
        upperThreshold = upper_threshold
      )
  }

  tryCatch({ #TODO handle errors with httr2 instead
    resp <- httr2::req_perform(req)
  }, error = function(msg) {
    message("Data service is currently unavailable, please try again later.")
    return(NULL)
  })

  mapURL <-
    httr2::resp_body_json(resp)$mapUrl

  if (!is.null(mapURL)) {
    z <- tempfile()
    httr2::request(mapURL) %>%
      httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)") %>%
      httr2::req_perform(path = z)

    # "Discarded datum" seems to be a PROJ error that pops up in raster and
    # terra. Not sure if wrapping in withCallingHandlers() is still necessary
    # after migrating to terra, but I'll leave it in just in case.
    ras <- withCallingHandlers(
      terra::rast(z),
      warning = function(w) {
        if (any(grepl("Discarded datum", w))) {
          invokeRestart("muffleWarning")
        }
      }
    )
  }
  return(ras)
}
