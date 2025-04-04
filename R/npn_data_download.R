#' Download Status and Intensity Records
#'
#' This function allows for a parameterized search of all status records in the
#' USA-NPN database, returning all records as per the search parameters in a
#' data table. Data fetched from NPN services is returned as raw JSON before
#' being channeled into a data table. Optionally results can be directed to an
#' output file in which case the raw JSON is converted to CSV and saved to file;
#' in that case, data is also streamed to file which allows for more easily
#' handling of the data if the search otherwise returns more data than can be
#' handled at once in memory.
#'
#' Most search parameters are optional. However, users are encouraged to supply
#' additional search parameters to get results that are easier to work with.
#' `request_source` must be provided. This is a self-identifying string, telling
#' the service who is asking for the data or from where the request is being
#' made. It is recommended you provide your name or organization name. If the
#' call to this function is acting as an intermediary for a client, then you may
#' also optionally provide a user email and/or IP address for usage data
#' reporting later.
#'
#' Additional fields provides the ability to specify more, non-critical fields
#' to include in the search results. A complete list of additional fields can be
#' found in the NPN service's [companion
#' documentation](https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.w0nctgedhaop).
#' Metadata on all fields can be found in the following Excel sheet:
#' <https://www.usanpn.org/files/metadata/status_intensity_datafield_descriptions.xlsx>
#'
#' @param request_source Required field, character Self-identify who is making
#'   requests to the data service.
#' @param years Required field, character vector. Specify the years to include
#'   in the search, e.g. `c('2013','2014')`. You must specify at least one year.
#' @param coords Numeric vector, used to specify a bounding box as a search
#'   parameter, e.g. `c(lower_left_lat, lower_left_long, upper_right,
#'   lat,upper_right_long)`.
#' @param species_ids Integer vector of unique IDs for searching based on
#'   species, e.g. `c(3, 34, 35)`.
#' @param genus_ids Integer vector of unique IDs for searching based on
#'   taxonomic family, e.g. `c(3, 34, 35)`. This parameter will take precedence
#'   if `species_ids` is also set.
#' @param family_ids Integer vector of unique IDs for searching based on
#'   taxonomic family, e.g. `c(3, 34, 35)`. This parameter will take precedence
#'   if `species_ids` is also set.
#' @param order_ids Integer vector of unique IDs for searching based on
#'   taxonomic order, e.g. `c(3, 34, 35)`. This parameter will take precedence
#'   if `species_ids` or `family_ids` are also set.
#' @param class_ids Integer vector of unique IDs for searching based on
#'   taxonomic class, e.g. `c(3, 34, 35)`. This parameter will take precedence
#'   if `species_ids`, `family_ids` or `order_ids` are also set.
#' @param station_ids Integer vector of unique IDs for searching based on site
#'   location, e.g. `c(5, 9)`.
#' @param species_types Character vector of unique species type names for
#'   searching based on species types, e.g. `c("Deciduous", "Evergreen")`.
#' @param network_ids Integer vector of unique IDs for searching based on
#'   partner group/network, e.g. `c(500, 300)`.
#' @param states Character vector of US postal states to be used as search
#'   params, e.g. `c("AZ", "IL")`.
#' @param phenophase_ids Integer vector of unique IDs for searching based on
#'   phenophase, e.g. `c(323, 324)`.
#' @param functional_types Character vector of unique functional type names,
#'   e.g. `c("Birds").
#' @param additional_fields Character vector of additional fields to be included
#'   in the search results, e.g. `c("Station_Name", "Plant_Nickname")`.
#' @param climate_data Boolean value indicating that all climate variables
#'   should be included in `additional_fields`.
#' @param ip_address Optional field, string. IP Address of user requesting data.
#'   Used for generating data reports.
#' @param dataset_ids Integer vector of unique IDs for searching based on
#'   dataset, e.g. NEON or GRSM `c(17,15)`.
#' @param email Optional field, string. Email of user requesting data.
#' @inheritParams npn_get_data_by_year
#' @param pheno_class_ids Integer vector of unique IDs for searching based on
#'   pheno class. Note that if both `pheno_class_id` and `phenophase_id` are
#'   provided in the same request, `phenophase_id` will be ignored.
#' @param wkt WKT geometry by which filter data. Specifying a valid WKT within
#'   the contiguous US will filter data based on the locations which fall within
#'   that WKT.
#' @returns A tibble of all status records returned as per the search
#'   parameters. If `download_path` is specified, the file path is returned
#'   instead.
#' @export
#' @examples \dontrun{
#' #Download all saguaro data for 2016
#' npn_download_status_data(
#'   request_source = "Your Name or Org Here",
#'   years = c(2016),
#'   species_id = c(210),
#'   download_path = "saguaro_data_2016.csv"
#' )
#' }
npn_download_status_data = function(
  request_source,
  years,
  coords = NULL,
  species_ids = NULL,
  genus_ids = NULL,
  family_ids = NULL,
  order_ids = NULL,
  class_ids = NULL,
  station_ids = NULL,
  species_types = NULL,
  network_ids = NULL,
  states = NULL,
  phenophase_ids = NULL,
  functional_types = NULL,
  additional_fields = NULL,
  climate_data = FALSE,
  ip_address = NULL,
  dataset_ids = NULL,
  email = NULL,
  download_path = NULL,
  six_leaf_layer = FALSE,
  six_bloom_layer = FALSE,
  agdd_layer = NULL,
  six_sub_model = NULL,
  additional_layers = NULL,
  pheno_class_ids = NULL,
  wkt = NULL
) {
  query <- npn_get_common_query_vars(
    request_source,
    coords,
    species_ids,
    station_ids,
    species_types,
    network_ids,
    states,
    phenophase_ids,
    functional_types,
    additional_fields,
    climate_data,
    ip_address,
    dataset_ids,
    genus_ids,
    family_ids,
    order_ids,
    class_ids,
    pheno_class_ids,
    taxonomy_aggregate = NULL,
    pheno_class_aggregate = NULL,
    wkt,
    email
  )

  years <- sort(unlist(years))
  res <- npn_get_data_by_year(
    endpoint = "/observations/getObservations.ndjson",
    query = query,
    years = years,
    download_path = download_path,
    six_leaf_layer = six_leaf_layer,
    six_bloom_layer = six_bloom_layer,
    agdd_layer = agdd_layer,
    six_sub_model = six_sub_model,
    additional_layers = additional_layers
  )

  return(res)
}


#' Download Individual Phenometrics
#'
#' This function allows for a parameterized search of all individual
#' phenometrics records in the USA-NPN database, returning all records as per
#' the search parameters in a data table. Data fetched from NPN services is
#' returned as raw JSON before being channeled into a data table. Optionally
#' results can be directed to an output file in which case raw JSON is converted
#' to CSV and saved to file; in that case, data is also streamed to file which
#' allows for more easily handling of the data if the search otherwise returns
#' more data than can be handled at once in memory.
#'
#' This data type includes estimates of the dates of phenophase onsets and ends
#' for individual plants and for animal species at a site during a user-defined
#' time period. Each row represents a series of consecutive "yes" phenophase
#' status records, beginning with the date of the first "yes" and ending with
#' the date of the last "yes", submitted for a given phenophase on a given
#' organism. Note that more than one consecutive series for an organism may be
#' present within a single growing season or year.
#'
#' Most search parameters are optional, however, users are encouraged to supply
#' additional search parameters to get results that are easier to work with.
#' `request_source` must be provided. This is a self-identifying string, telling
#' the service who is asking for the data or from where the request is being
#' made. It is recommended you provide your name or organization name. If the
#' call to this function is acting as an intermediary for a client, then you may
#' also optionally provide a user email and/or IP address for usage data
#' reporting later.
#'
#' Additional fields provides the ability to specify additional, non-critical
#' fields to include in the search results. A complete list of additional fields
#' can be found in the NPN service's [companion
#' documentation](https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.7yy4i3278v7u)
#' Metadata on all fields can be found in the following Excel sheet:
#' <https://www.usanpn.org/files/metadata/individual_phenometrics_datafield_descriptions.xlsx>
#' @inheritParams npn_download_status_data
#' @inheritParams npn_get_data_by_year
#' @param individual_ids Comma-separated string of unique IDs for individual
#'   plants/animal species by which to filter the data.
#' @returns A tibble of all status records returned as per the search
#'   parameters. If `download_path` is specified, the file path is returned
#'   instead.
#' @export
#' @examples \dontrun{
#' #Download all saguaro data for 2013 and 2014 using "water year" as the period
#' npn_download_individual_phenometrics(
#'   request_source = "Your Name or Org Here",
#'   years = c(2013, 2014),
#'   period_start = "10-01",
#'   period_end = "09-30",
#'   species_id = 210,
#'   download_path = "saguaro_data_2013_2014.csv"
#' )
#' }
npn_download_individual_phenometrics <- function(
  request_source,
  years,
  period_start = "01-01",
  period_end = "12-31",
  coords = NULL,
  individual_ids = NULL,
  species_ids = NULL,
  station_ids = NULL,
  species_types = NULL,
  network_ids = NULL,
  states = NULL,
  phenophase_ids = NULL,
  functional_types = NULL,
  additional_fields = NULL,
  climate_data = FALSE,
  ip_address = NULL,
  dataset_ids = NULL,
  genus_ids = NULL,
  family_ids = NULL,
  order_ids = NULL,
  class_ids = NULL,
  pheno_class_ids = NULL,
  email = NULL,
  download_path = NULL,
  six_leaf_layer = FALSE,
  six_bloom_layer = FALSE,
  agdd_layer = NULL,
  six_sub_model = NULL,
  additional_layers = NULL,
  wkt = NULL
) {
  query <- npn_get_common_query_vars(
    request_source = request_source,
    coords = coords,
    species_ids = species_ids,
    station_ids = station_ids,
    species_types = species_types,
    network_ids = network_ids,
    states = states,
    phenophase_ids = phenophase_ids,
    functional_types = functional_types,
    additional_fields = additional_fields,
    climate_data = climate_data,
    ip_address = ip_address,
    dataset_ids = dataset_ids,
    genus_ids = genus_ids,
    family_ids = family_ids,
    order_ids = order_ids,
    class_ids = class_ids,
    pheno_class_ids = pheno_class_ids,
    taxonomy_aggregate = NULL,
    pheno_class_aggregate = NULL,
    wkt = wkt,
    email = email
  )

  if (!is.null(individual_ids)) {
    query["individual_ids"] <- individual_ids
  }

  return(
    npn_get_data_by_year(
      "/observations/getSummarizedData.ndjson",
      query = query,
      years = years,
      period_start = period_start,
      period_end = period_end,
      download_path = download_path,
      six_leaf_layer = six_leaf_layer,
      six_bloom_layer = six_bloom_layer,
      agdd_layer = agdd_layer,
      six_sub_model = six_sub_model,
      additional_layers = additional_layers
    )
  )
}


#' Download Site Phenometrics
#'
#' This function allows for a parameterized search of all site phenometrics
#' records in the USA-NPN database, returning all records as per the search
#' parameters in a data table. Data fetched from NPN services is returned as raw
#' JSON before being channeled into a data table. Optionally results can be
#' directed to an output file in which case raw JSON is converted to CSV and
#' saved to file; in that case, data is also streamed to file which allows for
#' more easily handling of the data if the search otherwise returns more data
#' than can be handled at once in memory.
#'
#' This data type includes estimates of the overall onset and end of phenophase
#' activity for plant and animal species at a site over a user-defined time
#' period. Each row provides the first and last occurrences of a given
#' phenophase on a given species, beginning with the date of the first observed
#' "yes" phenophase status record and ending with the date of the last observed
#' "yes" record of the user-defined time period. For plant species where
#' multiple individuals are monitored at the site, the date provided for "first
#' yes" is the mean of the first "yes" records for each individual plant at the
#' site, and the date for "last yes" is the mean of the last "yes" records. Note
#' that a phenophase may have ended and restarted during the overall period of
#' its activity at the site. These more fine-scale patterns can be explored in
#' the individual phenometrics data.
#'
#' Most search parameters are optional, however, users are encouraged to supply
#' additional search parameters to get results that are easier to work with.
#' `request_source` must be provided. This is a self-identifying string, telling
#' the service who is asking for the data or from where the request is being
#' made. It is recommended you provide your name or organization name. If the
#' call to this function is acting as an intermediary for a client, then you may
#' also optionally provide a user email and/or IP address for usage data
#' reporting later.
#'
#' Additional fields provides the ability to specify additional, non-critical
#' fields to include in the search results. A complete list of additional fields
#' can be found in the NPN service's [companion
#' documentation](https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.ueaexz9bczti).
#' Metadata on all fields can be found in the following Excel sheet:
#' <https://www.usanpn.org/files/metadata/site_phenometrics_datafield_descriptions.xlsx>
#'
#' @inheritParams npn_download_status_data
#' @inheritParams npn_get_data_by_year
#' @param num_days_quality_filter Required field, defaults to `30`. The integer
#'   value sets the upper limit on the number of days difference between the
#'   first Y value and the previous N value for each individual to be included
#'   in the data aggregation.
#' @param taxonomy_aggregate Boolean value indicating whether to aggregate data
#'   by a taxonomic order higher than species. This will be based on the values
#'   set in `family_ids`, `order_ids`, or `class_ids`. If one of those three
#'   fields are not set, then this value is ignored.
#' @param pheno_class_aggregate Boolean value indicating whether to aggregate
#'   data by the pheno class ids as per the `pheno_class_ids` parameter. If the
#'   `pheno_class_ids` value is not set, then this parameter is ignored. This
#'   can be used in conjunction with `taxonomy_aggregate` and higher taxonomic
#'   level data filtering.
#' @returns A tibble of all status records returned as per the search
#'   parameters. If `download_path` is specified, the file path is returned
#'   instead.
#' @export
#' @examples \dontrun{
#' #Download all saguaro data for 2013 and 2014
#' npn_download_site_phenometrics(
#'   request_source = "Your Name or Org Here",
#'   years = c(2013, 2014),
#'   species_id = 210,
#'   download_path = "saguaro_data_2013_2014.csv"
#' )
#' }
npn_download_site_phenometrics <- function(
  request_source,
  years,
  period_start = "01-01",
  period_end = "12-31",
  num_days_quality_filter = "30",
  coords = NULL,
  species_ids = NULL,
  genus_ids = NULL,
  family_ids = NULL,
  order_ids = NULL,
  class_ids = NULL,
  pheno_class_ids = NULL,
  station_ids = NULL,
  species_types = NULL,
  network_ids = NULL,
  states = NULL,
  phenophase_ids = NULL,
  functional_types = NULL,
  additional_fields = NULL,
  climate_data = FALSE,
  ip_address = NULL,
  dataset_ids = NULL,
  email = NULL,
  download_path = NULL,
  six_leaf_layer = FALSE,
  six_bloom_layer = FALSE,
  agdd_layer = NULL,
  six_sub_model = NULL,
  additional_layers = NULL,
  taxonomy_aggregate = NULL,
  pheno_class_aggregate = NULL,
  wkt = NULL
) {
  query <- npn_get_common_query_vars(
    request_source = request_source,
    coords = coords,
    species_ids = species_ids,
    station_ids = station_ids,
    species_types = species_types,
    network_ids = network_ids,
    states = states,
    phenophase_ids = phenophase_ids,
    functional_types = functional_types,
    additional_fields = additional_fields,
    climate_data = climate_data,
    ip_address = ip_address,
    dataset_ids = dataset_ids,
    genus_ids = genus_ids,
    family_ids = family_ids,
    order_ids = order_ids,
    class_ids = class_ids,
    pheno_class_ids = pheno_class_ids,
    taxonomy_aggregate = taxonomy_aggregate,
    pheno_class_aggregate = pheno_class_aggregate,
    wkt = wkt,
    email = email
  )

  query["num_days_quality_filter"] <- num_days_quality_filter

  return(
    npn_get_data_by_year(
      endpoint = "/observations/getSiteLevelData.ndjson",
      query = query,
      years = years,
      period_start = period_start,
      period_end = period_end,
      download_path = download_path,
      six_leaf_layer = six_leaf_layer,
      six_bloom_layer = six_bloom_layer,
      agdd_layer = agdd_layer,
      six_sub_model = six_sub_model,
      additional_layers = additional_layers
    )
  )
}


#' Download Magnitude Phenometrics
#'
#' This function allows for a parameterized search of all magnitude phenometrics
#' in the USA-NPN database, returning all records as per the search results in a
#' data table. Data fetched from NPN services is returned as raw JSON before
#' being channeled into a data table. Optionally results can be directed to an
#' output file in which case raw JSON is saved to file; in that case, data is
#' also streamed to file which allows for more easily handling of the data if
#' the search otherwise returns more data than can be handled at once in memory.
#'
#' This data type includes various measures of the extent to which a phenophase
#' for a plant or animal species is expressed across multiple individuals and
#' sites over a user-selected set of time intervals. Each row provides up to
#' eight calculated measures summarized weekly, bi-weekly, monthly or over a
#' custom time interval. These measures include approaches to evaluate the shape
#' of an annual activity curve, including the total number of "yes" records and
#' the proportion of "yes" records relative to the total number of status
#' records over the course of a calendar year for a region of interest. They
#' also include several approaches for standardizing animal abundances by
#' observer effort over time and space (e.g. mean active bird individuals per
#' hour). See the Metadata window for more information.
#'
#' Most search parameters are optional, however, failing to provide even a
#' single search parameter will return all results in the database.
#' Request_Source must be provided. This is a self-identifying string, telling
#' the service who is asking for the data or from where the request is being
#' made. It is recommended you provide your name or organization name. If the
#' call to this function is acting as an intermediary for a client, then you may
#' also optionally provide a user email and/or IP address for usage data
#' reporting later.
#'
#' Additional fields provides the ability to specify more, non-critical fields
#' to include in the search results. A complete list of additional fields can be
#' found in the NPN service's [companion documentation](https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.df3zspopwq98).
#' Metadata on all fields can be found in the following Excel sheet:
#' <https://www.usanpn.org/files/metadata/magnitude_phenometrics_datafield_descriptions.xlsx>
#'
#' @inheritParams npn_download_status_data
#' @param period_frequency Required field, integer. The integer value specifies
#'   the number of days by which to delineate the period of time specified by
#'   the start_date and end_date, i.e. a value of 7 will delineate the period of
#'   time weekly. Any remainder days are grouped into the final delineation.
#'   This parameter, while typically an int, also allows for a "special" string
#'   value, "months" to be passed in. Specifying this parameter as "months" will
#'   delineate the period of time by the calendar months regardless of how many
#'   days are in each month. Defaults to `30` if omitted.
#' @inheritParams npn_download_site_phenometrics
#' @returns A tibble of the requested data. If a `download_path` was specified,
#'   the file path is returned.
#' @export
#' @examples \dontrun{
#' #Download book all saguaro data for 2013
#' npn_download_magnitude_phenometrics(
#'   request_source="Your Name or Org Here",
#'   years=c(2013),
#'   species_id=c(210),
#'   download_path="saguaro_data_2013.csv"
#' )
#' }
npn_download_magnitude_phenometrics <- function(
  request_source,
  years,
  period_frequency = "30",
  coords = NULL,
  species_ids = NULL,
  genus_ids = NULL,
  family_ids = NULL,
  order_ids = NULL,
  class_ids = NULL,
  pheno_class_ids = NULL,
  station_ids = NULL,
  species_types = NULL,
  network_ids = NULL,
  states = NULL,
  phenophase_ids = NULL,
  functional_types = NULL,
  additional_fields = NULL,
  climate_data = FALSE,
  ip_address = NULL,
  dataset_ids = NULL,
  email = NULL,
  download_path = NULL,
  taxonomy_aggregate = NULL,
  pheno_class_aggregate = NULL,
  wkt = NULL
) {
  query <- npn_get_common_query_vars(
    request_source,
    coords,
    species_ids,
    station_ids,
    species_types,
    network_ids,
    states,
    phenophase_ids,
    functional_types,
    additional_fields,
    climate_data,
    ip_address,
    dataset_ids,
    genus_ids,
    family_ids,
    order_ids,
    class_ids,
    pheno_class_ids,
    taxonomy_aggregate,
    pheno_class_aggregate,
    wkt,
    email
  )

  query["frequency"] <- period_frequency

  years <- sort(unlist(years))
  query['start_date'] <- paste0(years[1], "-01-01")
  query['end_date'] <- paste0(years[length(years)], "-12-31")

  message("Downloading...")
  data <- npn_get_data(
    endpoint = "/observations/getMagnitudeData.ndjson",
    query = query,
    download_path = download_path
  )
  return(data)
}


#' Get Data By Year
#'
#' Utility function to chain multiple requests to [npn_get_data] for requests
#' where data should only be retrieved on an annual basis, or otherwise
#' automatically be delineated in some way. Results in a data table that's a
#' combined set of the results from each request to the data service.
#'
#' @param endpoint String, the endpoint to query.
#' @param query Base query string to use. This includes all the user selected
#'   parameters but doesn't include start/end date which will be automatically
#'   generated and added.
#' @param years Integer vector; the years for which to retrieve data. There
#'   will be one request to the service for each year.  If the period
#'   (determined by `period_start` and `period_end`) crosses a year boundary,
#'   `years` determines the start years.
#' @param period_start,period_end Character vectors of the form "MM-DD". Used to
#'   determine the period over which phenophase status records are summarized.
#'   For example, to use a "water year" set `period_start = "10-01"` and
#'   `period_end = "09-30"`. If not provided, they will default to "01-01" and
#'   "12-31", respectively, to use the calendar year.
#' @param download_path Character, optional file path to the file for which to
#'   output the results.
#' @param six_leaf_layer Boolean value when set to `TRUE` will attempt to
#'   resolve the date of the observation to a spring index, leafing value for
#'   the location at which the observations was taken.
#' @param six_bloom_layer Boolean value when set to `TRUE` will attempt to
#'   resolve the date of the observation to a spring index, bloom value for the
#'   location at which the observations was taken.
#' @param agdd_layer Numeric value, accepts `32` or `50`. When set, the results
#'   will attempt to resolve the date of the observation to an AGDD value for
#'   the location; the `32` or `50` represents the base value of the AGDD value
#'   returned. All AGDD values are based on a January 1st start date of the year
#'   in which the observation was taken.
#' @param six_sub_model Affects the results of the six layers returned. Can be
#'   used to specify one of three submodels used to calculate the spring index
#'   values. Thus setting this field will change the results of `six_leaf_layer`
#'   and `six_bloom_layer`. Valid values include: `'lilac'`, `'zabelli'` and
#'   `'arnoldred'`. For more information see the NPN's Spring Index Maps
#'   documentation: <https://www.usanpn.org/data/maps/spring>.
#' @param additional_layers Data frame with first column named `name` and
#'   containing the names of the layer for which to retrieve data and the second
#'   column named `param` and containing string representations of the
#'   time/elevation subset parameter to use. This variable can be used to append
#'   additional geospatial layer data fields to the results, such that the date
#'   of observation in each row will resolve to a value from the specified
#'   layers, given the location of the observation.
#' @returns A tibble combining each requests results from the service. If
#'   `download_path` is specified, the file path is returned instead.
#' @keywords internal
#' @examples \dontrun{
#' endpoint <- "/observations/getObservations.json"
#' query <- list(
#'   request_src = "Unit%20Test",
#'   climate_data = "0",
#'   `species_id[1]` = "6"
#' )
#'
#' npn_get_data_by_year(endpoint = endpoint,
#'                      query = query,
#'                      years = c(2013, 2014))
#'
#' #Set a custom period from October through September
#' # This will return data for 2013-10-01 through 2014-09-30 and from 2014-10-01
#' # through 2015-09-30
#' npn_get_data_by_year(
#'   endpoint = endpoint,
#'   query = query,
#'   years = c(2013, 2014),
#'   period_start = "10-01",
#'   period_end = "09-30"
#' )
#' }
npn_get_data_by_year <- function(
  endpoint,
  query,
  years,
  period_start = "01-01",
  period_end = "12-31",
  download_path = NULL,
  six_leaf_layer = FALSE,
  six_bloom_layer = FALSE,
  agdd_layer = NULL,
  six_sub_model = NULL,
  additional_layers = NULL
) {
  #validate period start and end
  validate_mmdd(period_start)
  validate_mmdd(period_end)

  #coerce year to numeric if it was provided as legacy character vector
  years <- as.integer(years)

  all_data <- NULL
  first_year <- TRUE
  six_leaf_raster <- NULL
  six_bloom_raster <- NULL
  additional_rasters <- NULL
  if (length(years) > 0) {
    agdd_layer <- resolve_agdd_raster(agdd_layer)

    if (!is.null(additional_layers)) {
      additional_layers$raster <- get_additional_rasters(additional_layers)
    }
    for (year in years) {
      # This is where the start/end dates are automatically created
      # based on the input years.
      start_date <- as.Date(paste(year, period_start, sep = "-"))
      end_date <- as.Date(paste(year, period_end, sep = "-"))

      #assume if end_date is before start_date that it should actually be the period_end of the next year
      if (end_date < start_date) {
        end_date <- as.Date(paste(year + 1, period_end, sep = "-"))
      }
      query['start_date'] <- as.character(start_date)
      query['end_date'] <- as.character(end_date)

      if (isTRUE(six_leaf_layer)) {
        six_leaf_raster <-
          resolve_six_raster(
            year = year,
            phenophase = "leaf",
            sub_model = six_sub_model
          )
      }

      if (isTRUE(six_bloom_layer)) {
        six_bloom_raster <-
          resolve_six_raster(
            year = year,
            phenophase = "bloom",
            sub_model = six_sub_model
          )
      }

      # We also have to generate a unique URL on each request to account
      # for the changes in the start/end date
      message("Downloading...")
      data <- npn_get_data(
        endpoint = endpoint,
        query = query,
        download_path = download_path,
        always_append = !first_year,
        six_leaf_raster = six_leaf_raster,
        six_bloom_raster = six_bloom_raster,
        agdd_layer = agdd_layer,
        additional_layers = additional_layers
      )
      # First if statement checks whether this is the results returned is empty.
      # Second if statement checks if we've made a previous request that's
      # returned data. The data doesn't have to be combined if there was
      # no previous iteration / the results were empty
      if (!is.null(data) && is.null(download_path)) {
        if (!is.null(all_data)) {
          all_data <- dplyr::bind_rows(all_data, data)
        } else {
          all_data <- data
        }
      }
      if (!is.null(data)) {
        first_year <- FALSE
      }
    }
  }
  if (is.null(download_path)) {
    return(all_data)
  } else {
    return(normalizePath(download_path))
  }
}


#' Download NPN Data
#'
#' Generic utility function for querying data from the NPN data services.
#'
#' @param endpoint The endpoint to request data from starting at
#'   'https://services.usanpn.org/npn_portal/'. E.g.
#'   `"observations/getObservations.ndjson"`
#' @param download_path String, optional file path to the file for which to
#'   output the results.
#' @param always_append Boolean flag. When set to `TRUE`, then we always append
#'   data to the download path. This is used in the case of
#'   [npn_get_data_by_year()] where we're making multiple requests to the same
#'   service and aggregating all data results in a single file. Without this
#'   flag, otherwise, each call to the service would truncate the output file.
#'
#' @returns A tibble of the requested data. If a `download_path` was specified,
#'   the file path is returned.
#' @keywords internal
#' @examples \dontrun{
#' npn_get_data(
#'   endpoint = "observations/getObservations.ndjson",
#'   query = list(
#'     request_src = "Unit Test",
#'     climate_data = "0",
#'     `species_id[1]` = "6",
#'     start_date = "2010-01-01",
#'     end_date = "2010-12-31"
#'   )
#' )
#' }
npn_get_data <- function(
  endpoint,
  query,
  download_path = NULL,
  always_append = FALSE,
  six_leaf_raster = NULL,
  six_bloom_raster = NULL,
  agdd_layer = NULL,
  additional_layers = NULL
) {
  if (is.null(download_path)) {
    #use JSON
    endpoint <- sub("(?<=\\.)\\w+$", "json", endpoint, perl = TRUE)
  } else {
    #use NDJSON
    endpoint <- sub("(?<=\\.)\\w+$", "ndjson", endpoint, perl = TRUE)
  }

  req <- base_req %>%
    httr2::req_url_path_append(endpoint) %>%
    # httr2::req_progress(type = "down") %>% #doesn't work—only for file downloads
    httr2::req_method("POST") %>%
    httr2::req_body_form(!!!query)

  #define data wrangling function to be run on entire df or by chunks of 5000
  #rows
  wrangle_dl_data <- function(df) {
    df <- df %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.numeric),
          function(x) ifelse(x == -9999, NA_real_, x)
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(is.character),
          function(x) ifelse(x == "-9999", NA_character_, x)
        )
      ) %>%
      #handle some columns that may be read in as the wrong type if they happen
      #to be all NAs (#87). `any_of()` is used because not all endpoints will
      #return these columns!
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of("update_datetime"),
          function(x) as.POSIXct(x, tz = "UTC")
        ),
        dplyr::across(
          dplyr::any_of(c("intensity_value", "abundance_value")),
          as.character
        )
      )

    # Reconcile all the points in the frame with the SIX leaf raster,
    # if it's been requested.
    if (!is.null(six_leaf_raster)) {
      df <- npn_merge_geo_data(six_leaf_raster, "SI-x_Leaf_Value", df)
    }

    # Reconcile all the points in the frame with the SIX bloom raster,
    # if it's been requested.
    if (!is.null(six_bloom_raster)) {
      df <- npn_merge_geo_data(six_bloom_raster, "SI-x_Bloom_Value", df)
    }

    if (!is.null(additional_layers)) {
      for (j in rownames(additional_layers)) {
        df <- npn_merge_geo_data(
          additional_layers[j, ][['raster']][[1]],
          as.character(additional_layers[j, ][['name']][[1]]),
          df
        )
      }
    }

    # Reconcile the AGDD point values with the data points if that
    # was requested.
    if (!is.null(agdd_layer)) {
      date_col <- NULL

      if ("observation_date" %in% colnames(df)) {
        date_col <- "observation_date"
      } else if ("mean_first_yes_doy" %in% colnames(df)) {
        df$cal_date <-
          as.Date(
            df[, "mean_first_yes_doy"],
            origin = paste0(df[, "mean_first_yes_year"], "-01-01")
          ) -
          1
        date_col <- "cal_date"
      } else if ("first_yes_day" %in% colnames(df)) {
        df$cal_date <-
          as.Date(
            df[, "first_yes_doy"],
            origin = paste0(df[, "first_yes_year"], "-01-01")
          ) -
          1
        date_col <- "cal_date"
      }

      pt_values <-
        apply(df[, c('latitude', 'longitude', date_col)], 1, function(x) {
          rnpn::npn_get_agdd_point_data(
            layer = agdd_layer,
            lat = as.numeric(x['latitude']),
            long = as.numeric(x['longitude']),
            date = x[date_col]
          )
        })
      pt_values <-
        tibble::as_tibble_col(pt_values, column_name = agdd_layer)
      df <- cbind(df, pt_values)

      if ("cal_date" %in% colnames(df)) {
        df$cal_date <- NULL
      }
    }
    return(tibble::as_tibble(df))
  }
  path <- withr::local_tempfile()
  resp <- httr2::req_perform(req, path = path)

  # If no download_path specified, just wrangle the data all at once, otherwise
  # assume that memory could be a limitation and wrangle data 5000 rows at a
  # time and append to the CSV file specified in `download_path`
  if (is.null(download_path)) {
    dtm <-
      httr2::resp_body_json(resp, simplifyVector = TRUE) %>%
      wrangle_dl_data()
    return(dtm)
  } else {
    #resp$body is a path to an .ndjson file
    i <- 0
    resp$body %>%
      file() %>%
      jsonlite::stream_in(
        handler = function(df) {
          df <- wrangle_dl_data(df)
          if (nrow(df) > 0) {
            write.table(
              df,
              download_path,
              append = !(i == 0 && isFALSE(always_append)),
              sep = ",",
              eol = "\n",
              row.names = FALSE,
              col.names = i == 0 && isFALSE(always_append)
            )
            i <<- i + 1
          }
        },
        pagesize = 5000
      )
    # If the user asks for the data to be saved to file then
    # return the `download_path`
    return(normalizePath(download_path))
  }
}


#' Get Common Query String Variables
#'
#' Utility function to generate a list of query string variables for requests to
#' NPN data service points. Some parameters are basically present in all
#' requests, so this function helps put them together.
#'
#'
#' @return List of query string variables.
#' @keywords internal
npn_get_common_query_vars <- function(
  request_source,
  coords = NULL,
  species_ids = NULL,
  station_ids = NULL,
  species_types = NULL,
  network_ids = NULL,
  states = NULL,
  phenophase_ids = NULL,
  functional_types = NULL,
  additional_fields = NULL,
  climate_data = FALSE,
  ip_address = NULL,
  dataset_ids = NULL,
  genus_ids = NULL,
  family_ids = NULL,
  order_ids = NULL,
  class_ids = NULL,
  pheno_class_ids = NULL,
  taxonomy_aggregate = NULL,
  pheno_class_aggregate = NULL,
  wkt = NULL,
  email = NULL
) {
  if (missing(request_source) || is.null(request_source)) {
    rlang::abort("`request_source` is a required argument")
  }
  if (!is.null(family_ids)) {
    species_ids = NULL
    genus_ids = NULL
  }

  if (!is.null(class_ids)) {
    species_ids = NULL
    genus_ids = NULL
    family_ids = NULL
  }

  if (!is.null(order_ids)) {
    species_ids = NULL
    genus_ids = NULL
    family_ids = NULL
    class_ids = NULL
  }

  if (!is.null(genus_ids)) {
    species_ids = NULL
  }

  if (!is.null(wkt)) {
    station_ids_shape <- tryCatch(
      {
        shape_stations <- npn_stations_by_location(wkt)
        station_ids_shape <- shape_stations$station_id
      },
      error = function(msg) {
        print("Unable to filter by shape file.")
        print(msg)
      }
    )

    if (!is.null(station_ids)) {
      station_ids <- c(station_ids, station_ids_shape)
    } else {
      station_ids <- station_ids_shape
    }
  }

  query <- c(
    list(
      request_src = URLencode(request_source),
      #TODO change to something like
      # if(!is.null(climate_data)) climate_data <- as.integer(climate_data)
      # this *might* break things if it is important that climate_date = 0 always
      climate_data = (if (climate_data) "1" else "0")
    ),
    # All these variables take a multiplicity of possible parameters, this will help put them all together.
    npn_createArgList("species_id", species_ids),
    npn_createArgList("station_id", station_ids),
    npn_createArgList("species_type", species_types),
    npn_createArgList("network_id", network_ids),
    npn_createArgList("dataset_ids", dataset_ids),
    npn_createArgList("state", states),
    npn_createArgList("phenophase_id", phenophase_ids),
    npn_createArgList("functional_type", functional_types),
    npn_createArgList("additional_field", additional_fields),
    npn_createArgList("genus_id", genus_ids),
    npn_createArgList("family_id", family_ids),
    npn_createArgList("order_id", order_ids),
    npn_createArgList("class_id", class_ids),
    npn_createArgList("pheno_class_id", pheno_class_ids)
  )

  if (!is.null(coords) && length(coords) == 4) {
    if (is.numeric(coords)) {
      coords <- paste(coords)
    }
    query['bottom_left_x1'] <- coords[1]
    query['bottom_left_y1'] <- coords[2]
    query['upper_right_x2'] <- coords[3]
    query['upper_right_y2'] <- coords[4]
  }

  if (!is.null(ip_address)) {
    query['IP_Address'] <- ip_address
  }

  if (!is.null(email)) {
    query['user_email'] <- email
  }

  if (!is.null(taxonomy_aggregate) && taxonomy_aggregate) {
    query['taxonomy_aggregate'] <- "1"
  }

  if (!is.null(pheno_class_aggregate) && pheno_class_aggregate) {
    query['pheno_class_aggregate'] <- "1"
  }

  return(query)
}
