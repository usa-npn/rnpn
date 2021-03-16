
#'  Download Status and Intensity Records
#'
#'  This function allows for a parameterized search of all status records in the USA-NPN database, returning all records as per the search parameters in a data
#'  table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optionally results can be directed to an output file in
#'  which case the raw JSON is converted to CSV and saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
#'  returns more data than can be handled at once in memory.
#'
#'  Most search parameters are optional, however, users are encouraged to supply additional search parameters to get results that are easier to work with. Request_Source
#'  must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
#'  you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
#'  a user email and/or IP address for usage data reporting later.
#'
#'  Additional fields provides the ability to specify more, non-critical fields to include in the search results. A complete list of additional fields can be found in
#'  the NPN service's companion documentation
#'  https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.w0nctgedhaop
#'  Metadata on all fields can be found in the following Excel sheet:
#'  http://www.usanpn.org/files/metadata/status_intensity_datafield_descriptions.xlsx
#'
#' @param request_source Required field, string. Self-identify who is making requests to the data service
#' @param years Required field, list of strings. Specify the years to include in the search, e.g. c('2013','2014'). You must specify at least one year.
#' @param coords List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )
#' @param species_ids List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )
#' @param genus_ids List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.
#' @param family_ids List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.
#' @param order_ids List of unique IDs for searching based on taxonomic order, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids or family_ids are also set.
#' @param class_ids List of unique IDs for searching based on taxonomic class, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids, family_ids or order_ids are also set.
#' @param station_ids List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )
#' @param species_types List of unique species type names for searching based on species types, e.g. c ( "Deciduous", "Evergreen" )
#' @param network_ids List of unique IDs for searching based on partner group/network, e.g. ( 500, 300, ... )
#' @param states List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )
#' @param phenophase_ids List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )
#' @param functional_types List of unique functional type names, e.g. c ( "Birds"  )
#' @param additional_fields List of additional fields to be included in the search results, e.g. c( "Station_Name", "Plant_Nickname" )
#' @param climate_data Boolean value indicating that all climate variables should be included in additional_fields
#' @param ip_address Optional field, string. IP Address of user requesting data. Used for generating data reports
#' @param dataset_ids List of unique IDs for searching based on dataset, e.g. NEON or GRSM c(17,15)
#' @param email Optional field, string. Email of user requesting data.
#' @param download_path Optional file path to which search results should be re-directed for later use.
#' @param six_leaf_layer Boolean value when set to true will attempt to resolve the date of the observation to a spring index, leafing
#' value for the location at which the observations was taken
#' @param six_bloom_layer Boolean value when set to true will attempt to resolve the date of the observation to a spring index, bloom
#' value for the location at which the observations was taken
#' @param six_sub_model Affects the results of the six layers returned. Can be used to specify one of three submodels used to calculate
#' the spring index values. Thus setting this field will change the results of six_leaf_layer and six_bloom_layer. Valid values include:
#' 'lilac','zabelli' and 'arnoldred'. For more information see the NPN's Spring Index Maps documentation: https://www.usanpn.org/data/spring_indices
#' @param agdd_layer numeric value, accepts 32 or 50. When set, the results will attempt to resolve the date of the observation to
#' an AGDD value for the location; the 32 or 50 represents the base value of the AGDD value returned. All AGDD values are based on
#' a January 1st start date of the year in which the observation was taken.
#' @param additional_layers Data frame with first column named 'name' and containing the names of the layer for which to retrieve data
#' and the second column named 'param' and containing string representations of the time/elevation subset parameter to use.
#' This variable can be used to append additional geospatial layer data fields to the results, such that the date of observation
#' in each row will resolve to a value from the specified layers, given the location of the observation.
#' @param pheno_class_ids List of unique IDs for searching based on pheno class. Note that if
#' both pheno_class_id and phenophase_id are provided in the same request, phenophase_id will be ignored.
#' @param wkt WKT geometry by which filter data. Specifying a valid WKT within the contiguous US will
#' filter data based on the locations which fall within that WKT.
#' @return Data table of all status records returned as per the search parameters. Null if output directed to file.
#' @export
#' @examples \dontrun{
#' #Download all saguaro data for 2016
#' npn_download_status_data(
#'   request_source="Your Name or Org Here",
#'   years=c(2016),
#'   species_id=c(210),
#'   download_path="saguaro_data_2016.json"
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
  six_leaf_layer=FALSE,
  six_bloom_layer=FALSE,
  agdd_layer=NULL,
  six_sub_model=NULL,
  additional_layers=NULL,
  pheno_class_ids=NULL,
  wkt=NULL
){

  query <- npn_get_common_query_vars(request_source,
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
                  									 taxonomy_aggregate=NULL,
                  									 pheno_class_aggregate=NULL,
                  									 wkt,
                                     email)



  years <- sort(unlist(years))
  res <- npn_get_data_by_year("/observations/getObservations.ndjson?",query,years,download_path, six_leaf_layer, six_bloom_layer,agdd_layer, six_sub_model, additional_layers)

  return(res)

}




#'  Download Individual Phenometrics
#'
#'  This function allows for a parameterized search of all individual phenometrics records in the USA-NPN database, returning all records as per the search parameters in a
#'  data table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optionally results can be directed to an output file in
#'  which case raw JSON is converted to CSV and saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
#'  returns more data than can be handled at once in memory.
#'
#'  This data type includes estimates of the dates of phenophase onsets and ends for individual plants and for animal species at a site during a user-defined time
#'  period. Each row represents a series of consecutive "yes" phenophase status records, beginning with the date of the first "yes" and ending with the date of
#'  the last "yes", submitted for a given phenophase on a given organism. Note that more than one consecutive series for an organism may be present within a single
#'  growing season or year.
#'
#'  Most search parameters are optional, however, users are encouraged to supply additional search parameters to get results that are easier to work with. Request_Source
#'  must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
#'  you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
#'  a user email and/or IP address for usage data reporting later.
#'
#'  Additional fields provides the ability to specify additional, non-critical fields to include in the search results. A complete list of additional fields can be found in
#'  the NPN service's companion documentation
#'  https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.7yy4i3278v7u
#'  Metadata on all fields can be found in the following Excel sheet:
#'  http://www.usanpn.org/files/metadata/individual_phenometrics_datafield_descriptions.xlsx
#'
#' @param request_source Required field, string. Self-identify who is making requests to the data service
#' @param years Required field, list of strings. Specify the years to include in the search, e.g. c('2013','2014'). You must specify at least one year.
#' @param coords List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )
#' @param species_ids List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )
#' @param genus_ids List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.
#' @param family_ids List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.
#' @param order_ids List of unique IDs for searching based on taxonomic order, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids or family_ids are also set.
#' @param class_ids List of unique IDs for searching based on taxonomic class, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids, family_ids or order_ids are also set.
#' @param station_ids List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )
#' @param species_types List of unique species type names for searching based on species types, e.g. c ( "Deciduous", "Evergreen" )
#' @param network_ids List of unique IDs for searching based on partner group/network, e.g. c( 500, 300, ... )
#' @param states List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )
#' @param phenophase_ids List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )
#' @param functional_types List of unique functional type names, e.g. c ( "Birds"  )
#' @param additional_fields List of additional fields to be included in the search results, e.g. c ( "Station_Name", "Plant_Nickname" )
#' @param climate_data Boolean value indicating that all climate variables should be included in additional_fields.
#' @param ip_address Optional field, string. IP Address of user requesting data. Used for generating data reports
#' @param dataset_ids List of unique IDs for searching based on dataset, e.g. NEON or GRSM c(17,15)
#' @param individual_ids Comma-separated string of unique IDs for individual plants/animal species by which to filter the data
#' @param pheno_class_ids List of unique IDs for searching based on pheno class. Note that if
#' both pheno_class_id and phenophase_id are provided in the same request, phenophase_id will be ignored.
#' @param email Optional field, string. Email of user requesting data.
#' @param download_path Optional file path to which search results should be re-directed for later use.
#' @param six_leaf_layer Boolean value when set to true will attempt to resolve the date of the observation to a spring index, leafing
#' value for the location at which the observations was taken
#' @param six_bloom_layer Boolean value when set to true will attempt to resolve the date of the observation to a spring index, bloom
#' value for the location at which the observations was taken
#' @param six_sub_model Affects the results of the six layers returned. Can be used to specify one of three submodels used to calculate
#' the spring index values. Thus setting this field will change the results of six_leaf_layer and six_bloom_layer. Valid values include:
#' 'lilac','zabelli' and 'arnoldred'. For more information see the NPN's Spring Index Maps documentation: https://www.usanpn.org/data/spring_indices
#' @param agdd_layer numeric value, accepts 32 or 50. When set, the results will attempt to resolve the date of the observation to
#' an AGDD value for the location; the 32 or 50 represents the base value of the AGDD value returned. All AGDD values are based on
#' a January 1st start date of the year in which the observation was taken.
#' @param additional_layers Data frame with first column named 'name' and containing the names of the layer for which to retrieve data
#' and the second column named 'param' and containing string representations of the time/elevation subset parameter to use.
#' This variable can be used to append additional geospatial layer data fields to the results, such that the date of observation
#' in each row will resolve to a value from the specified layers, given the location of the observation.
#' @param wkt WKT geometry by which filter data. Specifying a valid WKT within the contiguous US will
#' filter data based on the locations which fall within that WKT.
#' @return Data table of all status records returned as per the search parameters. Null if output directed to file.
#' @export
#' @examples \dontrun{
#' #Download all saguaro data for 2013 and 2014
#' npn_download_individual_phenometrics(
#'   request_source="Your Name or Org Here",
#'   years=c('2013','2014'),
#'   species_id=c(210),
#'   download_path="saguaro_data_2013_2014.json"
#' )
#' }
npn_download_individual_phenometrics <- function(
  request_source,
  years,
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
  pheno_class_ids= NULL,
  email = NULL,
  download_path = NULL,
  six_leaf_layer=FALSE,
  six_bloom_layer=FALSE,
  agdd_layer=NULL,
  six_sub_model=NULL,
  additional_layers=NULL,
  wkt=NULL
){

  query <- npn_get_common_query_vars(request_source,
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
                                     taxonomy_aggregate=NULL,
                                     pheno_class_aggregate=NULL,
                                     wkt,
                                     email)

  if(!is.null(individual_ids)){
    query["individual_ids"] <- individual_ids
  }


  return(npn_get_data_by_year("/observations/getSummarizedData.ndjson?",query,years,download_path, six_leaf_layer, six_bloom_layer, agdd_layer, six_sub_model,additional_layers))

}





#'  Download Site Phenometrics
#'
#'  This function allows for a parameterized search of all site phenometrics records in the USA-NPN database, returning all records as per the search parameters in a
#'  data table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optionally results can be directed to an output file in
#'  which case raw JSON is converted to CSV and saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
#'  returns more data than can be handled at once in memory.
#'
#'  This data type includes estimates of the overall onset and end of phenophase activity for plant and animal species at a site over a user-defined time period.
#'  Each row provides the first and last occurrences of a given phenophase on a given species, beginning with the date of the first observed "yes" phenophase status
#'  record and ending with the date of the last observed "yes" record of the user-defined time period. For plant species where multiple individuals are monitored
#'  at the site, the date provided for "first yes" is the mean of the first "yes" records for each individual plant at the site, and the date for "last yes" is
#'  the mean of the last "yes" records. Note that a phenophase may have ended and restarted during the overall period of its activity at the site.
#'  These more fine-scale patterns can be explored in the individual phenometrics data.
#'
#'  Most search parameters are optional, however, users are encouraged to supply additional search parameters to get results that are easier to work with. Request_Source
#'  must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
#'  you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
#'  a user email and/or IP address for usage data reporting later.
#'
#'  Additional fields provides the ability to specify additional, non-critical fields to include in the search results. A complete list of additional fields can be found in
#'  the NPN service's companion documentation
#'  https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.ueaexz9bczti
#'  Metadata on all fields can be found in the following Excel sheet:
#'  http://www.usanpn.org/files/metadata/site_phenometrics_datafield_descriptions.xlsx
#'
#' @param request_source Required field, string. Self-identify who is making requests to the data service
#' @param years Required field, list of strings. Specify the years to include in the search, e.g. c('2013','2014'). You must specify at least one year.
#' @param num_days_quality_filter Required field, defaults to 30. The integer value sets the upper limit on the number of days difference between the
#' first Y value and the previous N value for each individual to be included in the data aggregation.
#' @param coords List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )
#' @param species_ids List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )
#' @param genus_ids List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.
#' @param family_ids List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.
#' @param order_ids List of unique IDs for searching based on taxonomic order, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids or family_ids are also set.
#' @param class_ids List of unique IDs for searching based on taxonomic class, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids, family_ids or order_ids are also set.
#' @param taxonomy_aggregate Boolean value indicating whether to aggregate data by a taxonomic order higher than species. This will be based on the values set in family_ids, order_ids, or class_ids. If one of those three fields are not set, then this value is ignored.
#' @param pheno_class_ids List of unique IDs for searching based on pheno class id, e.g. c (1, 5, 13)
#' @param pheno_class_aggregate Boolean value indicating whether to aggregate data by the pheno class ids as per the pheno_class_ids parameter. If the pheno_class_ids value is not set, then this parameter is ignored. This can be used in conjunction with taxonomy_aggregate and higher taxonomic level data filtering.
#' @param station_ids List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )
#' @param species_types List of unique species type names for searching based on species types, e.g. c ( "Deciduous", "Evergreen" )
#' @param network_ids List of unique IDs for searching based on partner group/network, e.g. ( 500, 300, ... )
#' @param states List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )
#' @param phenophase_ids List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )
#' @param functional_types List of unique functional type names, e.g. c ( "Birds"  )
#' @param additional_fields List of additional fields to be included in the search results, e.g. ( "Station_Name", "Plant_Nickname" )
#' @param climate_data Boolean value indicating that all climate variables should be included in additional_fields
#' @param ip_address Optional field, string. IP Address of user requesting data. Used for generating data reports
#' @param dataset_ids List of unique IDs for searching based on dataset, e.g. NEON or GRSM c(17,15)
#' @param email Optional field, string. Email of user requesting data.
#' @param download_path Optional file path to which search results should be re-directed for later use.
#' @param six_leaf_layer Boolean value when set to true will attempt to resolve the date of the observation to a spring index, leafing
#' value for the location at which the observations was taken
#' @param six_bloom_layer Boolean value when set to true will attempt to resolve the date of the observation to a spring index, bloom
#' value for the location at which the observations was taken
#' @param six_sub_model Affects the results of the six layers returned. Can be used to specify one of three submodels used to calculate
#' the spring index values. Thus setting this field will change the results of six_leaf_layer and six_bloom_layer. Valid values include:
#' 'lilac','zabelli' and 'arnoldred'. For more information see the NPN's Spring Index Maps documentation: https://www.usanpn.org/data/spring_indices
#' @param agdd_layer numeric value, accepts 32 or 50. When set, the results will attempt to resolve the date of the observation to
#' an AGDD value for the location; the 32 or 50 represents the base value of the AGDD value returned. All AGDD values are based on
#' a January 1st start date of the year in which the observation was taken.
#' @param additional_layers Data frame with first column named 'name' and containing the names of the layer for which to retrieve data
#' and the second column named 'param' and containing string representations of the time/elevation subset parameter to use.
#' This variable can be used to append additional geospatial layer data fields to the results, such that the date of observation
#' in each row will resolve to a value from the specified layers, given the location of the observation.
#' @param wkt WKT geometry by which filter data. Specifying a valid WKT within the contiguous US will
#' filter data based on the locations which fall within that WKT.
#' @return Data table of all status records returned as per the search parameters. Null if output directed to file.
#' @export
#' @examples \dontrun{
#' #Download all saguaro data for 2013 and 2014
#' npn_download_site_phenometrics(
#'   request_source="Your Name or Org Here",
#'   years=c('2013','2014'),
#'   species_id=c(210),
#'   download_path="saguaro_data_2013_2014.json"
#' )
#' }
npn_download_site_phenometrics <- function(
  request_source,
  years,
  num_days_quality_filter="30",
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
  six_leaf_layer=FALSE,
  six_bloom_layer=FALSE,
  agdd_layer=NULL,
  six_sub_model=NULL,
  additional_layers=NULL,
  taxonomy_aggregate=NULL,
  pheno_class_aggregate=NULL,
  wkt=NULL
){

  query <- npn_get_common_query_vars(request_source,
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
                                     email)

  query["num_days_quality_filter"] <- num_days_quality_filter



  return(npn_get_data_by_year("/observations/getSiteLevelData.ndjson?",query,years,download_path, six_leaf_layer, six_bloom_layer, agdd_layer, six_sub_model,additional_layers))

}





#'  Download Magnitude Phenometrics
#'
#'  This function allows for a parameterized search of all magnitude phenometrics in the USA-NPN database, returning all records as per the search results in a
#'  data table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optionally results can be directed to an output file in
#'  which case raw JSON is saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
#'  returns more data than can be handled at once in memory.
#'
#'  This data type includes various measures of the extent to which a phenophase for a plant or animal species is expressed across multiple individuals and sites
#'  over a user-selected set of time intervals. Each row provides up to eight calculated measures summarized weekly, bi-weekly, monthly or over a custom time interval.
#'  These measures include approaches to evaluate the shape of an annual activity curve, including the total number of "yes" records and the proportion of "yes"
#'  records relative to the total number of status records over the course of a calendar year for a region of interest. They also include several approaches for
#'  standardizing animal abundances by observer effort over time and space (e.g. mean active bird individuals per hour). See the Metadata window for more information.
#'
#'  Most search parameters are optional, however, failing to provide even a single search parameter will return all results in the database. Request_Source
#'  must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
#'  you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
#'  a user email and/or IP address for usage data reporting later.
#'
#'  Additional fields provides the ability to specify more, non-critical fields to include in the search results. A complete list of additional fields can be found in
#'  the NPN service's companion documentation
#'  https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.df3zspopwq98
#'  Metadata on all fields can be found in the following Excel sheet:
#'  http://www.usanpn.org/files/metadata/magnitude_phenometrics_datafield_descriptions.xlsx
#'
#' @param request_source Required field, string. Self-identify who is making requests to the data service
#' @param years Required field, list of strings. Specify the years to include in the search, e.g. c('2013','2014'). You must specify at least one year.
#' @param period_frequency Required field, integer. The integer value specifies the number of days by which to delineate the period of time specified by the
#' start_date and end_date, i.e. a value of 7 will delineate the period of time weekly. Any remainder days are grouped into the final delineation.
#' This parameter, while typically an int, also allows for a "special" string value, "months" to be passed in. Specifying this parameter as "months" will
#' delineate the period of time by the calendar months regardless of how many days are in each month. Defaults to 30 if omitted.
#' @param coords List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )
#' @param species_ids List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )
#' @param genus_ids List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.
#' @param family_ids List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.
#' @param order_ids List of unique IDs for searching based on taxonomic order, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids or family_ids are also set.
#' @param class_ids List of unique IDs for searching based on taxonomic class, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids, family_ids or order_ids are also set.
#' @param taxonomy_aggregate Boolean value indicating whether to aggregate data by a taxonomic order higher than species. This will be based on the values set in family_ids, order_ids, or class_ids. If one of those three fields are not set, then this value is ignored.
#' @param pheno_class_ids List of unique IDs for searching based on pheno class id, e.g. c (1, 5, 13)
#' @param pheno_class_aggregate Boolean value indicating whether to aggregate data by the pheno class ids as per the pheno_class_ids parameter. If the pheno_class_ids value is not set, then this parameter is ignored. This can be used in conjunction with taxonomy_aggregate and higher taxonomic level data filtering.
#' @param station_ids List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )
#' @param species_types List of unique species type names for searching based on species types, e.g. c ( "Deciduous", "Evergreen" )
#' @param network_ids List of unique IDs for searching based on partner group/network, e.g. ( 500, 300, ... )
#' @param states List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )
#' @param phenophase_ids List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )
#' @param functional_types List of unique functional type names, e.g. c ( "Birds"  )
#' @param additional_fields List of additional fields to be included in the search results, e.g. ( "Station_Name", "Plant_Nickname" )
#' @param climate_data Boolean value indicating that all climate variables should be included in additional_fields
#' @param ip_address Optional field, string. IP Address of user requesting data. Used for generating data reports
#' @param dataset_ids List of unique IDs for searching based on dataset, e.g. NEON or GRSM c(17,15)
#' @param email Optional field, string. Email of user requesting data.
#' @param download_path Optional file path to which search results should be re-directed for later use.
#' @param wkt WKT geometry by which filter data. Specifying a valid WKT within the contiguous US will
#' filter data based on the locations which fall within that WKT.
#' @return Data table of all status records returned as per the search parameters. Null if output directed to file.
#' @export
#' @examples \dontrun{
#' #Download book all saguaro data for 2013
#' npn_download_magnitude_phenometrics(
#'   request_source="Your Name or Org Here",
#'   years=c(2013),
#'   species_id=c(210),
#'   download_path="saguaro_data_2013.json"
#' )
#' }
npn_download_magnitude_phenometrics <- function(
  request_source,
  years,
  period_frequency="30",
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
  taxonomy_aggregate=NULL,
  pheno_class_aggregate=NULL,
  wkt=NULL
){

  query <- npn_get_common_query_vars(request_source,
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
                                     email)

  query["frequency"] <- period_frequency


  years <- sort(unlist(years))
  query['start_date'] <- paste0(years[1],"-01-01")
  query['end_date'] <- paste0(years[length(years)],"-12-31")



  url = npn_get_download_url("/observations/getMagnitudeData.ndjson")

  return (npn_get_data(url,query, download_path))

}






#' Get Data By Year
#'
#' Utility function to chain multiple requests to npn_get_data for requests where data should only be retrieved on an annual basis, or otherwise automatically be
#' delineated in some way. Results in a data table that's a combined set of the results from each request to the data service.
#'
#' @param endpoint String, the endpoint to query
#' @param query Base query string to use. This includes all the user selected parameters but doesn't include start/end date which will be automatically generated and
#' added
#' @param years List of strings; the years for which to retrieve data. There will be one request to the service for each year
#' @param download_path String, optional file path to the file for which to output the results.
#'
#' @return Data table - a data table combining each requests results from the service
#' @keywords internal
#'
npn_get_data_by_year <- function(
  endpoint,
  query,
  years,
  download_path=NULL,
  six_leaf_layer=FALSE,
  six_bloom_layer=FALSE,
  agdd_layer=NULL,
  six_sub_model=NULL,
  additional_layers=NULL
  ){

  all_data=NULL
  first_year=TRUE
  six_leaf_raster = NULL
  six_bloom_raster = NULL
  additional_rasters = NULL
  if(length(years) > 0){

    agdd_layer <- resolve_agdd_raster(agdd_layer)

    if(!is.null(additional_layers)){
      additional_layers$raster <- get_additional_rasters(additional_layers)
    }

    for(i in years){

      # This is where the start/end dates are automatically created
      # based on the input years.

      query['start_date'] = paste0(i,"-01-01")
      query['end_date'] = paste0(i, "-12-31")


      if(isTRUE(six_leaf_layer)){
        six_leaf_raster <- resolve_six_raster(i, "leaf", six_sub_model)
      }

      if(isTRUE(six_bloom_layer)){
        six_bloom_raster <- resolve_six_raster(i, "bloom", six_sub_model)
      }




      # We also have to generate a unique URL on each request to account
      # for the changes in the start/end date
      url = npn_get_download_url(endpoint)
      data = npn_get_data(url,query,download_path,!first_year, six_leaf_raster=six_leaf_raster, six_bloom_raster=six_bloom_raster,agdd_layer=agdd_layer, additional_layers=additional_layers)





      # First if statement checks whether this is the results returned is empty.
      # Second if statement checks if we've made a previous request that's
      # returned data. The data doesn't have to be combined if there was
      # no previous iteration / the results were empty
      if(!is.null(data) && is.null(download_path)){
        if(!is.null(all_data)){
          all_data <- rbindlist(list(all_data,data))
        }else{
          all_data = data
        }
      }

      if(!is.null(data)){
        first_year=FALSE
      }
    }

  }

  return(all_data)
}




#' Download NPN Data
#'
#' Generic utility function for querying data from the NPN data services.
#'
#' @param url The URL of the service endpoint to request data from
#' @param download_path  String, optional file path to the file for which to output the results.
#' @param always_append Boolean flag. When set to true, then we always append data to the download path. This is used
#' in the case of npn_get_data_by_year where we're making multiple requests to the same service and aggregating all
#' data results in a single file. Without this flag, otherwise, each call to the service would truncate the output file.
#'
#' @return Data table of the requested data. NULL if a download_path was specified.
#' @keywords internal
npn_get_data <- function(
  url,
  query,
  download_path=NULL,
  always_append=FALSE,
  six_leaf_raster=NULL,
  six_bloom_raster=NULL,
  agdd_layer=NULL,
  additional_layers=NULL
){


  h <- curl::new_handle()
  query = c(query, customrequest="POST")
  curl::handle_setform(h, .list = query)

  con <- curl::curl(url,handle=h)
  current_data <- NULL
  dtm<- data.table::data.table()
  set_has_data <- FALSE
  i<-0

  # Read the data 8MB at a time. This might be further optimized with the backing service.
  tryCatch({
    jsonlite::stream_in(con, function(df){


      # Reconcile all the points in the frame with the SIX leaf raster,
      # if it's been requested.
      if(!is.null(six_leaf_raster)){
        df <- npn_merge_geo_data(six_leaf_raster, "SI-x_Leaf_Value", df)
      }

      # Reconcile all the points in the frame with the SIX bloom raster,
      # if it's been requested.
      if(!is.null(six_bloom_raster)){
        df <- npn_merge_geo_data(six_bloom_raster, "SI-x_Bloom_Value", df)
      }

      if(!is.null(additional_layers)){
        for(j in rownames(additional_layers)){
          df <- npn_merge_geo_data(additional_layers[j,][['raster']][[1]],as.character(additional_layers[j,][['name']][[1]]),df)
        }
      }


      # Reconcile the AGDD point values with the data points if that
      # was requested.
      if(!is.null(agdd_layer)){

        date_col <- NULL

        if("observation_date" %in% colnames(df)){
          date_col <- "observation_date"
        }else if("mean_first_yes_doy" %in% colnames(df)){
          df$cal_date <- as.Date(df[, "mean_first_yes_doy"], origin = paste0(df[, "mean_first_yes_year"], "-01-01")) - 1
          date_col <- "cal_date"
        }else if("first_yes_day" %in% colnames(df)){
          df$cal_date <- as.Date(df[, "first_yes_doy"], origin = paste0(df[, "first_yes_year"], "-01-01")) - 1
          date_col <- "cal_date"
        }

        pvalues <- apply(df[,c('latitude','longitude',date_col)],1,function(x){
          rnpn::npn_get_agdd_point_data(layer=agdd_layer,lat=as.numeric(x['latitude']),long=as.numeric(x['longitude']),date=x[date_col])
        })

        pvalues <- t(as.data.frame(pvalues))
        colnames(pvalues) <- c(agdd_layer)
        df <- cbind(df,pvalues)

        if("cal_date" %in% colnames(df)){
          df$cal_date <- NULL
        }

      }



      # If the user asked for the data to be saved to file, then do that
      # otherwise append the frame to the dtm (master data table) variable
      if(is.null(download_path)){
        dtm <<- rbind(dtm, data.table::as.data.table(df))
      }else{
        if(length(df) > 0){
          set_has_data <- TRUE
          write.table(df,download_path,append=if(i==0 && !always_append) FALSE else TRUE, sep=",",eol="\n",row.names=FALSE,col.names=if(i==0 && !always_append) TRUE else FALSE)

        }
      }

      i<<-i+1



    },pagesize = 5000)
  },
  error=function(cond){
    message("Service is currently unavailable. Please try again later!")
    set_has_data <- FALSE
    dtm <- data.table::data.table()
  })


  # If the user asks for the data to be saved to file then
  # there is nothing to return.
  if(is.null(download_path)){
    return (dtm)
  }else{
    return (set_has_data)
  }


}





#' Generate Download URL
#'
#' Utility function to create the service point URL. Base URL comes from zzz.R, endpoint is specified in the code, and query_vars should be a list of parameters.
#' This function will manually put those query parameters into the proper GET syntax.
#'
#' @param endpoint The service point, e.g. "observations/getObservations.json?"
#' @param query_vars List of query params
#'
#' @return The URL, as a string
#' @keywords internal
npn_get_download_url <- function(
  endpoint
){
  url<- paste0(base(), endpoint)
#  query_str <- paste(names(query_vars),"=",query_vars,sep="",collapse = '&')

  return (paste0(url))
}


#' Get Common Query String Variables
#'
#' Utility function to generate a list of query string variables for requests to NPN data service points. Some parameters are basically present in all requests,
#' so this function helps put them together.
#'
#'
#' @return List of query string variables
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
  pheno_class_ids= NULL,
  taxonomy_aggregate=NULL,
  pheno_class_aggregate=NULL,
  wkt=NULL,
  email = NULL
){


  if(!is.null(family_ids)){
    species_ids = NULL
    genus_ids = NULL
  }

  if(!is.null(class_ids)){
    species_ids = NULL
    genus_ids = NULL
    family_ids = NULL
  }

  if(!is.null(order_ids)){
    species_ids = NULL
    genus_ids = NULL
    family_ids = NULL
    class_ids = NULL
  }

  if(!is.null(genus_ids)){
    species_ids = NULL
  }

  if(!is.null(wkt)){


    station_ids_shape <- tryCatch({
      shape_stations <- npn_stations_by_location(wkt)
      station_ids_shape <- shape_stations$station_id
    },error=function(msg){
      print("Unable to filter by shape file.")
      print(msg)
    })

    if(!is.null(station_ids)){
      station_ids <- c(station_ids,station_ids_shape)
    }else{
      station_ids <- station_ids_shape
    }

  }

  query = c(
    list(
      request_src = URLencode(request_source),
      climate_data = (if(climate_data) "1" else "0")
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

  if(!is.null(coords) && length(coords) == 4){

    if(is.numeric(coords)) {
      coords <- paste(coords)
    }
    query['bottom_left_x1'] = coords[1]
    query['bottom_left_y1'] = coords[2]
    query['upper_right_x2'] = coords[3]
    query['upper_right_y2'] = coords[4]
  }


  if(!is.null(ip_address)){
    query['IP_Address'] = ip_address
  }

  if(!is.null(email)){
    query['user_email'] = email
  }

  if(!is.null(taxonomy_aggregate) && taxonomy_aggregate){
    query['taxonomy_aggregate'] = "1"
  }

  if(!is.null(pheno_class_aggregate) && pheno_class_aggregate){
    query['pheno_class_aggregate'] = "1"
  }

  return(query)

}

