
#'  Download Status and Intesity Records
#'
#'  This function allows for a parameterized search of all status records in the USA-NPN database, returning all records as per the search results in a data
#'  table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optinally results can be directed to an output file in
#'  which case raw JSON is saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
#'  returns more data than can be handled at once in memory.
#'
#'  All search parameters are optional, however, failing to provide even a single search parameter will return all results in the database. Request_Source
#'  must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
#'  you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
#'  a user email and/or IP address for usage data reporting later.
#'
#'  Additional fields provides the ability to specify more, non-critical fields to include in the search results. A complete list of additional fields can be found in
#'  the NPN service's companion documention
#'  https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.w0nctgedhaop
#'  Metadata on all fields can be found in the following Excel sheet:
#'  http://www.usanpn.org/files/metadata/status_intensity_datafield_descriptions.xlsx
#'
#' @param request_source Required field, string. Self-identify who is making requests to the data service
#' @param start_date String. Specify the start date of the search. Must be used in conjunction with end date.
#' @param end_date String. Specify the end date of the search. Must be used in conjunction with start date.
#' @param coords List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )
#' @param species_ids List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )
#' @param station_ids List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )
#' @param species_types List of unique species type names for searching based on species types, e.g. c ( "Decidious", "Evergreen" )
#' @param network_ids List of unique IDs for searching based on parter group/network, e.g. ( 500, 300, ... )
#' @param states List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )
#' @param phenophase_ids List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )
#' @param functional_types List of unique functional type names, e.g. c ( "Birds"  )
#' @param additional_fields List of additional fields to be included in the search results, e.g. ( "Station_Name", "Plant_Nickname" )
#' @param climate_data Boolean value indicating that all climate variables should be included in additional_fields
#' @param ip_address Optional field, string. IP Address of user requesting data. Used for generating data reports
#' @param email Optional field, string. Email of user requesting data.
#' @param download_path Optional file path to which search results should be re-directed for later use.
#'
#' @return Data table of all status records returned as per the search parameters. Null if output directed to file.
#' @export
#' @examples \dontrun{
#' Download all red maple data for 2016:
#' npn_download_status_data(request_source="Your Name or Org Here", start_date="2016-01-01", end_date="2016-12-31", species_id=c(3))
#'
#' Download all saguaro data for the summer of 2016
#' npn_download_status_data(request_source="Your Name or Org Here", start_date="2016-03-01", end_date="2016-10-31", species_id=c(210),
#' download_path="saguaro_data_summer_2016.json")
#' }
npn_download_status_data = function(
  request_source,
  start_date = NULL,
  end_date = NULL,
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
  email = NULL,
  download_path = NULL
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
                                     email)


  if(!is.null(start_date) && !is.null(end_date)){
    query['start_date'] = start_date
    query['end_date'] = end_date
  }

  url = npn_get_download_url("/observations/getObservations.json?", query)

  return (npn_get_data(url,download_path))

}




#'  Download Individual Phenometrics
#'
#'  This function allows for a parameterized search of all individual phenometrics records in the USA-NPN database, returning all records as per the search results in a
#'  data table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optinally results can be directed to an output file in
#'  which case raw JSON is saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
#'  returns more data than can be handled at once in memory.
#'
#'  This data type includes estimates of the dates of phenophase onsets and ends for individual plants and for animal species at a site during a user-defined time
#'  period. Each row represents a series of consecutive "yes" phenophase status records, beginning with the date of the first "yes" and ending with the date of
#'  the last "yes", submitted for a given phenophase on a given organism. Note that more than one consecutive series for an organism may be present within a single
#'  growing season or year.
#'
#'  Most search parameters are optional, however, failing to provide even a single search parameter will return all results in the database. Request_Source
#'  must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
#'  you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
#'  a user email and/or IP address for usage data reporting later.
#'
#'  Additional fields provides the ability to specify more, non-critical fields to include in the search results. A complete list of additional fields can be found in
#'  the NPN service's companion documention
#'  https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.7yy4i3278v7u
#'  Metadata on all fields can be found in the following Excel sheet:
#'  http://www.usanpn.org/files/metadata/individual_phenometrics_datafield_descriptions.xlsx
#'
#' @param request_source Required field, string. Self-identify who is making requests to the data service
#' @param years Required field, list of strings. Specify the years to include in the search, e.g. c('2013','2014'). You must specify at least one year.
#' @param coords List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )
#' @param species_ids List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )
#' @param station_ids List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )
#' @param species_types List of unique species type names for searching based on species types, e.g. c ( "Decidious", "Evergreen" )
#' @param network_ids List of unique IDs for searching based on parter group/network, e.g. ( 500, 300, ... )
#' @param states List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )
#' @param phenophase_ids List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )
#' @param functional_types List of unique functional type names, e.g. c ( "Birds"  )
#' @param additional_fields List of additional fields to be included in the search results, e.g. ( "Station_Name", "Plant_Nickname" )
#' @param climate_data Boolean value indicating that all climate variables should be included in additional_fields
#' @param ip_address Optional field, string. IP Address of user requesting data. Used for generating data reports
#' @param email Optional field, string. Email of user requesting data.
#' @param download_path Optional file path to which search results should be re-directed for later use.
#'
#' @return Data table of all status records returned as per the search parameters. Null if output directed to file.
#' @export
#' @examples \dontrun{
#' Download all saguaro data for 2013 and 2014
#' npn_download_individual_phenometrics(request_source="Your Name or Org Here", years=c('2013','2014'), species_id=c(210),
#' download_path="saguaro_data_2013_2014.json")
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
  email = NULL,
  download_path = NULL
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
                                     email)

  if(!is.null(individual_ids)){
    query["individual_ids"] <- individual_ids
  }


  return(npn_get_data_by_year("/observations/getSummarizedData.json?",query,years,download_path))

}





#'  Download Site Phenometrics
#'
#'  This function allows for a parameterized search of all site records in the USA-NPN database, returning all records as per the search results in a
#'  data table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optinally results can be directed to an output file in
#'  which case raw JSON is saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
#'  returns more data than can be handled at once in memory.
#'
#'  This data type includes estimates of the overall onset and end of phenophase activity for plant and animal species at a site over a user-defined time period.
#'  Each row provides the first and last occurrences of a given phenophase on a given species, beginning with the date of the first observed “yes” phenophase status
#'  record and ending with the date of the last observed “yes” record of the user-defined time period. For plant species where multiple individuals are monitored
#'  at the site, the date provided for “first yes” is the mean of the first “yes” records for each individual plant at the site, and the date for “last yes” is
#'  the mean of the last “yes” records. Note that a phenophase may have ended and restarted during the overall period of its activity at the site.
#'  These more fine-scale patterns can be explored in the individual phenometrics data.
#'
#'  Most search parameters are optional, however, failing to provide even a single search parameter will return all results in the database. Request_Source
#'  must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
#'  you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
#'  a user email and/or IP address for usage data reporting later.
#'
#'  Additional fields provides the ability to specify more, non-critical fields to include in the search results. A complete list of additional fields can be found in
#'  the NPN service's companion documention
#'  https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.ueaexz9bczti
#'  Metadata on all fields can be found in the following Excel sheet:
#'  http://www.usanpn.org/files/metadata/site_phenometrics_datafield_descriptions.xlsx
#'
#' @param request_source Required field, string. Self-identify who is making requests to the data service
#' @param years Required field, list of strings. Specify the years to include in the search, e.g. c('2013','2014'). You must specify at least one year.
#' @param num_days_quality_filter Required field, defaultsto 30. The integer value sets the upper limit on the number of days difference between the
#' first Y value and the previous N value for each individual to be included in the data aggregation.
#' @param coords List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )
#' @param species_ids List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )
#' @param station_ids List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )
#' @param species_types List of unique species type names for searching based on species types, e.g. c ( "Decidious", "Evergreen" )
#' @param network_ids List of unique IDs for searching based on parter group/network, e.g. ( 500, 300, ... )
#' @param states List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )
#' @param phenophase_ids List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )
#' @param functional_types List of unique functional type names, e.g. c ( "Birds"  )
#' @param additional_fields List of additional fields to be included in the search results, e.g. ( "Station_Name", "Plant_Nickname" )
#' @param climate_data Boolean value indicating that all climate variables should be included in additional_fields
#' @param ip_address Optional field, string. IP Address of user requesting data. Used for generating data reports
#' @param email Optional field, string. Email of user requesting data.
#' @param download_path Optional file path to which search results should be re-directed for later use.
#'
#' @return Data table of all status records returned as per the search parameters. Null if output directed to file.
#' @export
#' @examples \dontrun{
#' Download all saguaro data for 2013 and 2014
#' npn_download_site_phenometrics(request_source="Your Name or Org Here", years=c('2013','2014'), species_id=c(210),
#' download_path="saguaro_data_2013_2014.json")
#' }
npn_download_site_phenometrics <- function(
  request_source,
  years,
  num_days_quality_filter=30,
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
  email = NULL,
  download_path = NULL
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
                                     email)

  query["num_days_quality_filter"] <- num_days_quality_filter



  return(npn_get_data_by_year("/observations/getSiteLevelData.json?",query,years,download_path))

}





#'  Download Magnitude Phenometrics
#'
#'  This function allows for a parameterized search of all magnitude phenometrics in the USA-NPN database, returning all records as per the search results in a
#'  data table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optinally results can be directed to an output file in
#'  which case raw JSON is saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
#'  returns more data than can be handled at once in memory.
#'
#'  This data type includes various measures of the extent to which a phenophase for a plant or animal species is expressed across multiple individuals and sites
#'  over a user-selected set of time intervals. Each row provides up to eight calculated measures summarized weekly, bi-weekly, monthly or over a custom time interval.
#'  These measures include approaches to evaluate the shape of an annual activity curve, including the total number of “yes” records and the proportion of “yes”
#'  records relative to the total number of status records over the course of a calendar year for a region of interest. They also include several approaches for
#'  standardizing animal abundances by observer effort over time and space (e.g. mean active bird individuals per hour). See the Metadata window for more information.
#'
#'  Most search parameters are optional, however, failing to provide even a single search parameter will return all results in the database. Request_Source
#'  must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
#'  you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
#'  a user email and/or IP address for usage data reporting later.
#'
#'  Additional fields provides the ability to specify more, non-critical fields to include in the search results. A complete list of additional fields can be found in
#'  the NPN service's companion documention
#'  https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.df3zspopwq98
#'  Metadata on all fields can be found in the following Excel sheet:
#'  http://www.usanpn.org/files/metadata/magnitude_phenometrics_datafield_descriptions.xlsx
#'
#' @param request_source Required field, string. Self-identify who is making requests to the data service
#' @param start_date Required field, string. Specify the start date of the search. Must be used in conjunction with end date.
#' @param end_date Required field, string. Specify the end date of the search. Must be used in conjunction with start date.
#' @param period_frequency Required field, integer. The integer value specifies the number of days by which to delineate the period of time specified by the
#' start_date and end_date, i.e. a value of 7 will delineate the period of time weekly. Any remainder days are grouped into the final delineation.
#' This parameter, while typically an int, also allows for a “special” string value, “months” to be passed in. Specifying this parameter as “months” will
#' delineate the period of time by the calendar months regardless of how many days are in each month. Defaults to 30 if omitted.
#' @param coords List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )
#' @param species_ids List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )
#' @param station_ids List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )
#' @param species_types List of unique species type names for searching based on species types, e.g. c ( "Decidious", "Evergreen" )
#' @param network_ids List of unique IDs for searching based on parter group/network, e.g. ( 500, 300, ... )
#' @param states List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )
#' @param phenophase_ids List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )
#' @param functional_types List of unique functional type names, e.g. c ( "Birds"  )
#' @param additional_fields List of additional fields to be included in the search results, e.g. ( "Station_Name", "Plant_Nickname" )
#' @param climate_data Boolean value indicating that all climate variables should be included in additional_fields
#' @param ip_address Optional field, string. IP Address of user requesting data. Used for generating data reports
#' @param email Optional field, string. Email of user requesting data.
#' @param download_path Optional file path to which search results should be re-directed for later use.
#'
#' @return Data table of all status records returned as per the search parameters. Null if output directed to file.
#' @export
#' @examples \dontrun{
#' Download all saguaro data for 2013
#' npn_download_magnitude_phenometrics(request_source="Your Name or Org Here", start_date='2013-01-01', end_date='2013-12-31', species_id=c(210),
#' download_path="saguaro_data_2013.json")
#' }
npn_download_magnitude_phenometrics <- function(
  request_source,
  start_date,
  end_date,
  period_frequency=30,
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
  email = NULL,
  download_path = NULL
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
                                     email)

  query["frequency"] <- period_frequency


  query['start_date'] = start_date
  query['end_date'] = end_date



  url = npn_get_download_url("/observations/getMagnitudeData.json?", query)

  return (npn_get_data(url,download_path))

}






#' Get Data By Year
#'
#' Utility function to chain multiple requests to npn_get_data for requests where data should only be retrieved on an annual basis, or otherwise automatically be
#' deliniated in some way. Results in a data table that's a combined set of the results from each request to the data service.
#'
#' @param endpoint String, the endpoint to query
#' @param query Base query string to use. This includes all the user selected parameters but doesn't include start/end date which will be automatically generated and
#' added
#' @param years List of strings; the years for which to retrieve data. There will be one request to the service for each year
#' @param download_path String, optional file path to the file for which to output the results.
#'
#' @return Data table - a data table combining each requests results from the service
#' @keywords internal
npn_get_data_by_year <- function(
  endpoint,
  query,
  years,
  download_path=NULL
  ){

  all_data=NULL
  first_year=TRUE
  if(length(years) > 0){
    for(i in years){

      # This is where the start/end dates are automatically created
      # based on the input years.

      query['start_date'] = paste0(i,"-01-01")
      query['end_date'] = paste0(i, "-12-31")

      # We also have to generate a unique URL on each request to account
      # for the changes in the start/end date
      url = npn_get_download_url(endpoint, query)
      data = npn_get_data(url,download_path,!first_year)

      # First if statement checks whether this is the results returned is empty.
      # Second if statement checks if we've made a previous request that's
      # returned data. The data doesn't have to be combined if there was
      # no previous iteration / the results were empty
      if(!is.null(data)){
        if(!is.null(all_data)){
          all_data <- rbindlist(list(all_data,data))
        }else{
          all_data = data
        }
      }

      first_year=FALSE
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
#' @return Data table of the requested dawta. NULL if a download_path was specified.
#' @keywords internal
npn_get_data <- function(
  url,
  download_path=NULL,
  always_append=FALSE
){
  con <- curl (url)
  open(con,"rb")
  data<- ""
  i<-0
  # Read the data 8MB at a time. This might be further optimized with the backing service.
  while(length(x <- readBin(con, raw(), n = 8388608))){

    if(is.null(download_path)){
      data<-paste(data, rawToChar(x))
    }else{
      write(rawToChar(x),download_path,append=if(i==0 && !always_append) FALSE else TRUE, sep="")
    }
    i<-i+1

  }

  close(con)
  if(is.null(download_path)){
    return(as.data.table(jsonlite::fromJSON(data)))
  }else{
    return (NULL)
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
  endpoint,
  query_vars
){
  url<- paste0(base(), endpoint)
  query_str <- paste(names(query_vars),"=",query_vars,sep="",collapse = '&')

  return (paste0(url,query_str))
}


#' Get Common Query String Variables
#'
#' Utility function to generate a list of query string variables for requests to NPN data service points. Some parametse are basically present in all requests,
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
  email = NULL
){

  query = c(
    list(
      request_src = request_source,
      climate_data = (if(climate_data) 1 else 0)
    ),
    # All these variables take a multiplicity of possible parameters, this will help put them all together.
    npn_createArgList("species_id", species_ids),
    npn_createArgList("site_id", station_ids),
    npn_createArgList("species_type", species_types),
    npn_createArgList("network_id", network_ids),
    npn_createArgList("state", states),
    npn_createArgList("phenophase_id", phenophase_ids),
    npn_createArgList("functional_type", functional_types),
    npn_createArgList("additional_field", additional_fields)
  )

  if(!is.null(coords) && length(coords) == 4){
    query['bottom_left_x1'] = coords[1]
    query['bottom_left_y1'] = coords[2]
    query['upper_rigth_x2'] = coords[3]
    query['upper_right_y2'] = coords[4]
  }


  if(!is.null(ip_address)){
    query['IP_Address'] = ip_address
  }

  if(!is.null(email)){
    query['user_email'] = email
  }

  return(query)

}
