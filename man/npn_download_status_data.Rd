% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/npn_data_download.R
\name{npn_download_status_data}
\alias{npn_download_status_data}
\title{Download Status and Intensity Records}
\usage{
npn_download_status_data(
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
)
}
\arguments{
\item{request_source}{Required field, string. Self-identify who is making requests to the data service}

\item{years}{Required field, list of strings. Specify the years to include in the search, e.g. c('2013','2014'). You must specify at least one year.}

\item{coords}{List of float values, used to specify a bounding box as a search parameter, e.g. c ( lower_left_lat, lower_left_long,upper_right,lat,upper_right_long )}

\item{species_ids}{List of unique IDs for searching based on species, e.g. c ( 3, 34, 35 )}

\item{genus_ids}{List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.}

\item{family_ids}{List of unique IDs for searching based on taxonomic family, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids is also set.}

\item{order_ids}{List of unique IDs for searching based on taxonomic order, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids or family_ids are also set.}

\item{class_ids}{List of unique IDs for searching based on taxonomic class, e.g. c ( 3, 34, 35 ) . This parameter will take precedence if species_ids, family_ids or order_ids are also set.}

\item{station_ids}{List of unique IDs for searching based on site location, e.g. c ( 5, 9, ... )}

\item{species_types}{List of unique species type names for searching based on species types, e.g. c ( "Deciduous", "Evergreen" )}

\item{network_ids}{List of unique IDs for searching based on partner group/network, e.g. ( 500, 300, ... )}

\item{states}{List of US postal states to be used as search params, e.g. c ( "AZ", "IL" )}

\item{phenophase_ids}{List of unique IDs for searching based on phenophase, e.g. c ( 323, 324, ... )}

\item{functional_types}{List of unique functional type names, e.g. c ( "Birds"  )}

\item{additional_fields}{List of additional fields to be included in the search results, e.g. c( "Station_Name", "Plant_Nickname" )}

\item{climate_data}{Boolean value indicating that all climate variables should be included in additional_fields}

\item{ip_address}{Optional field, string. IP Address of user requesting data. Used for generating data reports}

\item{dataset_ids}{List of unique IDs for searching based on dataset, e.g. NEON or GRSM c(17,15)}

\item{email}{Optional field, string. Email of user requesting data.}

\item{download_path}{Optional file path to which search results should be re-directed for later use.}

\item{six_leaf_layer}{Boolean value when set to true will attempt to resolve the date of the observation to a spring index, leafing
value for the location at which the observations was taken}

\item{six_bloom_layer}{Boolean value when set to true will attempt to resolve the date of the observation to a spring index, bloom
value for the location at which the observations was taken}

\item{agdd_layer}{numeric value, accepts 32 or 50. When set, the results will attempt to resolve the date of the observation to
an AGDD value for the location; the 32 or 50 represents the base value of the AGDD value returned. All AGDD values are based on
a January 1st start date of the year in which the observation was taken.}

\item{six_sub_model}{Affects the results of the six layers returned. Can be used to specify one of three submodels used to calculate
the spring index values. Thus setting this field will change the results of six_leaf_layer and six_bloom_layer. Valid values include:
'lilac','zabelli' and 'arnoldred'. For more information see the NPN's Spring Index Maps documentation: https://www.usanpn.org/data/spring_indices}

\item{additional_layers}{Data frame with first column named 'name' and containing the names of the layer for which to retrieve data
and the second column named 'param' and containing string representations of the time/elevation subset parameter to use.
This variable can be used to append additional geospatial layer data fields to the results, such that the date of observation
in each row will resolve to a value from the specified layers, given the location of the observation.}

\item{pheno_class_ids}{List of unique IDs for searching based on pheno class. Note that if
both pheno_class_id and phenophase_id are provided in the same request, phenophase_id will be ignored.}

\item{wkt}{WKT geometry by which filter data. Specifying a valid WKT within the contiguous US will
filter data based on the locations which fall within that WKT.}
}
\value{
Data table of all status records returned as per the search parameters. Null if output directed to file.
}
\description{
This function allows for a parameterized search of all status records in the USA-NPN database, returning all records as per the search parameters in a data
 table. Data fetched from NPN services is returned as raw JSON before being channeled into a data table. Optionally results can be directed to an output file in
 which case the raw JSON is converted to CSV and saved to file; in that case, data is also streamed to file which allows for more easily handling of the data if the search otherwise
 returns more data than can be handled at once in memory.
}
\details{
Most search parameters are optional, however, users are encouraged to supply additional search parameters to get results that are easier to work with. Request_Source
 must be provided. This is a self-identifying string, telling the service who is asking for the data or from where the request is being made. It is recommended
 you provide your name or organization name. If the call to this function is acting as an intermediary for a client, then you may also optionally provide
 a user email and/or IP address for usage data reporting later.

 Additional fields provides the ability to specify more, non-critical fields to include in the search results. A complete list of additional fields can be found in
 the NPN service's companion documentation
 https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.w0nctgedhaop
 Metadata on all fields can be found in the following Excel sheet:
 http://www.usanpn.org/files/metadata/status_intensity_datafield_descriptions.xlsx
}
\examples{
\dontrun{
#Download all saguaro data for 2016
npn_download_status_data(
  request_source="Your Name or Org Here",
  years=c(2016),
  species_id=c(210),
  download_path="saguaro_data_2016.json"
)
}
}
