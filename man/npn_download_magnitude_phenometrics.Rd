% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/npn_data_download.R
\name{npn_download_magnitude_phenometrics}
\alias{npn_download_magnitude_phenometrics}
\title{Download Magnitude Phenometrics}
\usage{
npn_download_magnitude_phenometrics(
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
)
}
\arguments{
\item{request_source}{Required field, character Self-identify who is making
requests to the data service.}

\item{years}{Required field, character vector. Specify the years to include
in the search, e.g. \code{c('2013','2014')}. You must specify at least one year.}

\item{period_frequency}{Required field, integer. The integer value specifies
the number of days by which to delineate the period of time specified by
the start_date and end_date, i.e. a value of 7 will delineate the period of
time weekly. Any remainder days are grouped into the final delineation.
This parameter, while typically an int, also allows for a "special" string
value, "months" to be passed in. Specifying this parameter as "months" will
delineate the period of time by the calendar months regardless of how many
days are in each month. Defaults to \code{30} if omitted.}

\item{coords}{Numeric vector, used to specify a bounding box as a search
parameter, e.g. \code{c(lower_left_lat, lower_left_long, upper_right, lat,upper_right_long)}.}

\item{species_ids}{Integer vector of unique IDs for searching based on
species, e.g. \code{c(3, 34, 35)}.}

\item{genus_ids}{Integer vector of unique IDs for searching based on
taxonomic family, e.g. \code{c(3, 34, 35)}. This parameter will take precedence
if \code{species_ids} is also set.}

\item{family_ids}{Integer vector of unique IDs for searching based on
taxonomic family, e.g. \code{c(3, 34, 35)}. This parameter will take precedence
if \code{species_ids} is also set.}

\item{order_ids}{Integer vector of unique IDs for searching based on
taxonomic order, e.g. \code{c(3, 34, 35)}. This parameter will take precedence
if \code{species_ids} or \code{family_ids} are also set.}

\item{class_ids}{Integer vector of unique IDs for searching based on
taxonomic class, e.g. \code{c(3, 34, 35)}. This parameter will take precedence
if \code{species_ids}, \code{family_ids} or \code{order_ids} are also set.}

\item{pheno_class_ids}{Integer vector of unique IDs for searching based on
pheno class. Note that if both \code{pheno_class_id} and \code{phenophase_id} are
provided in the same request, \code{phenophase_id} will be ignored.}

\item{station_ids}{Integer vector of unique IDs for searching based on site
location, e.g. \code{c(5, 9)}.}

\item{species_types}{Character vector of unique species type names for
searching based on species types, e.g. \code{c("Deciduous", "Evergreen")}.}

\item{network_ids}{Integer vector of unique IDs for searching based on
partner group/network, e.g. \code{c(500, 300)}.}

\item{states}{Character vector of US postal states to be used as search
params, e.g. \code{c("AZ", "IL")}.}

\item{phenophase_ids}{Integer vector of unique IDs for searching based on
phenophase, e.g. \code{c(323, 324)}.}

\item{functional_types}{Character vector of unique functional type names,
e.g. `c("Birds").}

\item{additional_fields}{Character vector of additional fields to be included
in the search results, e.g. \code{c("Station_Name", "Plant_Nickname")}.}

\item{climate_data}{Boolean value indicating that all climate variables
should be included in \code{additional_fields}.}

\item{ip_address}{Optional field, string. IP Address of user requesting data.
Used for generating data reports.}

\item{dataset_ids}{Integer vector of unique IDs for searching based on
dataset, e.g. NEON or GRSM \code{c(17,15)}.}

\item{email}{Optional field, string. Email of user requesting data.}

\item{download_path}{Character, optional file path to the file for which to
output the results.}

\item{taxonomy_aggregate}{Boolean value indicating whether to aggregate data
by a taxonomic order higher than species. This will be based on the values
set in \code{family_ids}, \code{order_ids}, or \code{class_ids}. If one of those three
fields are not set, then this value is ignored.}

\item{pheno_class_aggregate}{Boolean value indicating whether to aggregate
data by the pheno class ids as per the \code{pheno_class_ids} parameter. If the
\code{pheno_class_ids} value is not set, then this parameter is ignored. This
can be used in conjunction with \code{taxonomy_aggregate} and higher taxonomic
level data filtering.}

\item{wkt}{WKT geometry by which filter data. Specifying a valid WKT within
the contiguous US will filter data based on the locations which fall within
that WKT.}
}
\value{
A tibble of the requested data. If a \code{download_path} was specified,
the file path is returned.
}
\description{
This function allows for a parameterized search of all magnitude phenometrics
in the USA-NPN database, returning all records as per the search results in a
data table. Data fetched from NPN services is returned as raw JSON before
being channeled into a data table. Optionally results can be directed to an
output file in which case raw JSON is saved to file; in that case, data is
also streamed to file which allows for more easily handling of the data if
the search otherwise returns more data than can be handled at once in memory.
}
\details{
This data type includes various measures of the extent to which a phenophase
for a plant or animal species is expressed across multiple individuals and
sites over a user-selected set of time intervals. Each row provides up to
eight calculated measures summarized weekly, bi-weekly, monthly or over a
custom time interval. These measures include approaches to evaluate the shape
of an annual activity curve, including the total number of "yes" records and
the proportion of "yes" records relative to the total number of status
records over the course of a calendar year for a region of interest. They
also include several approaches for standardizing animal abundances by
observer effort over time and space (e.g. mean active bird individuals per
hour). See the Metadata window for more information.

Most search parameters are optional, however, failing to provide even a
single search parameter will return all results in the database.
Request_Source must be provided. This is a self-identifying string, telling
the service who is asking for the data or from where the request is being
made. It is recommended you provide your name or organization name. If the
call to this function is acting as an intermediary for a client, then you may
also optionally provide a user email and/or IP address for usage data
reporting later.

Additional fields provides the ability to specify more, non-critical fields
to include in the search results. A complete list of additional fields can be
found in the NPN service's \href{https://docs.google.com/document/d/1yNjupricKOAXn6tY1sI7-EwkcfwdGUZ7lxYv7fcPjO8/edit#heading=h.df3zspopwq98}{companion documentation}.
Metadata on all fields can be found in the following Excel sheet:
\url{https://www.usanpn.org/files/metadata/magnitude_phenometrics_datafield_descriptions.xlsx}
}
\examples{
\dontrun{
#Download book all saguaro data for 2013
npn_download_magnitude_phenometrics(
  request_source="Your Name or Org Here",
  years=c(2013),
  species_id=c(210),
  download_path="saguaro_data_2013.csv"
)
}
}
