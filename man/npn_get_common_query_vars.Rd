% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/npn_data_download.R
\name{npn_get_common_query_vars}
\alias{npn_get_common_query_vars}
\title{Get Common Query String Variables}
\usage{
npn_get_common_query_vars(
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
)
}
\value{
List of query string variables.
}
\description{
Utility function to generate a list of query string variables for requests to
NPN data service points. Some parameters are basically present in all
requests, so this function helps put them together.
}
\keyword{internal}
