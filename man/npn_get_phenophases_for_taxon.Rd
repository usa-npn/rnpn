% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/npn_phenophases.R
\name{npn_get_phenophases_for_taxon}
\alias{npn_get_phenophases_for_taxon}
\title{Get Phenophases for Taxon}
\usage{
npn_get_phenophases_for_taxon(
  family_ids = NULL,
  order_ids = NULL,
  class_ids = NULL,
  genus_ids = NULL,
  date = NULL,
  return_all = 0,
  ...
)
}
\arguments{
\item{family_ids}{List of taxonomic family ids to search for.}

\item{order_ids}{List of taxonomic order ids to search for.}

\item{class_ids}{List of taxonomic class ids to search for}

\item{genus_ids}{List of taxonomic genus ids to search for}

\item{date}{Specify the date of interest. For this function to return anything, either this value must be set of return_all must be 1.}

\item{return_all}{Takes either 0 or 1 as input and defaults to 0. For this function to return anything, either this value must be set to 1
or date must be set.}

\item{...}{Curl options passed on to \code{\link[httr]{GET}}}
}
\value{
A data frame listing phenophases in the NPN database for the specified taxon and date.
}
\description{
This function gets a list of phenophases that are applicable for a provided taxonomic grouping, e.g. family, order.
Note that since a higher taxononmic order will aggregate individual species not every phenophase returned through this
function will be applicable for every species belonging to that taxonomic group.
}
\details{
It's also important to note that phenophase definitions can change for individual species over time, so there's a need
to specify either a date of interest, or to explicitly state that the function should return all phenophases that were
ever applicable for any species belonging to the specified taxonomic group.

When called, this function requires of these three parameters, exactly one of family_ids, order_ids or class_ids to be set.
}
