% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/npn_geoserver.R
\name{get_additional_rasters}
\alias{get_additional_rasters}
\title{Get Additional Layers}
\usage{
get_additional_rasters(data)
}
\arguments{
\item{data}{Data frame with first column named \code{name} and containing the
names of the layer for which to retrieve data and the second column named
\code{param} and containing string representations of the time/elevation subset
parameter to pass.}
}
\value{
Returns a data frame containing the raster objects related to the
specified layers.
}
\description{
Utility function to easily take arbitrary layer name parameters as a data
frame and return the raster data from NPN Geospatial data services.
}
\keyword{internal}
