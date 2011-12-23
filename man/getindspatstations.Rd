\name{getindspatstations}
\alias{getindspatstations}
\title{Get all observations for a particular species or set of species.}
\usage{
  getindspatstations(speciesid = NA, stationid = NA,
    year = NA, downform = "json", printdf = TRUE,
    url = "http://www.usanpn.org/npn_portal/individuals/getIndividualsOfSpeciesAtStations",
    ..., curl = getCurlHandle())
}
\arguments{
  \item{speciesid}{species id numbers, from 1 to infinity,
  potentially, use e.g., c(52, 53, etc.) if more than one
  species desired (numeric)}

  \item{stationid}{use e.g., c(4881, 4882, etc.) if more
  than one species desired (numeric)}

  \item{year}{Year (numeric).}

  \item{downform}{Download format, one of 'json' or 'xml'.}

  \item{url}{the PLoS API url for the function (should be
  left to default)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Observations for each species by date.
}
\description{
  Get all observations for a particular species or set of
  species.
}
\examples{
\dontrun{
getindspatstations(35, c(60, 259), 2009)
getindspatstations(35, c(60, 259), 2009, 'xml')
}
}

