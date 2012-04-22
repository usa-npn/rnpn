\name{getobsspindloc}
\alias{getobsspindloc}
\title{Return all of the data, positive or negative, for an individual or a group
   of individuals, which are of a common species, at any number of locations.}
\usage{
  getobsspindloc(year = NA, stationid = NA, speciesid = NA,
    downform = "json", printdf = TRUE,
    url = "http://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesIndividualAtLocation",
    ..., curl = getCurlHandle())
}
\arguments{
  \item{year}{Year.}

  \item{stationid}{Station id; Use e.g., c(4881, 4882,
  etc.) if more than one species desired (numeric).}

  \item{speciesid}{Species id number (numeric).}

  \item{downform}{Download format, one of 'json' or 'xml'.}

  \item{printdf}{print data.frame (default, TRUE) or not
  (FALSE)}

  \item{url}{the PLoS API url for the function (should be
  left to default)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Data frame/json/xml of phenophas id's, phenophase names,
  sequence numbers, color, date, and observation id's.
}
\description{
  Return all of the data, positive or negative, for an
  individual or a group of individuals, which are of a
  common species, at any number of locations.
}
\examples{
\dontrun{
getobsspindloc(2009, c(4881, 4882), 3)
getobsspindloc(2009, c(4881, 4882), 3, 'xml')
}
}

