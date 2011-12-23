\name{getobsspindloc}
\alias{getobsspindloc}
\title{Get a list of all stations which have an individual whom is a member of a
   set of species.}
\usage{
  getobsspindloc(year = NA, stationid = NA, speciesid = NA,
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
  Stations' latitude and longitude, names, and ids.
}
\description{
  Get a list of all stations which have an individual whom
  is a member of a set of species.
}
\examples{
\dontrun{
getobsspindloc(2009, c(4881, 4882), 3)
}
}

