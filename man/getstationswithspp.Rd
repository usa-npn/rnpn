\name{getstationswithspp}
\alias{getstationswithspp}
\title{Get a list of all stations which have an individual whom is a member of a
   set of species.}
\usage{
  getstationswithspp(speciesid = NA, downform = "json",
    printdf = TRUE,
    url = "http://www.usanpn.org/npn_portal/stations/getStationsWithSpecies",
    ..., curl = getCurlHandle())
}
\arguments{
  \item{speciesid}{species id numbers, from 1 to infinity,
  potentially, use e.g., c(52, 53, etc.) if more than one
  species desired (numeric)}

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
getstationswithspp(c(52,53,54))
getstationswithspp(c(52,53), printdf = FALSE)
}
}

