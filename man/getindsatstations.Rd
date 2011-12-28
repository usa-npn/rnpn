\name{getindsatstations}
\alias{getindsatstations}
\title{Get all observations for a particular species or set of species.}
\usage{
  getindsatstations(stationid = NA, downform = "json",
    printdf = TRUE,
    url = "http://www.usanpn.org/npn_portal/individuals/getIndividualsAtStations",
    ..., curl = getCurlHandle())
}
\arguments{
  \item{stationid}{use e.g., c(4881, 4882, etc.) if more
  than one species desired (numeric)}

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
  Observations for each species by date.
}
\description{
  Get all observations for a particular species or set of
  species.
}
\examples{
\dontrun{
getindsatstations(c(507, 523))
getindsatstations(c(507, 523), 'xml')
}
}

