\name{getobsspindloc}
\alias{getobsspindloc}
\title{Return all of the data, positive or negative, for an individual or a group
   of individuals, which are of a common species, at any number of locations.}
\usage{
  getobsspindloc(year = NULL, stationid = NULL,
    speciesid = NULL, printdf = TRUE, callopts = list())
}
\arguments{
  \item{year}{Year (numeric), required.}

  \item{stationid}{Station id; Use e.g., c(4881, 4882,
  etc.) if more than one species desired (numeric).}

  \item{speciesid}{Species id number (numeric).}

  \item{printdf}{print data.frame (default, TRUE) or not
  (FALSE)}

  \item{callopts}{Optional additional curl options
  (debugging tools mostly)}
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

