\name{getobscomm}
\alias{getobscomm}
\title{Get observation comments for a particular species or set of species.}
\usage{
  getobscomm(observationid = NA, downform = "json",
    url = "https://www.usanpn.org/npn_portal/observations/getObservationComment",
    ..., curl = getCurlHandle())
}
\arguments{
  \item{observationid}{observation id, or as vector if >1,
  as e.g., c(2, 1938) (numeric)}

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
  Comments.
}
\description{
  Get observation comments for a particular species or set
  of species.
}
\examples{
\dontrun{
getobscomm(1938)
}
}

