\name{getstationsbystate}
\alias{getstationsbystate}
\title{Get number of stations by state in a data frame.}
\usage{
  getstationsbystate(url = "http://www.usanpn.org/npn_portal/stations/getStationCountByState",
    ..., curl = getCurlHandle())
}
\arguments{
  \item{url}{the PLoS API url for the function (should be
  left to default)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Number of stations by state.
}
\description{
  Get number of stations by state in a data frame.
}
\examples{
\dontrun{
getstationsbystate()
}
}

