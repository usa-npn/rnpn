\name{getallobssp}
\alias{getallobssp}
\title{Get all observations for a particular species or set of species.}
\usage{
  getallobssp(speciesid = NA, startdate = NA, enddate = NA,
    downform = "json",
    url = "http://www.usanpn.org/npn_portal/observations/getAllObservationsForSpecies",
    method = "getAllObservationsForSpecies", ...,
    curl = getCurlHandle())
}
\arguments{
  \item{speciesid}{species id numbers, from 1 to infinity,
  potentially, use e.g., c(52, 53, etc.) if more than one
  species desired (numeric)}

  \item{startdate}{start date of data period desired, see
  format in examples (character)}

  \item{enddate}{end date of data period desired, see
  format in examples (character)}

  \item{downform}{Download format, one of 'json' or 'xml'.}

  \item{url}{the PLoS API url for the function (should be
  left to default)}

  \item{method}{the API method (should be left to default)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Date when article was published.
}
\description{
  Get all observations for a particular species or set of
  species.
}
\examples{
\dontrun{
getallobssp(speciesid = c(52, 53), '2008-01-01', '2011-12-31')
getallobssp(speciesid = c(52, 53), '2008-01-01', '2011-12-31', 'xml')
}
}

