\name{getallobssp}
\alias{getallobssp}
\title{Get all observations for a particular species or set of species.}
\usage{
  getallobssp(speciesid = NA, startdate = NA, enddate = NA,
    ..., curl = getCurlHandle())
}
\arguments{
  \item{speciesid}{species id numbers, from 1 to infinity,
  potentially, use e.g., c(52, 53, etc.) if more than one
  species desired (numeric)}

  \item{startdate}{start date of data period desired, see
  format in examples (character)}

  \item{enddate}{end date of data period desired, see
  format in examples (character)}

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
# Lookup names
lookup_names(name='Pinus', type='genus')

# Get data on some species
getallobssp(speciesid = c(52, 53), startdate='2008-01-01', enddate='2011-12-31')
}
}

