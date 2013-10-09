\name{getallobssp}
\alias{getallobssp}
\title{Get all observations for a particular species or set of species.}
\usage{
  getallobssp(speciesid = NULL, startdate = NULL,
    enddate = NULL, callopts = list())
}
\arguments{
  \item{speciesid}{species id numbers, from 1 to infinity,
  potentially, use e.g., c(52, 53, etc.) if more than one
  species desired (numeric)}

  \item{startdate}{start date of data period desired, see
  format in examples (character)}

  \item{enddate}{end date of data period desired, see
  format in examples (character)}

  \item{callopts}{Optional additional curl options
  (debugging tools mostly)}
}
\value{
  An object of class npn with slots for taxa, stations,
  phenophase metadata, and data.
}
\description{
  Get all observations for a particular species or set of
  species.
}
\examples{
\dontrun{
# Lookup names
lookup_names(name='Pinus', type='genus')

# Get data on one species
getallobssp(speciesid = 52, startdate='2008-01-01', enddate='2011-12-31')

# Get data on two species
getallobssp(speciesid = c(52, 53), startdate='2008-01-01', enddate='2011-12-31')

# Get data on one species, convert to X
out <- getallobssp(speciesid = 52, startdate='2008-01-01', enddate='2011-12-31')
npn_todf(out)
}
}
\seealso{
  npn_todf
}

