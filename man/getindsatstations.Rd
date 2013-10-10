\name{getindsatstations}
\alias{getindsatstations}
\title{Get all observations for a particular species or set of species.}
\usage{
  getindsatstations(stationid = NULL, printdf = TRUE,
    callopts = list())
}
\arguments{
  \item{stationid}{use e.g., c(4881, 4882, etc.) if more
  than one species desired (numeric)}

  \item{printdf}{print data.frame (default, TRUE) or not
  (FALSE)}

  \item{callopts}{Optional additional curl options
  (debugging tools mostly)}
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

