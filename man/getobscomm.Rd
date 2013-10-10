\name{getobscomm}
\alias{getobscomm}
\title{Get observation comments for a particular species or set of species.}
\usage{
  getobscomm(observationid = NULL, callopts = list())
}
\arguments{
  \item{observationid}{observation id, or as vector if >1,
  as e.g., c(2, 1938) (numeric)}

  \item{callopts}{Optional additional curl options
  (debugging tools mostly)}
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

