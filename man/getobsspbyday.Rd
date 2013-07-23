\name{getobsspbyday}
\alias{getobsspbyday}
\title{Get observations by day for a particular species or set of species.}
\usage{
  getobsspbyday(speciesid = NA, startdate = NA,
    enddate = NA, format = "json", printdf = FALSE,
    tablename = NA, user = NA, dbname = NA, host = NA,
    addprimkey = NA,
    url = "https://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesByDay",
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

  \item{format}{Output format, one of 'json', 'xml', or
  'mysql'.}

  \item{printdf}{Print data.frame (default, TRUE) or not
  (FALSE)}

  \item{user}{If writemysql == TRUE, specify username for
  your MySQL login.}

  \item{dbname}{If writemysql == TRUE, specify the database
  name in MySQL.}

  \item{user}{If writemysql == TRUE, specify username for
  your MySQL login.}

  \item{url}{the PLoS API url for the function (should be
  left to default)}

  \item{...}{optional additional curl options (debugging
  tools mostly)}

  \item{curl}{If using in a loop, call getCurlHandle()
  first and pass the returned value in here (avoids
  unnecessary footprint)}
}
\value{
  Number of observations by day.
}
\description{
  Get observations by day for a particular species or set
  of species.
}
\details{
  Remember to turn on your MySQL server before using the
  'mysql' in the format argument.
}
\examples{
\dontrun{
getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31')
getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31', printdf = TRUE)
getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31', format = 'xml')

# Write to MySQL database.
getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31', format = 'mysql',
 tablename='rnpntest', user='yourusername', dbname='yourdatabasename',
 host='yourhostname', addprimkey=TRUE)
}
}

