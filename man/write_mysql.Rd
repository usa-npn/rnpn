\name{write_mysql}
\alias{write_mysql}
\title{Write data to MySQL.}
\usage{
  write_mysql(dat2write, tablename, user, dbname, host,
    addprimkey = TRUE)
}
\arguments{
  \item{dat2write}{data.frame to write to mysql}

  \item{tablename}{What do you want to name the new table
  in mysql?}

  \item{user}{Your username for MySQL (e.g., ).}

  \item{dbname}{The MySQL database you want to write the
  data.frame in to.}

  \item{host}{Your host name (e.g., ).}

  \item{addprimkey}{Add primary key or not (default =
  TRUE).}
}
\value{
  Data written to MySQL, and prints message saying so.
}
\description{
  Write data to MySQL.
}
\details{
  Use this function wrapped with suppressMessages() to
  suppress messages.
}
\examples{
\dontrun{
# Use this function on its own
dat <- data.frame(a=rnorm(10^2), b=rnorm(10^2))
write_mysql(dat, 'dat230', 'asdfaf', 'test', 'localhost', TRUE)

# Use this function within another function
getobsspbyday()
}
}

