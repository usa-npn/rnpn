\name{getpartners}
\alias{getpartners}
\title{Get a list of all the networks, i.e. organizations, with which NPN is associated.}
\usage{
  getpartners(url = "http://www.usanpn.org/npn_portal/networks/getPartnerNetworks",
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
  Data frame of each network’s name and id.
}
\description{
  Get a list of all the networks, i.e. organizations, with
  which NPN is associated.
}
\examples{
\dontrun{
getpartners()
}
}

