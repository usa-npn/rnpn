base_req_geoserver <-
  httr2::request("http://geoserver.usanpn.org/geoserver/") %>%
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)")

base <- function() {
  if (!is.null(pkg.env$remote_env)) {
    pkg.env$remote_env <- pkg.env$remote_env
  } else{
    pkg.env$remote_env <- "ops"
  }

  if (pkg.env$remote_env == "ops") {
    return('https://services.usanpn.org/npn_portal/')
  } else{
    return('https://services-staging.usanpn.org/npn_portal/')
  }
}

#TODO consider adding retry and rate-limiting
base_req <-
  httr2::request(base()) |>
  httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)")

base_data_domain <- function() {
  if (!is.null(pkg.env$remote_env)) {
    pkg.env$remote_env <- pkg.env$remote_env
  } else{
    pkg.env$remote_env <- "ops"
  }

  if (pkg.env$remote_env == "ops") {
    return('https://services.usanpn.org/')
  } else{
    return('https://services-staging.usanpn.org/')
  }
}

base_geoserver <- function() {
  if ( !is.null(pkg.env$remote_env)) {
    pkg.env$remote_env <- pkg.env$remote_env
  } else {
    pkg.env$remote_env <- "ops"
  }

  if (pkg.env$remote_env=="ops") {
    return('https://geoserver.usanpn.org/geoserver/wcs?service=WCS&version=2.0.1&request=GetCoverage&')
  } else {
    return('https://geoserver-dev.usanpn.org/geoserver/wcs?service=WCS&version=2.0.1&request=GetCoverage&')
  }
}
