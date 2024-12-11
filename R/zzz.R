
pkg.env <- new.env(parent = emptyenv())

#' Set Environment
#'
#' By default this library will call the NPN's production services
#' but in some cases it's preferable to access the development web services
#' so this function allows for manually setting the web service endpoints
#' to use DEV instead. Just pass in "dev" to this function to change the
#' endpoints to use.
#' @param env The environment to use. Should be "ops" or "dev"
#' @export
npn_set_env <- function (env = "ops") {
  pkg.env$remote_env <- env
}

get_test_env <- function() {
  return("dev")
}

get_skip_long_tests <- function() {
  return(TRUE)
}


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

# TODO: remove completely once we're sure error handling is equivalent or better
# Remember to remove from importFrom in rnpn-package.R also
# npn_GET <- function(url, args, parse = FALSE, ...) {
#   res <- tryCatch(
#     {
#       tmp <- httr::GET(url, query = args, ...)
#       httr::stop_for_status(tmp)
#       tt <- httr::content(tmp, as = "text", encoding = "UTF-8")
#       if (nchar(tt) == 0) tt else jsonlite::fromJSON(tt, parse, flatten = TRUE)
#     },
#     error=function(cond){
#       # If the service is down for some reason give the user
#       # a message and return an empty list with n = 0
#       message("Service is unavailable. Try again later!")
#       tt <- "{\"nodata\":\"servicedown\"}"
#       if (nchar(tt) == 0) tt else jsonlite::fromJSON(tt, parse, flatten = TRUE)
#     }
#   )
#
# }


