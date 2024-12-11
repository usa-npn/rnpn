
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

#TODO: eventually remove this in favor of explode_query() once httr->httr2 is complete
# Helps create URL strings for requests to NPN data services in the format variable_name[number]=Value
npn_createArgList <- function(arg_name, arg_list) {
  args <- list()
  for (i in seq_along(arg_list)) {
    args[paste0(arg_name, '[', i, ']')] <- URLencode(toString(arg_list[i]))
  }
  return(args)
}

#' Null and empty coalescing operator
#'
#' Modified version of null coalescing operator (%||%) from rlang/soon to be in
#' base R. This version also coalesces empty lists and vectors
#' @param lhs left hand side; an object that is potentially NULL or of length 0
#' @param rhs right hand side; what to replace `lhs` with if NULL or length 0
#' @noRd
#' @examples
#' # with rlang's %||%
#'
#' NULL %||% 5
#' #> 5
#' character() %||% 5
#' #> character(0)
#'
#' # with modified version
#' NULL %|||% 5
#' #> 5
#' character() %|||% 5
#' #> 5
`%|||%` <- function(lhs, rhs) {
  if (is.null(lhs) | length(lhs) == 0) {
    rhs
  } else {
    lhs
  }
}

#' Explode multiple queries NPN style
#'
#' Alternative helper function that works in `httr2::req_url_query()` by naming
#' a vector `query_name[1]`, `query_name[2]`, etc
#' @noRd
#' @examples
#' species_id <- c(100, 103)
#' base_req %>%
#'   httr2::req_url_path_append('phenophases/getPhenophasesForSpecies.json') %>%
#'   httr2::req_url_query(!!!explode_query("species_id", species_id))
#'
explode_query <- function(arg_name, arg_vals) {
  if (!is.null(arg_vals)) {
    stats::setNames(arg_vals, paste0(arg_name, "[", seq_along(arg_vals), "]"))
  } else {
    NULL
  }
}
