#' Runs a basic check to see if
#' a valid response is returned by
#' the NPN Portal service
#' and returns TRUE/FALSE
#'
#' Used in unit tests to determine if
#' tests should be run
#'
#'
check_service <- function() {
  req <- base_req %>%
    httr2::req_url_path_append('species/getSpeciesById.json') %>%
    httr2::req_url_query(species_id = 3) %>%
    httr2::req_method("HEAD")
  resp <- httr2::req_perform(req)
  httr2::resp_is_error(resp)
}



#TODO this no longer works.  This endpoint doesn't exist
check_data_service <- function() {
  # npn_set_env(get_test_env())
  url <- paste0(base_data_domain(), 'web-services/geo.html')
  res <- NULL
  tryCatch({
    res <- httr::GET(url)
  }, error = function(msg) {
    return(FALSE)
  })

  if (is.null(res) || res$status_code != 200) {
    return(FALSE)
  }

  return(TRUE)
}

#' Runs a basic check to see if
#' a valid response is returned by
#' Geoserver and returns TRUE/FALSE
#'
#' Used in unit tests to determine if
#' tests should be run
#'
check_geo_service <- function() {
  if (!is.null(pkg.env$remote_env)) {
    pkg.env$remote_env <- pkg.env$remote_env
  } else{
    pkg.env$remote_env <- "ops"
  }

  if (pkg.env$remote_env == "ops") {
    url <- "http://geoserver.usanpn.org/geoserver/ows?service=wms&version=1.3.0&request=GetCapabilities"
  } else{
    url <- "http://geoserver-dev.usanpn.org/geoserver/ows?service=wms&version=1.3.0&request=GetCapabilities"
  }

  res <- NULL
  tryCatch({
    res <- httr::GET(url)
  }, error = function(msg) {
    return(FALSE)
  })

  if (is.null(res) || res$status_code != 200) {
    return(FALSE)
  }

  return(TRUE)
}

#TODO do this a different way with an env var
get_skip_long_tests <- function() {
  return(TRUE)
}
