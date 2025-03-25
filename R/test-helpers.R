#' Runs a basic check to see if a valid response is returned by the NPN Portal
#' service and returns TRUE/FALSE. Used in unit tests to determine if tests
#' should be run
#' @returns `TRUE` if service is up, `FALSE` if service is down
#' @noRd
check_service <- function() {
  req <- base_req %>%
    httr2::req_url_path_append('species/getSpeciesById.json') %>%
    httr2::req_url_query(species_id = 3) %>%
    httr2::req_method("HEAD")
  resp <- httr2::req_perform(req)
  !httr2::resp_is_error(resp)
}

#' Runs a basic check to see if a valid response is returned by Geoserver and
#' returns TRUE/FALSE. Used in unit tests to determine if tests should be run
#'
#' @returns `TRUE` if service is up, `FALSE` if service is down
#' @noRd
check_geo_service <- function() {
  req <- httr2::request(base_geoserver_url) %>%
    httr2::req_user_agent("rnpn (https://github.com/usa-npn/rnpn/)") %>%
    httr2::req_url_path_append("ows") %>%
    httr2::req_url_query(
      service = "wms",
      version = "1.3.0",
      request = "GetCapabilities"
    ) %>% httr2::req_method("HEAD")
  resp <- httr2::req_perform(req)
  !httr2::resp_is_error(resp)
}

