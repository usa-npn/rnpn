#' Get Station Data
#'
#' Get a list of all stations, optionally filtered by state
#'
#' @export
#' @param state_code The postal code of the US state by which to filter the
#'   results returned. Leave empty to get all stations.
#' @param ... Currently unused.
#' @returns A data frame with stations' latitude and longitude, names, and ids.
#' @examples \dontrun{
#' npn_stations()
#' npn_stations('AZ')
#' }
npn_stations <- function(state_code = NULL, ...) {
  req <-
    base_req %>%
    httr2::req_url_path_append('stations/getAllStations.json')

  if (!is.null(state_code)) {
    state_code <- rlang::arg_match(state_code, datasets::state.abb, multiple = TRUE)
    reqs <- lapply(state_code, function(x) {
      httr2::req_url_query(req, state_code = x)
    })
    resps <- httr2::req_perform_sequential(reqs)
    tt <- lapply(resps, function(x) {
      httr2::resp_body_json(x, simplifyVector = TRUE)
    })
    out <- dplyr::bind_rows(tt)
  } else {
    resp <- httr2::req_perform(req)
    out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  }

  #return:
  tibble::as_tibble(out)
}


#' Get number of stations by state.
#'
#' @export
#' @param ... Currently unused.
#' @returns A data frame listing stations by state.
#' @examples \dontrun{
#' head(npn_stations_by_state())
#' }
npn_stations_by_state <- function(...) {
  req <- base_req %>%
    httr2::req_url_path_append('stations/getStationCountByState.json')
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #Note: this function previously sanitized NULL values to "emptyvalue", but now
  #they are automatically coerced to NAs by httr2.  I think that's ok
  #return:
  tibble::as_tibble(out)
}

#' Get station data based on a WKT defined geography.
#'
#' Takes a Well-Known Text based geography as input and returns data for all
#' stations, including unique IDs, within that boundary.
#'
#' @export
#' @param wkt Required field specifying the WKT geography to use.
#' @param ... Currently unused.
#' @returns A data frame listing stations filtered based on the WKT geography.
#' @examples \dontrun{
#' head(npn_stations_by_state(wkt = "POLYGON((
#' -110.94484396954107 32.23623109416672,-110.96166678448247 32.23594069208043,
#' -110.95960684795904 32.21328646993733,-110.94244071026372 32.21343170728929,
#' -110.93935080547857 32.23216538049456,-110.94484396954107 32.23623109416672))")
#' )
#' }
npn_stations_by_location <- function(wkt, ...) {
  #TODO: check if wkt is valid with something like wk package?
  req <- base_req %>%
    httr2::req_url_path_append('stations/getStationsByLocation.json') %>%
    httr2::req_url_query(wkt = wkt)
  resp <- httr2::req_perform(req)
  #TODO: capture `response_message` and convert to error? E.g. if wkt = "hello"
  resp %>%
    httr2::resp_body_json(simplifyVector = TRUE) %>%
    tibble::as_tibble()
}


#' Get Stations with Species
#'
#' Get a list of all stations which have an individual whom is a member of a
#'    set of species.
#'
#' @export
#' @param species_id Required. Species id numbers, from 1 to infinity,
#'   potentially, use e.g., `c(52, 53)`, if more than one species desired
#'   (numeric).
#' @param ... Currently unused.
#' @param speciesid Deprecated. Use `species_id` instead.
#' @returns A data frame with stations' latitude and longitude, names, and ids.
#' @examples \dontrun{
#' npn_stations_with_spp(species_id = c(52, 53, 54))
#' npn_stations_with_spp(species_id = 53)
#' }

npn_stations_with_spp <- function(species_id, ..., speciesid = deprecated()) {
  if (lifecycle::is_present(speciesid)) {
    lifecycle::deprecate_warn("1.4.0", "npn_stations_with_spp(speciesid = )", "npn_stations_with_spp(species_id = )")
    species_id <- speciesid
  }
  #TODO this doesn't work with species_id = 3 (and possibly others) for some reason
  #https://github.com/usa-npn/rnpn/issues/38
  req <- base_req %>%
    httr2::req_url_path_append('stations/getStationsWithSpecies.json') %>%
    httr2::req_url_query(!!!explode_query("species_id", species_id))
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  #return:
  tibble::as_tibble(out)
}

