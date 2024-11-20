#' Get Station Data
#'
#' Get a list of all stations, optionally filtered by state
#'
#' @export
#' @param state_code The postal code of the US state by which to filter the
#'   results returned. Leave empty to get all stations.
#' @param ... currently unused
#' @return A data frame with stations' latitude and longitude, names, and ids.
#' @examples \dontrun{
#' npn_stations()
#' npn_stations('AZ')
#' }

npn_stations <- function(state_code=NULL, ...) {
  req <-
    base_req %>%
    httr2::req_url_path_append('stations/getAllStations.json')

  if (!is.null(state_code)) {
    state_code <- rlang::arg_match(state_code, datasets::state.abb, multiple = TRUE)
    reqs <- lapply(state_code, function(x) httr2::req_url_query(req, state_code = x))
    resps <- httr2::req_perform_sequential(reqs)
    tt <- lapply(resps, function(x) httr2::resp_body_json(x, simplifyVector = TRUE))
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
#' @template curl
#' @return A data frame listing stations by state.
#' @examples \dontrun{
#' head( npn_stations_by_state() )
#' }
npn_stations_by_state <- function(...) {
  tt <- npn_GET(paste0(base(), 'stations/getStationCountByState.json'), list(), ...)
  states <- sapply(tt, function(x){
    if (is.null(x[[1]]) == TRUE) {
      x[[1]] <- "emptyvalue"
    } else{
      x[[1]] <- x[[1]]
    }
  })
  data <- sapply(tt, "[[", "number_stations")
  structure(
    data.frame(states, data, stringsAsFactors = FALSE),
    .Names = c("state", "number_stations"))
}

#' Get station data based on a WKT defined geography.
#'
#' Takes a Well-Known Text based geography as input and returns data for all
#' stations, including unique IDs, within that boundary.
#'
#' @export
#' @param wkt Required field specifying the WKT geography to use.
#' @param ... currently unused
#' @return A data frame listing stations filtered based on the WKT geography.
#' @examples \dontrun{
#' head( npn_stations_by_state(wkt="POLYGON((
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
#' @param speciesid Required. Species id numbers, from 1 to infinity, potentially,
#'    use e.g., c(52, 53, etc.) if more than one species desired (numeric).
#' @param ... currently unused
#' @return A data frame with stations' latitude and longitude, names, and ids.
#' @examples \dontrun{
#' npn_stations_with_spp(speciesid = c(52,53,54))
#' npn_stations_with_spp(speciesid = 53)
#' }

npn_stations_with_spp <- function(speciesid, ...) {
  #TODO this doesn't work with speciesid = 3 (and possibly others) for some reason
  #https://github.com/usa-npn/rnpn/issues/38
  speciesid <- as.list(
    rlang::set_names(speciesid, paste0("species_id[", seq_along(speciesid), "]"))
  )
  req <- base_req %>%
    httr2::req_url_path_append('stations/getStationsWithSpecies.json') %>%
    httr2::req_url_query(!!!speciesid)
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  #return:
  tibble::as_tibble(out)
}

