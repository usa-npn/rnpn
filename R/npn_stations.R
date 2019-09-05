#' Get Station Data
#'
#' Get a list of all stations, optionally filtered by state
#'
#' @export
#' @param state_code The postal code of the US state by which to filter
#' the results returned. Leave empty to get all stations.
#' @template curl
#' @return Stations' latitude and longitude, names, and ids.
#' @examples \dontrun{
#' npn_stations()
#' npn_stations('AZ')
#' }

npn_stations <- function(state_code=NULL, ...) {
  end_point <- 'stations/getAllStations.json'
  if(!is.null(state_code)){
    tt <- lapply(state_code, function(z){
      npn_GET(paste0(base(), end_point), list(state_code = z), TRUE, ...)
    })
    ldfply(tt)
  }else{
    tibble::as_data_frame(
      npn_GET(paste0(base(), end_point), list(), TRUE, ...)
    )
  }
}





#' Get number of stations by state.
#'
#' @export
#' @template curl
#' @return Number of stations by state as a data.frame.
#' @examples \dontrun{
#' head( npn_stationsbystate() )
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


#' @export
npn_stations_by_location <- function( wkt, ...){

  end_point <- 'stations/getStationsByLocation.json'
  if(!is.null(wkt)){
    tt <- lapply(wkt, function(z){
      npn_GET(paste0(base(), end_point), list(wkt = z), TRUE, ...)
    })
    ldfply(tt)
  }else{
    tibble::as_data_frame(
      npn_GET(paste0(base(), end_point), list(), TRUE, ...)
    )
  }

}


#' Get Stations with Species
#'
#' Get a list of all stations which have an individual whom is a member of a
#'    set of species.
#'
#' @export
#' @param speciesid Required. Species id numbers, from 1 to infinity, potentially,
#'    use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @template curl
#' @return Stations' latitude and longitude, names, and ids.
#' @examples \dontrun{
#' npn_stationswithspp(speciesid = c(52,53,54))
#' npn_stationswithspp(speciesid = 53)
#' }

npn_stations_with_spp <- function(speciesid, ...) {
  args <- list()
  for (i in seq_along(speciesid)) {
    args[paste0('species_id[',i,']')] <- speciesid[i]
  }
  ldfply(npn_GET(paste0(base(), 'stations/getStationsWithSpecies.json'), args, ...))
}

