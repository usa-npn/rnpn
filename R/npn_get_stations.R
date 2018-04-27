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
