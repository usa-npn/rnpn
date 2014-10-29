#' Get all observations for a particular species or set of species.
#'
#' @export
#'
#' @param stationid Required. Use e.g., c(4881, 4882, etc.) if more than one species desired
#' (numeric)
#' @param ... Optional additional curl options (debugging tools mostly)
#' @return Observations for each species by date.
#' @examples \donttest{
#' getindsatstations(stationid = c(507, 523))
#' }
getindsatstations <- function(stationid, ...)
{
  args <- list()
  for(i in seq_along(stationid)) {
    args[paste('station_ids[',i,']',sep='')] <- stationid[i]
  }
  tt <- npn_GET(paste0(base(), 'individuals/getIndividualsAtStations.json'), args, ...)
  ldfply(tt)
}
