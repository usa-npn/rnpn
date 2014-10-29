#' Get all observations for a particular species or set of species.
#'
#' @export
#'
#' @param speciesid Required. Species id numbers, from 1 to infinity, potentially,
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param stationid Required. Use e.g., c(4881, 4882, etc.) if more than one species desired
#' (numeric)
#' @param year Year (numeric).
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param ... Optional additional curl options (debugging tools mostly)
#' @return Observations for each species by date.
#' @examples \dontrun{
#' getindspatstations(speciesid = 35, stationid = c(60, 259), year = 2009)
#' getindspatstations(35, c(60, 259), 2009, 'xml')
#' }
getindspatstations <-  function(speciesid, stationid, year = NULL, ...)
{
  args <- npnc(list(year = year))
  for(i in seq_along(speciesid)) {
    args[paste('species_id[',i,']',sep='')] <- speciesid[i]
  }
  for(i in seq_along(stationid)) {
    args[paste('station_ids[',i,']',sep='')] <- stationid[i]
  }
  tt <- npn_GET(paste0(base(), 'individuals/getIndividualsOfSpeciesAtStations.json'), args, ...)
  ldfply(tt)
}
