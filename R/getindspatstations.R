#' Get all observations for a particular species or set of species.
#'
#' @export
#'
#' @param speciesid species id numbers, from 1 to infinity, potentially,
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param stationid use e.g., c(4881, 4882, etc.) if more than one species desired (numeric)
#' @param year Year (numeric).
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param ... Optional additional curl options (debugging tools mostly)
#' @return Observations for each species by date.
#' @examples \dontrun{
#' getindspatstations(35, c(60, 259), 2009)
#' getindspatstations(35, c(60, 259), 2009, 'xml')
#' }
getindspatstations <-  function(speciesid = NULL, stationid = NULL, year = NULL,
  printdf = TRUE, ...)
{
  if(is.null(speciesid))
    stop("You must provide a speciesid")

  if(is.null(stationid))
    stop("You must provide a stationid")

  url = 'https://www.usanpn.org/npn_portal/individuals/getIndividualsOfSpeciesAtStations.json'
  args <- npnc(list(year = year))
  for(i in seq_along(speciesid)) {
    args[paste('species_id[',i,']',sep='')] <- speciesid[i]
  }
  for(i in seq_along(stationid)) {
    args[paste('station_ids[',i,']',sep='')] <- stationid[i]
  }
  tmp <- GET(url, query = args, ...)
  stop_for_status(tmp)
  tt <- content(tmp)
  if(printdf){
    data.frame(do.call(rbind, tt))
  } else
    {tt}
}
