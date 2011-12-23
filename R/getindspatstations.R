#' Get all observations for a particular species or set of species.
#' @import RJSONIO RCurl plyr XML
#' @param speciesid species id numbers, from 1 to infinity, potentially, 
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param stationid use e.g., c(4881, 4882, etc.) if more than one species desired (numeric)
#' @param year Year (numeric).
#' @param downform Download format, one of 'json' or 'xml'.
#' @param url the PLoS API url for the function (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Observations for each species by date.
#' @export
#' @examples \dontrun{
#' getindspatstations(35, c(60, 259), 2009)
#' getindspatstations(35, c(60, 259), 2009, 'xml')
#' }
getindspatstations <- 

function(speciesid = NA, stationid = NA, year = NA, downform = 'json', printdf = TRUE,
  url = 'http://www.usanpn.org/npn_portal/individuals/getIndividualsOfSpeciesAtStations',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, '.', downform, sep='')
  args <- list()
  if(!is.na(speciesid[1]))
    for(i in 1:length(speciesid)) {
      args[paste('species_id[',i,']',sep='')] <- speciesid[i]
    }
  if(!is.na(stationid[1]))
    for(i in 1:length(stationid)) {
      args[paste('station_ids[',i,']',sep='')] <- stationid[i]
    }
  if(!is.na(year))
    args$year <- year
  tt <- getForm(url2,
                .params = args,
                ...,
                curl = curl)
  if(downform == 'json'){
    out <- fromJSON(tt)
      if(printdf == TRUE){
        ldply(out, identity)} else {out}
  } else
    {xmlTreeParse(tt)}
}