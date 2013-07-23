#' Get all observations for a particular species or set of species.
#'
#' @import RJSONIO RCurl plyr XML
#' @param stationid use e.g., c(4881, 4882, etc.) if more than one species desired (numeric)
#' @param downform Download format, one of 'json' or 'xml'.
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param url the PLoS API url for the function (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Observations for each species by date.
#' @export
#' @examples \dontrun{
#' getindsatstations(c(507, 523))
#' getindsatstations(c(507, 523), 'xml')
#' }
getindsatstations <- function(stationid = NA, downform = 'json', printdf = TRUE,
  url = 'https://www.usanpn.org/npn_portal/individuals/getIndividualsAtStations',
  ..., curl = getCurlHandle() )
{
  url2 <- paste(url, '.', downform, sep='')
  args <- list()
  if(!is.na(stationid[1]))
    for(i in 1:length(stationid)) {
      args[paste('station_ids[',i,']',sep='')] <- stationid[i]
    }
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