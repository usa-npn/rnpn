#' Get observation comments for a particular species or set of species.
#'
#' @import RJSONIO RCurl plyr XML
#' @param observationid observation id, or as vector if >1, as e.g., c(2, 1938) (numeric)
#' @param downform Download format, one of 'json' or 'xml'.
#' @param url the PLoS API url for the function (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Comments.
#' @export
#' @examples \dontrun{
#' getobscomm(1938)
#' }
getobscomm <- function(observationid = NA, downform = 'json',
  url = 'http://www.usanpn.org/npn_portal/observations/getObservationComment',
  ..., curl = getCurlHandle() ) 
{
  url2 <- paste(url, '.', downform, sep='')
  args <- list()
  if(!is.na(observationid[1]))
    for(i in 1:length(observationid)) {
      args[paste('observation_id[',i,']',sep='')] <- observationid[i]
    }
  tt <- getForm(url2,
    .params = args,
    ...,
    curl = curl)
  if(downform == 'json'){fromJSON(tt)} else {xmlTreeParse(tt)}
}