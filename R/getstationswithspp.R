#' Get a list of all stations which have an individual whom is a member of a 
#'    set of species.
#' @import RJSONIO RCurl plyr XML
#' @param speciesid species id numbers, from 1 to infinity, potentially, 
#'    use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param downform Download format, one of 'json' or 'xml'.
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param url the PLoS API url for the function (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Stations' latitude and longitude, names, and ids.
#' @export
#' @examples \dontrun{
#' getstationswithspp(c(52,53,54))
#' getstationswithspp(c(52,53), printdf = FALSE)
#' }
getstationswithspp <- 

function(speciesid = NA, downform = 'json', printdf = TRUE,
  url = 'http://www.usanpn.org/npn_portal/stations/getStationsWithSpecies',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, '.json?', sep='')
  args <- list()
  if(!is.na(speciesid[1]))
    for(i in 1:length(speciesid)) {
      args[paste('species_id[',i,']',sep='')] <- speciesid[i]
    }
  tt <- getForm(url2,
    .params = args,
    ...,
    curl = curl)
  if(downform == 'json'){
    if(printdf == TRUE){
      ldply(fromJSON(tt), identity)
    } else 
      {fromJSON(tt)}
  } else 
    {xmlTreeParse(tt)}
}