#' Get all observations for a particular species or set of species.
#' @import RJSONIO RCurl stringr plyr XML
#' @param speciesid species id numbers, from 1 to infinity, potentially,
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param downform Download format, one of 'json' or 'xml'.
#' @param url the PLoS API url for the function (should be left to default)
#' @param method the API method (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Date when article was published.
#' @export
#' @examples \dontrun{
#' getallobssp(speciesid = c(52, 53), '2008-01-01', '2011-12-31')
#' getallobssp(speciesid = c(52, 53), '2008-01-01', '2011-12-31', 'xml')
#' }
getallobssp <-

function(speciesid = NA, startdate = NA, enddate = NA, downform = 'json',
  url = 'http://www-dev.usanpn.org/npn_portal/observations/getAllObservationsForSpecies',
  method = 'getAllObservationsForSpecies',
  ...,
  curl = getCurlHandle() )
{
  url2 <- paste(url, '.', downform, sep='')
  args <- list()
  if(!is.na(speciesid[1]))
    for(i in 1:length(speciesid)) {
      args[paste('species_id[',i,']',sep='')] <- speciesid[i]
    }
  if(!is.na(startdate))
    args$start_date <- startdate
  if(!is.na(enddate))
    args$end_date <- enddate
  tt <- getForm(url2,
    .params = args,
    ...,
    curl = curl)
  if(downform == 'json'){fromJSON(tt)} else{xmlTreeParse(tt)}
}