#' Get observations by day for a particular species or set of species.
#' @import RJSONIO RCurl stringr plyr XML
#' @param speciesid species id numbers, from 1 to infinity, potentially, 
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param downform Download format, one of 'json' or 'xml'.
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param url the PLoS API url for the function (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Number of observations by day.
#' @export
#' @examples \dontrun{
#' getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31')
#' getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31', printdf = F)
#' getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31', downform = 'xml')
#' }
getobsspbyday <- 

function(speciesid = NA, startdate = NA, enddate = NA, downform = 'json', printdf = TRUE,
  url = 'http://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesByDay',
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
  if(downform == 'json'){
    if(printdf == TRUE){
      df <- llply(m$all_species$species, function(x) ldply(x[2]$count_list, identity))
      names(df) <- speciesid
      df
    } else 
      {fromJSON(tt)}
  } else 
    {xmlTreeParse(tt)}
}