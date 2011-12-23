#' Return all of the data, positive or negative, for an individual or a group 
#'    of individuals, which are of a common species, at any number of locations..
#' @import RJSONIO RCurl plyr XML
#' @param year Year.
#' @param stationid Station id; Use e.g., c(4881, 4882, etc.) if more than 
#'    one species desired (numeric).
#' @param speciesid Species id number (numeric). 
#' @param downform Download format, one of 'json' or 'xml'.
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param url the PLoS API url for the function (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'    the returned value in here (avoids unnecessary footprint)
#' @return Data frame/json/xml of phenophas id's, phenophase names, sequence 
#'    numbers, color, date, and observation id's.
#' @export
#' @examples \dontrun{
#' getobsspindloc(2009, c(4881, 4882), 3)
#' getobsspindloc(2009, c(4881, 4882), 3, 'xml')
#' }
getobsspindloc <- 

function(year = NA, stationid = NA, speciesid = NA, downform = 'json', 
  printdf = TRUE,
  url = 'http://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesIndividualAtLocation',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, '.', downform, sep='')
  args <- list()
  if(!is.na(stationid[1]))
    for(i in 1:length(stationid)) {
      args[paste('station_ids[',i,']',sep='')] <- stationid[i]
    }
  if(!is.na(speciesid))
    args$species_id <- speciesid
  if(!is.na(year))
    args$year <- year
  tt <- getForm(url2, 
                .params = args, 
                ...,
                curl = curl)
  if(downform == 'json'){
    out <- fromJSON(tt)  
    if(printdf == TRUE){
      f <- function(lst)
        function(nm) unlist(lapply(lst, "[[", nm), use.names=FALSE)
      funcx <- function(lst1) {
        temp <- lapply(lapply(lst1$dates, function(x) unlist(x)), function(x) c(lst1[1:4], x)) 
        as.data.frame(Map(f(temp), names(temp[[1]])))
      }
      ldply(out, funcx)
    } else
      {out}
  } else
    {xmlTreeParse(tt)}
}