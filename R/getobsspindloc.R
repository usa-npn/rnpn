#' Get a list of all stations which have an individual whom is a member of a 
#'    set of species.
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
#'  the returned value in here (avoids unnecessary footprint)
#' @return Stations' latitude and longitude, names, and ids.
#' @export
#' @examples \dontrun{
#' getobsspindloc(2009, c(4881, 4882), 3)
#' }
getobsspindloc <- 

function(year = NA, stationid = NA, speciesid = NA,
  url = 'http://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesIndividualAtLocation',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  stationidlist <- list()
  for(i in 1:length(stationid)) {
    stationidlist[i] <- paste('station_ids[', i, ']=', stationid[i], '&', sep='')
  }
  stationss <- str_c(laply(stationidlist, paste), collapse = '')
  yearr <- paste('year=', year, '&', sep = '')
  speciesidd <- paste('species_id=', speciesid, '&', sep='')
  url3 <- paste(url2, yearr, stationss, speciesidd, sep = '')  
  
  out <- fromJSON(getURLContent(url3))  
  
  f <- function(lst)
    function(nm) unlist(lapply(lst, "[[", nm), use.names=FALSE)

  funcx <- function(lst1) {
    temp <- lapply(lapply(lst1$dates, function(x) unlist(x)), function(x) c(lst1[1:4], x)) 
    as.data.frame(Map(f(temp), names(temp[[1]])))
  }
  outt <- ldply(out, funcx)
  return(outt)
}