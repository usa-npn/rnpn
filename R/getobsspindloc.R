# getobsspindloc.R

getobsspindloc <- 
# Args:
#   year: year (character)
#   stationid: use e.g., c(4881, 4882, etc.) if more than one species desired (numeric)
#   speciesid: species id number (numeric)
# Examples:
#   getobsspindloc(2009, c(4881, 4882), 3)

function(year = NA, stationid = NA, speciesid = NA,
  url = 'http://www-dev.usanpn.org/npn_portal/observations/',
  method = 'getObservationsForSpeciesIndividualAtLocation',
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
  funcx <- function(lst1) {
    temp <- lapply(lapply(lst1$dates, function(x) unlist(x)), function(x) c(lst1[1:4], x)) 
    as.data.frame(Map(f(temp), names(temp[[1]])))
  }
  outt <- ldply(out, funcx)
  return(outt)
}