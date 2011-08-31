# getindsatstations.R

getindsatstations <- 
# Args:
#   stationid: use e.g., c(4881, 4882, etc.) if more than one species desired (numeric)
# Examples:
#   getindsatstations(c(507, 523))

function(stationid = NA,
  url = 'http://www-dev.usanpn.org/npn_portal/individuals/',
  method = 'getIndividualsAtStations',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  stationidlist <- list()
  for(i in 1:length(stationid)) {
    stationidlist[i] <- paste('station_ids[', i-1, ']=', stationid[i], sep='')
  }
  stationss <- str_c(laply(stationidlist, paste), collapse = '&')
  url3 <- paste(url2, stationss, sep = '')  
  out <- fromJSON(getURLContent(url3))
  outt <- ldply(out, function(x) as.data.frame(x))
  return(outt)
}