# getindspatstations.R

getindspatstations <- 
# Args:
#   speciesid: species id numbers, from 1 to infinity, potentially, 
#     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#   stationid: use e.g., c(4881, 4882, etc.) if more than one species desired (numeric)
#   year: year (character)
# Examples:
#   getindspatstations(35, c(60, 259), 2009)

function(speciesid = NA, stationid = NA, year = NA,
  url = 'http://www-dev.usanpn.org/npn_portal/individuals/',
  method = 'getIndividualsOfSpeciesAtStations',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  speciesidlist <- list()
  for(i in 1:length(speciesid)) {
    speciesidlist[i] <- paste('species_id[', i, ']=', speciesid[i], '&', sep='')
  }
  speciess <- str_c(laply(speciesidlist, paste), collapse = '')
  stationidlist <- list()
  for(i in 1:length(stationid)) {
    stationidlist[i] <- paste('station_ids[', i, ']=', stationid[i], '&', sep='')
  }
  stationss <- str_c(laply(stationidlist, paste), collapse = '')
  yearr <- paste('year=', year, '&', sep = '')
  url3 <- paste(url2, speciess, stationss, yearr, sep = '')  
  out <- fromJSON(getURLContent(url3))
  outt <- ldply(out, function(x) as.data.frame(x))
  return(outt)
}