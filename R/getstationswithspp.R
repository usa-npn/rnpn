# getstationswithspp.R

getstationswithspp <- 
# Args:
#   speciesid: species id numbers, from 1 to infinity, potentially, 
#     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
# Examples:
#   getstationswithspp(c(52,53))

function(speciesid = NA, 
  url = 'http://www-dev.usanpn.org/npn_portal/stations/',
  method = 'getStationsWithSpecies',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  speciesidlist <- list()
  for(i in 1:length(speciesid)) {
    speciesidlist[i] <- paste('species_id[', i, ']=', speciesid[i], '&', sep='')
  }
  speciess <- str_c(laply(speciesidlist, paste), collapse = '')
  url3 <- paste(url2, speciess, sep = '')
  out <- fromJSON(getURLContent(url3))
  out2 <- ldply(out, function(x) as.data.frame(x))
  return(out2)
}