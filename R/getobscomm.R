# getobscomm.R

getobscomm <- 
# Args:
#   observationid: observation id, or as vector if >1, as e.g., c(2, 1938) (numeric)
# Examples:
#   getobscomm(1938)

function(observationid = NA,
  url = 'http://www-dev.usanpn.org/npn_portal/observations/',
  method = 'getObservationComment',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  obsidlist <- list()
  for(i in 1:length(observationid)) {
    obsidlist[i] <- paste('observation_id[', i, ']=', observationid[i], '&', sep='')
  }
  obsidss <- str_c(laply(obsidlist, paste), collapse = '')
  url3 <- paste(url2, obsidss, sep = '')  
  
  out <- fromJSON(getURLContent(url3))
  return(out)
}