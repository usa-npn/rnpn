# getpartners.R

getpartners <- 
# Args:
#   NONE
# Examples:
#   getpartners()

function(
  url = 'http://www.usanpn.org/npn_portal/networks/',
  method = 'getPartnerNetworks',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  out <- fromJSON(getURLContent(url2))
  df <- ldply(out, function(x) as.data.frame(x))
  return(df)
}