# getstationbyid.R


#
#   FOR SOME REASON THIS DOESN'T WORK, PROBLEM ON SERVER SIDE I THINK
#

getstationbyid <- 
# Args:
#   station: station ID (numeric)
# Examples:
#   getstationbyid(5122)

function(station, 
  url = 'http://www.usanpn.org/npn_portal/stations/',
  method = 'getStationById',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  stationn <- paste('station_id=', station, sep='')
  url3 <- paste(url2, stationn, sep = '')  
  out <- fromJSON(getURLContent(url3))
  return(out)
}