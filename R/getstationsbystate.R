# getstationsbystate.R

getstationsbystate <- 
# Args:
#   NONE
# Examples:
#   getstationsbystate()

function(
  url = 'http://www.usanpn.org/npn_portal/stations/',
  method = 'getStationCountByState',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  out <- fromJSON(getURLContent(url2))
  states <- ldply(out, function(x) if(is.null(x[[1]]) == TRUE) {x[[1]] <- "emptyvalue"} 
        else{x[[1]] <- x[[1]]})
  data <- ldply(out, function(x) x[[2]])
  dfout <- data.frame(states, data)
  names(dfout) <- c("state", "number_stations")
  return(dfout)
}