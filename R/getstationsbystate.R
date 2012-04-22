#' Get number of stations by state in a data frame.
#'
#' @import RJSONIO RCurl plyr XML
#' @param url the PLoS API url for the function (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Number of stations by state.
#' @export
#' @examples \dontrun{
#' getstationsbystate()
#' }
getstationsbystate <- function(
  url = 'http://www.usanpn.org/npn_portal/stations/getStationCountByState',
  ..., curl = getCurlHandle() ) 
{
  url2 <- paste(url, '.json?', sep='')
  out <- fromJSON(getURLContent(url2))
  states <- ldply(out, function(x) if(is.null(x[[1]]) == TRUE) {x[[1]] <- "emptyvalue"} 
        else{x[[1]] <- x[[1]]})
  data <- ldply(out, function(x) x[[2]])
  dfout <- data.frame(states, data)
  names(dfout) <- c("state", "number_stations")
  dfout
}