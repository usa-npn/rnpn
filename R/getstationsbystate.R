#' Get number of stations by state.
#'
#' @importFrom httr GET stop_for_status content
#' @importFrom plyr ldply
#' @param callopts Optional additional curl options (debugging tools mostly)
#' @return Number of stations by state as a data.frame.
#' @export
#' @examples \dontrun{
#' getstationsbystate()
#' }
getstationsbystate <- function(callopts=list())
{
  url = 'https://www.usanpn.org/npn_portal/stations/getStationCountByState.json'
  tmp <- GET(url, callopts)
  stop_for_status(tmp)
  out <- content(tmp)
  states <- ldply(out, function(x) if(is.null(x[[1]]) == TRUE) {x[[1]] <- "emptyvalue"} 
        else{x[[1]] <- x[[1]]})
  data <- ldply(out, function(x) x[[2]])
  dfout <- data.frame(states, data)
  names(dfout) <- c("state", "number_stations")
  dfout
}