#' Get all observations for a particular species or set of species.
#'
#' @export
#'
#' @param stationid use e.g., c(4881, 4882, etc.) if more than one species desired (numeric)
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param callopts Optional additional curl options (debugging tools mostly)
#' @return Observations for each species by date.
#' @examples \dontrun{
#' getindsatstations(c(507, 523))
#' getindsatstations(c(507, 523), 'xml')
#' }
getindsatstations <- function(stationid = NULL, printdf = TRUE, callopts=list())
{
  if(is.null(stationid))
    stop("You must provide a stationid")

  url = 'https://www.usanpn.org/npn_portal/individuals/getIndividualsAtStations.json'
  args <- list()
  for(i in seq_along(stationid)) {
    args[paste('station_id[',i,']',sep='')] <- stationid[i]
  }
  tmp <- GET(url, query = args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)
  if(printdf){
    data.frame(do.call(rbind, tt))
  } else {tt}
}
