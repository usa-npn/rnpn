#' Return all of the data, positive or negative, for an individual or a group
#'    of individuals, which are of a common species, at any number of locations.
#'
#' @export
#'
#' @param year Year (numeric), required.
#' @param stationid Station id; Use e.g., c(4881, 4882, etc.) if more than
#'    one species desired (numeric).
#' @param speciesid Species id number (numeric).
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param callopts Optional additional curl options (debugging tools mostly)
#' @return Data frame/json/xml of phenophas id's, phenophase names, sequence
#'    numbers, color, date, and observation id's.
#'
#' @examples \dontrun{
#' getobsspindloc(2009, c(4881, 4882), 3)
#' getobsspindloc(2009, c(4881, 4882), 3, 'xml')
#' }
getobsspindloc <- function(year = NULL, stationid = NULL, speciesid = NULL,
  printdf = TRUE, callopts=list())
{
  if(is.null(speciesid))
    stop("You must provide a speciesid")
  if(is.null(stationid))
    stop("You must provide a stationid")

  url = 'https://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesIndividualAtLocation.json'
  args <- npnc(list(year=year, speciesid=speciesid))
  for(i in seq_along(stationid)) {
    args[paste('station_ids[',i,']',sep='')] <- stationid[i]
  }
  tmp <- GET(url, query = args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)
  if(printdf){
    f <- function(lst) function(nm) unlist(lapply(lst, "[[", nm), use.names=FALSE)
    funcx <- function(lst1) {
      temp <- lapply(lapply(lst1$dates, function(x) unlist(x)), function(x) c(lst1[1:4], x))
      as.data.frame(Map(f(temp), names(temp[[1]])))
    }
    data.frame(do.call(funcx, tt))
  } else
    { tt }
}
