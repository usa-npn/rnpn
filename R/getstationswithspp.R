#' Get a list of all stations which have an individual whom is a member of a
#'    set of species.
#'
#' @export
#'
#' @param speciesid species id numbers, from 1 to infinity, potentially,
#'    use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param printdf print data.frame (default, TRUE) or not (FALSE)
#' @param callopts Optional additional curl options (debugging tools mostly)
#'
#' @return Stations' latitude and longitude, names, and ids.
#' @examples \dontrun{
#' getstationswithspp(c(52,53,54))
#' getstationswithspp(c(52,53), printdf = FALSE)
#' }
getstationswithspp <- function(speciesid = NA, printdf = TRUE, callopts=list())
{
  if(is.null(speciesid))
    stop("You must provide an speciesid")

  url = 'https://www.usanpn.org/npn_portal/stations/getStationsWithSpecies.json'
  args <- list()
  for(i in seq_along(speciesid)) {
    args[paste('species_id[',i,']',sep='')] <- speciesid[i]
  }
  tmp <- GET(url, query = args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)
  if(printdf == TRUE){
    data.frame(do.call(rbind, tt))
  } else
    { tt }
}
