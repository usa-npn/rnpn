#' Get observation comments for a particular species or set of species.
#'
#' @export
#'
#' @param observationid observation id, or as vector if >1, as e.g., c(2, 1938) (numeric)
#' @param callopts Optional additional curl options (debugging tools mostly)
#' @return Comments.
#' @examples \dontrun{
#' getobscomm(1938)
#' }
getobscomm <- function(observationid = NULL, callopts=list())
{
  if(is.null(observationid))
    stop("You must provide an observationid")

  url = 'https://www.usanpn.org/npn_portal/observations/getObservationComment.json'
  args <- list()
  for(i in seq_along(observationid)) {
    args[paste('observation_id[',i,']',sep='')] <- observationid[i]
  }
  tmp <- GET(url, query = args, callopts)
  stop_for_status(tmp)
  content(tmp)
}
