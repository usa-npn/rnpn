#' Get observation comments for a particular species or set of species.
#'
#' @export
#'
#' @param observationid Required. Observation id, or as vector if >1, as e.g., c(2, 1938) (numeric)
#' @param ... Optional additional curl options (debugging tools mostly)
#' @return Comments.
#' @examples \dontrun{
#' getobscomm(c(1,4,5,7,89))
#' }
getobscomm <- function(observationid, ...)
{
  args <- list()
  for(i in seq_along(observationid)) {
    args[paste('observation_id[',i,']',sep='')] <- observationid[i]
  }
  npn_GET(paste0(base(), 'observations/getObservationComment.json'), args, ...)
}
