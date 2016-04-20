#' Get observation comments for a particular species or set of species.
#'
#' @export
#' @param observationid Required. Observation id, or as vector if >1, as e.g., c(2, 1938) (numeric)
#' @template curl
#' @return Comments.
#' @examples \dontrun{
#' npn_obscomm(c(1,4,5,7,89))
#' }

npn_obscomm <- function(observationid, ...) {
  args <- list()
  for (i in seq_along(observationid)) {
    args[paste0('observation_id[',i,']')] <- observationid[i]
  }
  npn_GET(paste0(base(), 'observations/getObservationComment.json'), args, ...)
}
