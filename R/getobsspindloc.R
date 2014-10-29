#' Return all of the data, positive or negative, for an individual or a group
#'    of individuals, which are of a common species, at any number of locations.
#'
#' @export
#'
#' @param stationid Required. Station ID; Use e.g., c(4881, 4882, etc.) if more than
#'    one station desired (numeric).
#' @param speciesid Required. Species ID number (numeric).
#' @param year Required. Year (numeric). Default: 2010
#' @param individualid An individual's ID. (numeric)
#' @param ... Optional additional curl options (debugging tools mostly)
#' @return Data frame
#'
#' @examples \donttest{
#' getobsspindloc(stationid = c(4881, 4882), speciesid = 3, year = 2009)
#' getobsspindloc(stationid = c(4881, 4882), speciesid = 3)
#' getobsspindloc(stationid = 4881, speciesid = 67)
#' }

getobsspindloc <- function(stationid, speciesid, year=2010, individualid = NULL, ...) {
  args <- npnc(list(species_id=speciesid, individual_id=individualid, year=year))
  for(i in seq_along(stationid)) {
    args[paste('station_ids[',i,']',sep='')] <- stationid[i]
  }
  tt <- npn_GET(paste0(base(),
            'observations/getObservationsForSpeciesIndividualAtLocation.json'), args, ...)
  do.call(rbind.fill, lapply(tt, function(z){
    temp <- do.call(c, lapply(
      lapply(z$dates, function(m){
        if(length(m$observations) > 1){
          unname(Map(function(r,s) c(date=r, s), m$date, m$observations))
        } else {
          list(c(date=m$date, m$observations[[1]]))
        }
      }), function(bb) Map(function(x) c(z[1:4], x), bb)))
    ldfply(temp)
  })
  )
}
