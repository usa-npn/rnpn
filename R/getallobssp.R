#' Get all observations for a particular species or set of species.
#'
#' @import RJSONIO RCurl stringr plyr XML
#' @param speciesid species id numbers, from 1 to infinity, potentially,
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Date when article was published.
#' @export
#' @examples \dontrun{
#' # Lookup names
#' lookup_names(name='Pinus', type='genus')
#' 
#' # Get data on some species
#' getallobssp(speciesid = c(52, 53), startdate='2008-01-01', enddate='2011-12-31')
#' }
getallobssp <- function(speciesid = NA, startdate = NA, enddate = NA, ..., 
                        curl = getCurlHandle() )
{
  url = 'https://www.usanpn.org/npn_portal/observations/getAllObservationsForSpecies'
  method = 'getAllObservationsForSpecies'
  url2 <- paste(url, '.json', sep='')
  args <- list()
  if(!is.na(speciesid[1]))
    for(i in 1:length(speciesid)) {
      args[paste('species_id[',i,']',sep='')] <- speciesid[i]
    }
  if(!is.na(startdate))
    args$start_date <- startdate
  if(!is.na(enddate))
    args$end_date <- enddate
  tt <- getForm(url2,
    .params = args,
#     ...,
    curl = curl)
  out <- fromJSON(tt)
  ldply(out, )
}