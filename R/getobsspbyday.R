#' Get observations by day for a particular species or set of species.
#'
#' @import RJSONIO RCurl stringr plyr XML
#' @param speciesid species id numbers, from 1 to infinity, potentially, 
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @return Number of observations by day.
#' @export
#' @examples \dontrun{
#' # Lookup names
#' temp <- lookup_names(name='bird', type='common')
#' comnames <- temp[temp$species_id %in% c(357, 359, 1108), 'common_name']
#' 
#' out <- getobsspbyday(speciesid=c(357, 359, 1108), startdate='2010-04-01', enddate='2013-09-31')
#' names(out) <- comnames
#' df <- ldply(out)
#' df$date <- as.Date(df$date)
#' 
#' library(ggplot2)
#' ggplot(df, aes(date, count)) + 
#'  geom_line() +
#'  theme_grey(base_size=20) +
#'  facet_grid(.id ~.)
#' }
getobsspbyday <- function(speciesid = NA, startdate = NA, enddate = NA, ..., 
                          curl = getCurlHandle() ) 
{
  url = 'https://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesByDay'
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
  df_list <- llply(fromJSON(tt)$all_species$species, function(x) ldply(x[2]$count_list, identity))
  df_list <- llply(df_list, function(x){
      x$date <- str_replace(x$date, "\\s.+", "")
      x$count <- as.numeric(x$count)
      ddply(x, .(date), summarise, count=sum(count))
    })
  
#   df_list <- llply(df_list, function(x) ddply(x, .(date), summarise, count=sum(count)))
  names(df_list) <- speciesid
  return( df_list )
}