#' Get observations by day for a particular species or set of species.
#'
#' @export
#' @import data.table methods
#' @importFrom httr GET stop_for_status content
#' @importFrom jsonlite fromJSON
#' @importFrom plyr llply ldply ddply summarise
#' @importFrom stringr str_replace
#'
#' @param speciesid species id numbers, from 1 to infinity, potentially,
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param callopts Optional additional curl options (debugging tools mostly)
#' @return Number of observations by day, in object of class npn.
#' @examples \dontrun{
#' # Lookup names
#' temp <- lookup_names(name='bird', type='common')
#' comnames <- temp[temp$species_id %in% c(357, 359, 1108), 'common_name']
#'
#' out <- getobsspbyday(speciesid=c(357, 359, 1108), startdate='2010-04-01',
#'    enddate='2013-09-31')
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

getobsspbyday <- function(speciesid = NULL, startdate = NULL, enddate = NULL, callopts=list())
{
  if(is.null(speciesid))
    stop("You must provide a speciesid")

  url = 'https://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesByDay.json'
  args <- npnc(list(start_date=startdate, end_date=enddate))
  for(i in seq_along(speciesid)) {
    args[paste('species_id[',i,']',sep='')] <- speciesid[i]
  }
  tmp <- GET(url, query = args, callopts)
  stop_for_status(tmp)
  tt <- content(tmp)
  df_list <- lapply(tt$all_species$species, function(x) rbindlist(lapply(x[2]$count_list, data.frame)))
  df_list <- llply(df_list, function(x){
      x$date <- str_replace(x$date, "\\s.+", "")
      x$count <- as.numeric(x$count)
      tt <- data.frame(x[,sum(count),by=date])
      names(tt) <- c('date','count')
      tt
    })

  names(df_list) <- speciesid
  return( df_list )
}
