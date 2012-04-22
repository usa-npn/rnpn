#' Get observations by day for a particular species or set of species.
#'
#' @import RJSONIO RCurl stringr plyr XML
#' @param speciesid species id numbers, from 1 to infinity, potentially, 
#'     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#' @param startdate start date of data period desired, see format in examples (character)
#' @param enddate end date of data period desired, see format in examples (character)
#' @param format Output format, one of 'json', 'xml', or 'mysql'.
#' @param printdf Print data.frame (default, TRUE) or not (FALSE)
#' @param user If writemysql == TRUE, specify username for your MySQL login.
#' @param dbname If writemysql == TRUE, specify the database name in MySQL.
#' @param user If writemysql == TRUE, specify username for your MySQL login.
#' @param url the PLoS API url for the function (should be left to default)
#' @param ... optional additional curl options (debugging tools mostly)
#' @param curl If using in a loop, call getCurlHandle() first and pass
#'  the returned value in here (avoids unnecessary footprint)
#' @details Remember to turn on your MySQL server before using the 'mysql'
#'    in the format argument. 
#' @return Number of observations by day.
#' @export
#' @examples \dontrun{
#' getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31')
#' getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31', printdf = TRUE)
#' getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31', format = 'xml')
#' 
#' # Write to MySQL database. 
#' getobsspbyday(c(1, 2), '2011-11-01', '2011-12-31', format = 'mysql', 
#'  tablename='rnpntest', user='yourusername', dbname='yourdatabasename', 
#'  host='yourhostname', addprimkey=TRUE)
#' }
getobsspbyday <- function(speciesid = NA, startdate = NA, enddate = NA, 
  format = 'json', printdf = FALSE,
  tablename = NA, user = NA, dbname = NA, host = NA, addprimkey = NA,
  url = 'http://www.usanpn.org/npn_portal/observations/getObservationsForSpeciesByDay',
  ..., curl = getCurlHandle() ) 
{
  if(format == 'mysql'){ downform <- 'json' } else { downform <- format }
  url2 <- paste(url, '.', downform, sep='')
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
    ...,
    curl = curl)
  df <- llply(fromJSON(tt)$all_species$species, function(x) ldply(x[2]$count_list, identity))
  if(format == 'mysql'){ 
      names(df) <- speciesid
      dfsql <- ldply(df, identity)
      names(dfsql)[1] <- "species" 
      write_mysql(dat2write=dfsql, tablename=tablename, user=user, 
                  dbname=dbname, host=host, addprimkey=addprimkey) 
  } 
    else
  if(format == 'json'){
    if(printdf == TRUE){
      df
    } 
      else 
    {fromJSON(tt)}
  } 
    else
  if(format == 'xml') { 
    xmlParse(tt) 
  }
}