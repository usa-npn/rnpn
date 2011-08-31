# getobsspbyday.R

getobsspbyday <- 
# Args:
#   speciesid: species id numbers, from 1 to infinity, potentially, 
#     use e.g., c(52, 53, etc.) if more than one species desired (numeric)
#   startdate: start date of data period desired, see format in examples (character)
#   enddate: end date of data period desired, see format in examples (character)
# Examples:
#   getobsspbyday(c(1, 2), '2008-01-01', '2011-12-31')

function(speciesid = NA, startdate = NA, enddate = NA,
  url = 'http://www-dev.usanpn.org/npn_portal/observations/',
  method = 'getObservationsForSpeciesByDay',
  ..., 
  curl = getCurlHandle() ) 
{
  url2 <- paste(url, method, '.json?', sep='')
  speciesidlist <- list()
  for(i in 1:length(speciesid)) {
    speciesidlist[i] <- paste('species_id[', i, ']=', speciesid[i], '&', sep='')
  }
  speciess <- str_c(laply(speciesidlist, paste), collapse = '')
  dates <- paste('start_date=', startdate, '&', 'end_date=', enddate, sep='')
  url3 <- paste(url2, speciess, dates, sep = '')  
  out <- fromJSON(getURLContent(url3))
  df <- ldply(out[[1]][[1]], function(x) ldply(x$count_list, function(x) as.data.frame(x)))
  df$species <- rep(paste('species', c(1,2)), times = sapply(out[[1]][[1]], function(x) length(x$count_list)))
  return(df)
}