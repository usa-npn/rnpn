#' Write data to MySQL.
#' @import RMySQL 
#' @param dat2write data.frame to write to mysql
#' @param tablename What do you want to name the new table in mysql?
#' @param user Your username for MySQL (e.g., ).
#' @param dbname The MySQL database you want to write the data.frame in to.
#' @param host Your host name (e.g., ). 
#' @param addprimkey Add primary key or not (default = TRUE). 
#' @return Data written to MySQL, and prints message saying so.
#' @details Use this function wrapped with suppressMessages() to suppress 
#'    messages.
#' @export
#' @examples \dontrun{
#' # Use this function on its own
#' dat <- data.frame(a=rnorm(10^2), b=rnorm(10^2))
#' write_mysql(dat, 'dat230', 'asdfaf', 'test', 'localhost', TRUE)
#'
#' # Use this function within another function
#' getobsspbyday()
#' }
write_mysql <- 

function(dat2write, tablename, user, dbname, host, addprimkey = TRUE) {
  
  require(RMySQL)
  
  drv <- dbDriver("MySQL")
  con <- dbConnect(drv, user = user, dbname = dbname, host = host)
# g1 <- dbGetQuery(con, "SELECT col_16,col_18 FROM dat WHERE col_9='Lonchura nana'")
# dbListFields(con, "dat2")
  
# Write a data.frame to MySQL w/o row names
  message('Writing your data.frame to MySQL...')
  dbWriteTable(con, tablename, dat2write, row.names=F)

# Append rows of data to a MySQL table w/o row names
  # dbWriteTable(con, "dat60", dftosql2, row.names=F, append=T)

  message('\n...and, your data.frame is written to MySQL.')
  
# Add a primary key
  if(addprimkey == TRUE)
  message(paste('\nAdding primary key to', tablename, '...', sep=' '))  
    dbSendQuery(con, 
      paste(
        "ALTER TABLE", tablename, "ADD COLUMN primkey int primary key auto_increment", 
        sep=' ')) 
  
  message(paste('\nPrimary key added to', tablename, sep=' '))
}