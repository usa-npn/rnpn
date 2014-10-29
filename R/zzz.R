npnc <- function (l) Filter(Negate(is.null), l)

base <- function() 'https://www.usanpn.org/npn_portal/'

ldfply <- function(y){
  res <- lapply(y, function(x){
    x[ sapply(x, is.null) ] <- NA
    data.frame(x, stringsAsFactors = FALSE)
  })
  do.call(rbind.fill, res)
}

npn_GET <- function(url, args, ...){
  tmp <- GET(url, query = args, ...)
  stop_for_status(tmp)
  tt <- content(tmp, as = "text")
  jsonlite::fromJSON(tt, FALSE)
}
