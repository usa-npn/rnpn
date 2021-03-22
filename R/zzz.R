
pkg.env <- new.env(parent = emptyenv())

#' Set Environment
#'
#' By default this library will call the NPN's production services
#' but in some cases it's preferable to access the development web services
#' so this function allows for manually setting the web service endpoints
#' to use DEV instead. Just pass in "dev" to this function to change the
#' endpoints to use.
#' @param env The environment to use. Should be "ops" or "dev"
#' @export
npn_set_env <- function (env = "ops"){
  pkg.env$remote_env <- env
}

get_test_env <- function(){
  return("dev")
}

get_skip_long_tests <- function(){
  return(TRUE)
}

#' Runs a basic check to see if
#' a valid response is returned by
#' the NPN Portal service
#' and returns TRUE/FALSE
#'
#' Used in unit tests to determine if
#' tests should be run
#'
#'
check_service <- function() {
  npn_set_env(get_test_env())
  url <- paste0(base(), 'species/getSpeciesById.json')
  args <- list(species_id = 3)
  res <- NULL
  tryCatch({
    res <- GET(url, query = args)
  },
  error=function(msg){
    return(FALSE)
  })

  if (is.null(res) || res$status_code != 200) {
    return(FALSE)
  }

  return(TRUE)
}

check_data_service <- function() {
  npn_set_env(get_test_env())
  url <- paste0(base_data_domain(), 'web-services/geo.html')
  res <- NULL
  tryCatch({
    res <- GET(url)
  },
  error=function(msg){
    return(FALSE)
  })

  if (is.null(res) || res$status_code != 200) {
    return(FALSE)
  }

  return(TRUE)
}

#' Runs a basic check to see if
#' a valid response is returned by
#' Geoserver and returns TRUE/FALSE
#'
#' Used in unit tests to determine if
#' tests should be run
#'
check_geo_service <- function() {

  if( !is.null(pkg.env$remote_env)){
    pkg.env$remote_env <- pkg.env$remote_env
  }else{
    pkg.env$remote_env <- "ops"
  }

  if(pkg.env$remote_env=="ops"){
    url <- "http://geoserver.usanpn.org/geoserver/ows?service=wms&version=1.3.0&request=GetCapabilities"
  }else{
    url <- "http://geoserver-dev.usanpn.org/geoserver/ows?service=wms&version=1.3.0&request=GetCapabilities"
  }

  tryCatch({
    res <- GET(url)
  },
  error=function(msg){
    return(FALSE)
  })

  if (res$status_code != 200) {
    return(FALSE)
  }

  return(TRUE)
}


base <- function(){

  if( !is.null(pkg.env$remote_env)){
    pkg.env$remote_env <- pkg.env$remote_env
  }else{
    pkg.env$remote_env <- "ops"
  }

  if(pkg.env$remote_env=="ops"){
      return('https://www.usanpn.org/npn_portal/')
  }else{
      return('https://www-dev.usanpn.org/npn_portal/')
  }
}

base_data_domain <- function(){

  if( !is.null(pkg.env$remote_env)){
    pkg.env$remote_env <- pkg.env$remote_env
  }else{
    pkg.env$remote_env <- "ops"
  }

  if(pkg.env$remote_env=="ops"){
    return('https://data.usanpn.org/')
  }else{
    return('https://data-dev.usanpn.org/')
  }
}

base_geoserver <- function(){

  if( !is.null(pkg.env$remote_env)){
    pkg.env$remote_env <- pkg.env$remote_env
  }else{
    pkg.env$remote_env <- "ops"
  }

  if(pkg.env$remote_env=="ops"){
    return('https://geoserver.usanpn.org/geoserver/wcs?service=WCS&version=2.0.1&request=GetCoverage&')
  }else{
    return('https://geoserver-dev.usanpn.org/geoserver/wcs?service=WCS&version=2.0.1&request=GetCoverage&')
  }
}

npnc <- function(l) Filter(Negate(is.null), l)

pop <- function(x, y) {
  x[!names(x) %in% y]
}

ldfply <- function(y){
  res <- lapply(y, function(x){
    x[ sapply(x, is.null) ] <- NA
    data.frame(x, stringsAsFactors = FALSE)
  })
  do.call(rbind.fill, res)
}

npn_GET <- function(url, args, parse = FALSE, ...) {
  res <- tryCatch(
    {
      tmp <- GET(url, query = args, ...)
      stop_for_status(tmp)
      tt <- content(tmp, as = "text", encoding = "UTF-8")
      if (nchar(tt) == 0) tt else jsonlite::fromJSON(tt, parse, flatten = TRUE)
    },
    error=function(cond){
      # If the service is down for some reason give the user
      # a message and return an empty list with n = 0
      message("Service is unavailable. Try again later!")
      tt <- "{\"nodata\":\"servicedown\"}"
      if (nchar(tt) == 0) tt else jsonlite::fromJSON(tt, parse, flatten = TRUE)
    }
  )

}

#Utility function. Helps create URL strings for requests to NPN data services in the format variable_name[number]=Value
npn_createArgList <- function(arg_name, arg_list){
  args <- list()
  for (i in seq_along(arg_list)) {
    args[paste0(arg_name,'[',i,']')] <- URLencode(toString(arg_list[i]))
  }
  return(args)
}


