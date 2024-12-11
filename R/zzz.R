
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
npn_set_env <- function (env = "ops") {
  pkg.env$remote_env <- env
}

get_test_env <- function() {
  return("dev")
}


# TODO: remove completely once we're sure error handling is equivalent or better
# Remember to remove from importFrom in rnpn-package.R also
# npn_GET <- function(url, args, parse = FALSE, ...) {
#   res <- tryCatch(
#     {
#       tmp <- httr::GET(url, query = args, ...)
#       httr::stop_for_status(tmp)
#       tt <- httr::content(tmp, as = "text", encoding = "UTF-8")
#       if (nchar(tt) == 0) tt else jsonlite::fromJSON(tt, parse, flatten = TRUE)
#     },
#     error=function(cond){
#       # If the service is down for some reason give the user
#       # a message and return an empty list with n = 0
#       message("Service is unavailable. Try again later!")
#       tt <- "{\"nodata\":\"servicedown\"}"
#       if (nchar(tt) == 0) tt else jsonlite::fromJSON(tt, parse, flatten = TRUE)
#     }
#   )
#
# }


