
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
