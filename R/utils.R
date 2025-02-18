

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





#TODO: eventually remove this in favor of explode_query() once httr->httr2 is complete
# Helps create URL strings for requests to NPN data services in the format variable_name[number]=Value
npn_createArgList <- function(arg_name, arg_list) {
  args <- list()
  for (i in seq_along(arg_list)) {
    args[paste0(arg_name, '[', i, ']')] <- URLencode(toString(arg_list[i]))
  }
  return(args)
}

#' Null and empty coalescing operator
#'
#' Modified version of null coalescing operator (%||%) from rlang/soon to be in
#' base R. This version also coalesces empty lists and vectors
#' @param lhs left hand side; an object that is potentially NULL or of length 0
#' @param rhs right hand side; what to replace `lhs` with if NULL or length 0
#' @noRd
#' @examples
#' # with rlang's %||%
#'
#' NULL %||% 5
#' #> 5
#' character() %||% 5
#' #> character(0)
#'
#' # with modified version
#' NULL %|||% 5
#' #> 5
#' character() %|||% 5
#' #> 5
`%|||%` <- function(lhs, rhs) {
  if (is.null(lhs) | length(lhs) == 0) {
    rhs
  } else {
    lhs
  }
}

#' Explode multiple queries NPN style
#'
#' Alternative helper function that works in `httr2::req_url_query()` by naming
#' a vector `query_name[1]`, `query_name[2]`, etc
#' @noRd
#' @examples
#' species_id <- c(100, 103)
#' base_req %>%
#'   httr2::req_url_path_append('phenophases/getPhenophasesForSpecies.json') %>%
#'   httr2::req_url_query(!!!explode_query("species_id", species_id))
#'
explode_query <- function(arg_name, arg_vals) {
  if (!is.null(arg_vals)) {
    stats::setNames(arg_vals, paste0(arg_name, "[", seq_along(arg_vals), "]"))
  } else {
    NULL
  }
}

# year_start <- "12-315"
# validate_mmdd(year_start)
validate_mmdd <- function(x) {
  x_name <- deparse(substitute(x))
  mm_dd_check <- grepl("^\\d{2}-\\d{2}$", x)
  valid_date <- try(as.Date(paste0("2000-", x)), silent = TRUE)
  if (inherits(valid_date, "try-error") | isFALSE(mm_dd_check)) {
    stop("Please provide `", x_name, "` as 'MM-DD'.")
  }
}


