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

#' Bind rows and *then* figure out column types
#'
#' Binds rows of dataframes safely by first converting all columns to character,
#' then binding, then parsing column types based on combined data.
#' @param ... Data frames to combine.
#' @param .id Passed to [dplyr::bind_rows()].
#' @noRd
bind_rows_safe <- function(..., .id = NULL) {
  dots <- rlang::dots_list(..., .named = TRUE)
  df_list <- purrr::map(dots, function(x) {
    x %>% dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  })
  dplyr::bind_rows(df_list, .id = .id) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), readr::parse_guess))
}
