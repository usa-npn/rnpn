


#' Get Datasets
#'
#' Returns a complete list of information about all datasets integrated into the NPN
#' dataset. Data can then be pulled for individual datasets using their unique IDs.
#' @export
#' @template curl
#' @return data.frame of datasets and their IDs.
#' @examples \dontrun{
#' npn_datasets()
#' }
npn_datasets <- function(...) {
  tibble::as_tibble(
    npn_GET(paste0(base(), 'observations/getDatasetDetails.json'), list(), TRUE, ...)
  )
}
