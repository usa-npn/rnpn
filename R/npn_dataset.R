#' Get Datasets
#'
#' Returns a complete list of information about all datasets integrated into the
#' NPN dataset. Data can then be pulled for individual datasets using their
#' unique IDs.
#' @export
#' @param ... Currently unused.
#' @return tibble of datasets and their IDs.
#' @examples \dontrun{
#' npn_datasets()
#' }
npn_datasets <- function(...) {
  req <- base_req %>%
    httr2::req_url_path_append('observations/getDatasetDetails.json')
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return
  tibble::as_tibble(out)
}
