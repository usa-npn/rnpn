#' Get Partner Groups
#'
#' Returns a list of all groups participating in the NPN's data collection
#' program. These details can be used to further filter other service endpoints'
#' results.
#'
#' @param use_hierarchy Boolean indicating whether or not the list of networks
#'   should be represented in a hierarchy. If `TRUE`, the result will be
#'   returned as a nested list rather than a tibble. Defaults to `FALSE`.
#' @param ... Currently unused.
#' @returns A tibble (or nested list if `use_hierarchy = TRUE`) of partner
#'   groups, including `network_id` and `network_name`.
#' @export
#' @examples \dontrun{
#' npn_groups()
#' npn_groups(use_heirarchy = TRUE)
#' }
npn_groups <- function(use_hierarchy = FALSE, ...) {
  if (isTRUE(use_hierarchy)) {
    req <- base_req %>%
      httr2::req_url_path_append('networks/getNetworkTree.json')
  } else {
    req <- base_req %>%
      httr2::req_url_path_append('networks/getPartnerNetworks.json')
  }

  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = !use_hierarchy)
  #return:
  if (inherits(out, "data.frame")) {
    return(tibble::as_tibble(out))
  } else {
    return(out)
  }
}
