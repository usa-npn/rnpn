#' Get Species
#'
#' Returns a complete list of all species information of species represented in
#' the NPN database.
#' @export
#' @param ... Currently unused.
#' @returns A tibble with information on species in the NPN database and their
#'   IDs.
#' @examples \dontrun{
#' npn_species()
#' }
npn_species <- function(...) {
  req <- base_req %>%
    httr2::req_url_path_append('species/getSpecies.json')
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return:
  tibble::as_tibble(out)
}

#' Get Species By ID
#'
#' Returns information about a species based on the NPN's unique ID for that
#' species
#' @export
#' @rdname npn_species
#' @param ids Integer vector of species ids for which to retrieve information.
#' @param ... Currently unused.
#' @returns A tibble with information on species in the NPN database and their
#'   IDs, filtered by the species ID parameter.
#' @examples \dontrun{
#' npn_species_id(ids = 3)
#' }
npn_species_id <- function(ids, ...) {
  req <- base_req %>%
    httr2::req_url_path_append('species/getSpeciesById.json')
  reqs <- lapply(ids, function(z)
    httr2::req_url_query(req, species_id = z))
  resps <- httr2::req_perform_sequential(reqs)
  out <- lapply(resps, function(x)
    httr2::resp_body_json(x, simplifyVector = TRUE) %>% tibble::as_tibble())

  #return
  dplyr::bind_rows(out)
}


#' Get Species By State
#'
#' Search for species by state
#'
#' @export
#' @rdname npn_species
#' @param state A US postal state code to filter results.
#' @param kingdom Character vector of taxonomic kingdoms to filter results by.
#'   Valid values include `'Animalia'` and `'Plantae'`.
#' @param ... Currently unused.
#' @returns A tibble with information on species in the NPN database whose
#'   distribution includes a given state.
#' @examples \dontrun{
#' npn_species_state(state = "AZ")
#' # only return plants
#' npn_species_state(state = "AZ", kingdom = "Plantae")
#' }
npn_species_state <- function(state, kingdom = c("Animalia", "Plantae"), ...) {
  # The API does accept multiple states in the form `?state[1]=CA&state[2]=AZ`.
  # However, it isn't clear which results belong to which state, so for now this only accepts a single state.
  # Entries other than US states appear to be valid, otherwise we could check for valid input with:
  # state <- rlang::arg_match(state, datasets::state.abb)

  kingdom <- rlang::arg_match(kingdom, multiple = TRUE)
  if (length(kingdom) == 2) {
    kingdom <- NULL #omitting kingom from API query returns results for both animals and plants
  }
  req <- base_req %>%
    httr2::req_url_path_append('species/getSpeciesByState.json') %>%
    httr2::req_url_query(state = state, kingdom = kingdom)

  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return:
  tibble::as_tibble(out)
}



#' Species Search
#'
#' Search NPN species information using a number of different parameters, which
#' can be used in conjunction with one another, including:
#'  - Species on which a particular group or groups are actually collecting data
#'  - What species were observed in a given date range
#'  - What species were observed at a particular station or stations
#' @param network filter species based on identifiers of NPN groups that are
#'   actually observing data on the species. Takes a single numeric ID.
#' @param start_date filter species by date observed. This sets the start date
#'   of the date range and must be used in conjunction with `end_date`.
#' @param end_date filter species by date observed. This sets the end date of
#'   the date range and must be used in conjunction with `start_date`.
#' @param station_id filter species by a numeric vector of unique site
#'   identifiers.
#' @param ... Currently unused.
#' @returns A tibble with information on species in the NPN database filtered by
#'   partner group, dates and station/site IDs.
#' @export
#' @rdname npn_species
#' @examples \dontrun{
#' species <- npn_species_search(
#'   start_date = "2013-01-01",
#'   end_date = "2013-05-15"
#' )
#' }
npn_species_search <- function(network = NULL,
                               start_date = NULL,
                               end_date = NULL,
                               station_id = NULL,
                               ...) {
  #TODO: multiple network IDs may be allowed in the API, but for now this function only takes a single network
  req <- base_req %>%
    httr2::req_url_path_append('species/getSpeciesFilter.json') %>%
    httr2::req_url_query(
      network = network,
      start_date = start_date,
      end_date = end_date,!!!explode_query("station_id", station_id)
    )
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return:
  tibble::as_tibble(out)
}

#' Get Species Types
#'
#' Return all plant or animal functional types used in the NPN database.
#'
#' @param kingdom Character vector of taxonomic kingdoms to filter results by.
#'   Valid values include `'Animalia'`, `'Plantae'`, or `c("Animalia",
#'   "Plantae")` (default).
#' @param ... Currently unused.
#' @returns A data frame with a list of the functional types used in the NPN
#'   database, filtered by the specified kingdom.
#' @export
#' @examples \dontrun{
#' npn_species_types("Plantae")
#' }
npn_species_types <- function(kingdom = c("Animalia", "Plantae"), ...) {
  kingdom <- rlang::arg_match(kingdom, multiple = TRUE)

  req_plant <- base_req %>%
    httr2::req_url_path_append('species/getPlantTypes.json')

  req_animal <- base_req %>%
    httr2::req_url_path_append('species/getAnimalTypes.json')

  req_list <- list(Animalia = req_animal, Plantae = req_plant)[kingdom]
  resps <-
    httr2::req_perform_sequential(req_list) %>%
    rlang::set_names(names(req_list))
  #iterate over responses to add a 'kindom' column and then rowbind them
  out <-
    Map(function(resp, k) {
      httr2::resp_body_json(resp, simplifyVector = TRUE) %>%
        dplyr::mutate(kingdom = k, .before = 1)
  }, resps, names(resps)) %>%
    dplyr::bind_rows()
  return(tibble::as_tibble(out))
}
