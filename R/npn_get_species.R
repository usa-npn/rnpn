

#' Get Species
#'
#' Returns a complete list of all species information of species represented in the NPN
#' database.
#' @export
#' @template curl
#' @return data.frame of species and their IDs
#' @examples \dontrun{
#' npn_species()
#' npn_species_id(ids = 3)
#' }
npn_species <- function(...) {
  tibble::as_tibble(
    npn_GET(paste0(base(), 'species/getSpecies.json'), list(), TRUE, ...)
  )
}

#' Get Species By ID
#'
#' Returns information about a species based on the NPN's unique ID for that species
#' @export
#' @rdname npn_species
#' @param ids List of species ids for which to retrieve information
#' @return data.frame of the species' information
npn_species_id <- function(ids, ...) {

  tt <- lapply(ids, function(z){
    npn_GET(paste0(base(), 'species/getSpeciesById.json'), list(species_id = z), ...)
  })
  ldfply(tt)
}


#' Get Species By State
#'
#' Search for species by state
#'
#' @export
#' @rdname npn_species
#' @param state A US postal state code to filter results
#' @param kingdom Filters results by taxonomic kingdom. Takes either 'Animalia' or 'Plantae'
#' @examples \dontrun{
#' npn_species_state(state = "AZ")
#' npn_species_state(state = "AZ", kingdom = "Plantae")
#' }
npn_species_state <- function(state, kingdom = NULL, ...) {
  args <- npnc(list(state = state, kingdom = kingdom))
  ldfply(npn_GET(paste0(base(), 'species/getSpeciesByState.json'), args, ...))
}



#' Species Search
#'
#' Search NPN species information using a number of different parameters, which can be used in conjunction with one another, including:
#'  - Species on which a particular group or groups are actually collecting data
#'  - What species were observed in a given date range
#'  - What species were observed at a particular station or stations
#' @param network filter species based on a list of unique identifiers of NPN groups that are actually observing data on the species. Takes a list of IDs
#' @param start_date filter species by date observed. This sets the start date of the date range and must be used in conjunction with end_date
#' @param end_date filter species by date observed. This sets the end date of the date range and must be used in conjunction with start_date
#' @param station_id filter species by a list of unique site identifiers
#' @export
#' @rdname npn_species
npn_species_search <- function(network=NULL, start_date=NULL, end_date=NULL, station_id=NULL, ...) {
  args <- npnc(list(network_id = network, start_date = start_date,end_date = end_date))

  for (i in seq_along(station_id)) {
    args[paste0('station_ids[',i,']')] <- station_id[i]
  }

  ldfply(npn_GET(paste0(base(), 'species/getSpeciesFilter.json'), args, ...))
}

#' Get Species Types
#'
#' Return all plant or animal functional types used in the NPN database.
#'
#' @param kingdom The kingdom for which to return functional types; either 'Animalia' or 'Plantae'. Defaults to Plantae.
#' @template curl
#' @export
npn_species_types <- function(kingdom="Plantae", ...) {
  end_point = NULL

  if(kingdom == "Plantae"){
    end_point = 'species/getPlantTypes.json'

  }else if(kingdom == "Animalia"){
    end_point = 'species/getAnimalTypes.json'
  }

  if(!is.null(end_point)){
    tibble::as_tibble(
      npn_GET(paste0(base(), end_point), list(), TRUE, ...)
    )
  }else{
    plant_types <- tibble::as_tibble(
      npn_GET(paste0(base(), 'species/getPlantTypes.json'), list(), TRUE, ...)
    )

    animal_types <- tibble::as_tibble(
      npn_GET(paste0(base(), 'species/getAnimalTypes.json'), list(), TRUE, ...)
    )

    rbindlist(list(plant_types, animal_types))
  }

}
