#' Species Name Lookup
#'
#' Look up species IDs by taxonomic or common name
#'
#' @export
#' @param name A scientific or common name
#' @param type One of common_name, genus, or species
#' @param fuzzy One of TRUE or FALSE, if FALSE, uses fuzzy search via agrep, if
#'    FALSE, uses grep
#' @examples \dontrun{
#' lookup_names(name='Pinus', type='genus')
#' lookup_names(name='pine', type='common_name')
#' lookup_names(name='bird', type='common_name', fuzzy=TRUE)
#' }
npn_lookup_names <- function(name, type = 'genus', fuzzy = FALSE) {

  if(!exists("species_list") || is.null(species_list)){
    species_list <<- npn_species()
  }

  type <- match.arg(type, choices = c('common_name','genus','species'))
  if (fuzzy) {
    species_list[agrep(name, species_list[, type]), ]
  } else {
    species_list[grep(name, species_list[, type]), ]
  }
}
