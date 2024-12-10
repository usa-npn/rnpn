#' Species Name Lookup
#'
#' Look up species IDs by taxonomic or common name
#'
#' @export
#' @param name A scientific or common name.
#' @param type One of `"common_name"`, `"genus"`, or `"species"`.
#' @param fuzzy Logical; if `TRUE`, uses fuzzy search via [agrep()], if
#'   `FALSE`, uses [grep()].
#' @return A data frame with species ID numbers based on the name and type
#'   parameters.
#' @examples \dontrun{
#' npn_lookup_names(name='Pinus', type='genus')
#' npn_lookup_names(name='pine', type='common_name')
#' npn_lookup_names(name='bird', type='common_name', fuzzy=TRUE)
#' }
npn_lookup_names <- function(name, type = 'genus', fuzzy = FALSE) {

  if(is.null(pkg.env$species_list)){
    assign("species_list",npn_species(),envir = pkg.env)
  }

  type <- match.arg(type, choices = c('common_name','genus','species'))

  if (fuzzy) {
    pkg.env$species_list[agrep(name, pkg.env$species_list[, type]), ]
  } else {
    pkg.env$species_list[grep(name, pkg.env$species_list[[type]]), ]
  }
}
