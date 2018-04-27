



#' Get Phenophases
#'
#' @template curl
#' @export
#'
npn_phenophases <- function( ...) {

  tibble::as_data_frame(
    npn_GET(paste0(base(), 'phenophases/getPhenophases.json'), list(), TRUE, ...)
  )

}

#' Get Phenophase Definitions
#' @template curl
#' @export
npn_phenophase_definitions <- function ( ... ){
  tibble::as_data_frame(
    npn_GET(paste0(base(), 'phenophases/getPhenophaseDefinitionDetails.json'), list(), TRUE, ...)
  )
}

#' @export
npn_phenophase_details <- function (ids, ...){
  tibble::as_data_frame(
    npn_GET(paste0(base(), 'phenophases/getPhenophaseDetails.json'), list(ids = paste(ids,sep="",collapse = ',')), TRUE, ...)
  )
}

#' @export
npn_phenophases_by_species <- function (species_ids, date, ...){
  arg_list<-npn_createArgList("species_id", species_ids)
  tibble::as_data_frame(
    npn_GET(paste0(base(), 'phenophases/getPhenophasesForSpecies.json'), c(arg_list, date=date), TRUE, ...)
  )
}

#' @export
npn_abundance_categories <- function ( ...){


  tibble::as_data_frame(
    npn_GET(paste0(base(), 'phenophases/getAbundanceCategories.json'), list(), TRUE, ...)
  )

}

