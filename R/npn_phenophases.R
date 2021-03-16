



#' Get Phenophases
#'
#' Retrieves a complete list of all phenophases in the NPN database
#' @template curl
#' @export
#'
npn_phenophases <- function( ...) {

  tibble::as_tibble(
    npn_GET(paste0(base(), 'phenophases/getPhenophases.json'), list(), TRUE, ...)
  )

}

#' Get Phenophase Definitions
#'
#' Retrieves a complete list of all phenophase definitions.
#'
#' @template curl
#' @export
npn_phenophase_definitions <- function ( ... ){
  tibble::as_tibble(
    npn_GET(paste0(base(), 'phenophases/getPhenophaseDefinitionDetails.json'), list(), TRUE, ...)
  )
}


#' Get Phenophase Details
#'
#' Retrieves additional details for select phenophases, including full list of applicable phenophase definition IDs and phenophase
#' revision notes over time
#'
#' @param ids Takes a list of phenophase ids for which to retrieve additional details.
#' @template curl
#' @export
npn_phenophase_details <- function (ids=list(), ...){

  if(typeof(ids) == "character"){
    message("Invalid input, function expects a list or double as input")
    return(NULL)
  }

  if(typeof(ids) == "double"){
    ids <- list(ids)
  }

  tibble::as_tibble(

    npn_GET(paste0(base(), 'phenophases/getPhenophaseDetails.json'), list(ids = paste(ids,sep="",collapse = ',')), TRUE, ...)
  )

}


#' Get Phenophase for Species
#'
#' Retrieves the phenophases applicable to species for a given date. It's important to specify a date since protocols/phenophases for
#' any given species can change from year to year
#' @param species_ids List of species_ids for which to get phenophase information
#' @param date The applicable date for which to retrieve phenophases for the given species
#' @template curl
#' @export
npn_phenophases_by_species <- function (species_ids, date, ...){
  arg_list<-npn_createArgList("species_id", species_ids)
  tibble::as_tibble(
    npn_GET(paste0(base(), 'phenophases/getPhenophasesForSpecies.json'), c(arg_list, date=date), TRUE, ...)
  )
}

#' Get Pheno Classes
#'
#' Gets information about all pheno classes, which a higher-level order of phenophases
#'
#' @export
#' @template curl
npn_pheno_classes <- function (...){
  tibble::as_tibble(
    npn_GET(paste0(base(), 'phenophases/getPhenoClasses.json'), list(), TRUE, ...)
  )
}


#' Get Phenophases for Taxon
#'
#'
#' This function gets a list of phenophases that are applicable for a provided taxonomic grouping, e.g. family, order.
#' Note that since a higher taxononmic order will aggregate individual species not every phenophase returned through this
#' function will be applicable for every species belonging to that taxonomic group.
#'
#' It's also important to note that phenophase definitions can change for individual species over time, so there's a need
#' to specify either a date of interest, or to explicitly state that the function should return all phenophases that were
#' ever applicable for any species belonging to the specified taxonomic group.
#'
#' When called, this function requires of these three parameters, exactly one of family_ids, order_ids or class_ids to be set.
#'
#' @param family_ids List of taxonomic family ids to search for.
#' @param order_ids List of taxonomic order ids to search for.
#' @param class_ids List of taxonomic class ids to search for
#' @param genus_ids List of taxonomic genus ids to search for
#' @param date Specify the date of interest. For this function to return anything, either this value must be set of return_all must be 1.
#' @param return_all Takes either 0 or 1 as input and defaults to 0. For this function to return anything, either this value must be set to 1
#' or date must be set.
#' @export
#' @template curl
npn_get_phenophases_for_taxon <- function (family_ids=NULL,order_ids=NULL,class_ids=NULL,genus_ids=NULL,date=NULL,return_all=0, ...){
  family_list <- npn_createArgList("family_id", family_ids)
  class_list <- npn_createArgList("class_id", class_ids)
  order_list <- npn_createArgList("order_id", order_ids)
  genus_list <- npn_createArgList("genus_id", genus_ids)
  npn_GET(paste0(base(), 'phenophases/getPhenophasesForTaxon.json'), c(family_list, class_list, order_list, genus_list, date=date,return_all=return_all), FALSE, ...)
}

#' Get Abundance Categories
#'
#' Gets data on all abundance/intensity categories and includes a data frame of
#' applicable abundance/intensity values for each category
#' @export
#' @template curl
npn_abundance_categories <- function ( ...){


  tibble::as_tibble(
    npn_GET(paste0(base(), 'phenophases/getAbundanceCategories.json'), list(), TRUE, ...)
  )

}

