#' Get Phenophases
#'
#' Retrieves a complete list of all phenophases in the NPN database.
#' @param ... Currently unused.
#' @returns A tibble listing all phenophases available in the NPN database.
#' @export
#' @examples \dontrun{
#' phenophases <- npn_phenophases()
#' }
npn_phenophases <- function(...) {
  req <-
    base_req %>%
    httr2::req_url_path_append('phenophases/getPhenophases.json')
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return:
  tibble::as_tibble(out)
}

#' Get Phenophase Definitions
#'
#' Retrieves a complete list of all phenophase definitions.
#'
#' @param ... Currently unused.
#' @returns A tibble listing all phenophases in the NPN database and their
#'   definitions.
#' @export
#' @examples \dontrun{
#' pp <- npn_phenophase_definitions()
#' }
npn_phenophase_definitions <- function(...) {
  req <- base_req %>%
    httr2::req_url_path_append('phenophases/getPhenophaseDefinitionDetails.json')
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return:
  tibble::as_tibble(out)
}


#' Get Phenophase Details
#'
#' Retrieves additional details for select phenophases, including full list of
#' applicable phenophase definition IDs and phenophase revision notes over time
#'
#' @param ids Takes a vector of phenophase ids for which to retrieve additional
#'   details.
#' @param ... Currently unused.
#' @returns A tibble listing phenophases in the NPN database, including detailed
#'   information for each, filtered by the phenophase ID.
#' @export
#' @examples \dontrun{
#' pd <- npn_phenophase_details(c(56, 57))
#' }
npn_phenophase_details <- function(ids = NULL, ...) {
  if (!is.null(ids) & !is.numeric(ids)) {
    message("Invalid input, function expects a numeric vector as input")
    return(tibble::tibble())
  }
  req <- base_req %>%
    httr2::req_url_path_append('phenophases/getPhenophaseDetails.json') %>%
    httr2::req_url_query(ids = ids, .multi = "comma")

  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return:
  tibble::as_tibble(out)
}


#' Get Phenophase for Species
#'
#' Retrieves the phenophases applicable to species for a given date. It's
#' important to specify a date since protocols/phenophases for any given species
#' can change from year to year.
#' @param species_ids Integer vector of species IDs for which to get phenophase
#'   information.
#' @param date The applicable date for which to retrieve phenophases for the
#'   given species.
#' @param ... Currently unused.
#' @returns A tibble listing phenophases in the NPN database for the specified
#'   species and date.
#' @export
#' @examples \dontrun{
#' pp <- npn_phenophases_by_species(3, "2018-05-05")
#' }
npn_phenophases_by_species <- function(species_ids, date, ...) {
  species_ids <- npn_createArgList("species_id", species_ids)
  req <- base_req %>%
    httr2::req_url_path_append('phenophases/getPhenophasesForSpecies.json') %>%
    httr2::req_url_query(!!!species_ids) %>%
    httr2::req_url_query(date = date)

  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  #return:
  tibble::as_tibble(out)
  #TODO this data frame has a list-column.  Consider unnesting?
  # tidyr::unnest(tibble::as_tibble(out), phenophases)
}

#' Get Pheno Classes
#'
#' Gets information about all pheno classes, which are a higher-level order of
#' phenophases.
#'
#' @param ... Currently unused.
#' @returns A tibble listing the pheno classes in the NPN database.
#' @export
#' @examples \dontrun{
#' pc <- npn_pheno_classes()
#' }
npn_pheno_classes <- function(...) {
  req <- base_req %>%
    httr2::req_url_path_append('phenophases/getPhenoClasses.json')
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return:
  tibble::as_tibble(out)
}


#' Get Phenophases for Taxon
#'
#' This function gets a list of phenophases that are applicable for a provided
#' taxonomic grouping, e.g. family, order. Note that since a higher taxononmic
#' order will aggregate individual species not every phenophase returned through
#' this function will be applicable for every species belonging to that
#' taxonomic group.
#'
#' It's also important to note that phenophase definitions can change for
#' individual species over time, so there's a need to specify either a date of
#' interest, or to explicitly state that the function should return all
#' phenophases that were ever applicable for any species belonging to the
#' specified taxonomic group.
#'
#' When called, this function requires of these three parameters, exactly one of
#' `family_ids`, `order_ids` or `class_ids` to be set.
#'
#' @param family_ids Integer vector of taxonomic family ids to search for.
#' @param order_ids Integer vector of taxonomic order ids to search for.
#' @param class_ids Integer vector of taxonomic class ids to search for
#' @param genus_ids Integer vector of taxonomic genus ids to search for
#' @param date Specify the date of interest. For this function to return
#'   anything, either this value must be set or `return_all` must be `1`.
#' @param return_all Takes either `0` or `1` as input and defaults to `0`. For
#'   this function to return anything, either this value must be set to `1` or
#'   `date` must be set.
#' @param ... Currently unused.
#' @returns A data frame listing phenophases in the NPN database for the
#'   specified taxon and date.
#' @export
#' @examples \dontrun{
#' npn_get_phenophases_for_taxon(class_ids = c(5, 6), date = "2018-05-05")
#' npn_get_phenophases_for_taxon(family_ids = c(267, 268), date = "2018-05-05")
#'
#' #if you supply two or more "ids" arguments, the highest classification takes precedence
#' pheno <- npn_get_phenophases_for_taxon(
#'   class_ids = 4,
#'   family_ids = c(103, 104),
#'   genus_ids = c(409, 957, 610),
#'   date = "2018-05-05"
#' )
#'
#' colnames(pheno)
#' # [1] "family_id"   "family_name" "phenophases"
#' }
npn_get_phenophases_for_taxon <- function(family_ids = NULL,
                                          order_ids = NULL,
                                          class_ids = NULL,
                                          genus_ids = NULL,
                                          date = NULL,
                                          return_all = 0, #TODO switch to TRUE or FALSE?
                                          ...) {
  req <- base_req %>%
    httr2::req_url_path_append('phenophases/getPhenophasesForTaxon.json') %>%
    httr2::req_url_query(
      !!!explode_query("family_id", family_ids),!!!explode_query("class_id", class_ids),
      !!!explode_query("order_id", order_ids),!!!explode_query("genus_id", genus_ids),
      date = date,
      return_all = return_all
    )
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)

  #return:
  tibble::as_tibble(out)
  #TODO this data frame has a list-column.  Consider unnesting?
  # tidyr::unnest(tibble::as_tibble(out), phenophases)
}

#' Get Abundance Categories
#'
#' Gets data on all abundance/intensity categories and includes a data frame of
#' applicable abundance/intensity values for each category
#' @param ... Currently unused.
#' @returns A data frame listing all abundance/intensity categories and their
#'   corresponding values.
#' @export
#' @examples \dontrun{
#' ac <- npn_abundance_categories()
#' }
#'
npn_abundance_categories <- function(...) {
  req <- base_req %>%
    httr2::req_url_path_append('phenophases/getAbundanceCategories.json')
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #return:
  tibble::as_tibble(out)
  #TODO: consider unnesting
  #tibble::as_tibble(out) %>% tidyr::unnest(category_values)
}

