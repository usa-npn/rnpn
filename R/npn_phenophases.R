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
  species_ids <- explode_query("species_id", species_ids)
  req <- base_req %>%
    httr2::req_url_path_append('phenophases/getPhenophasesForSpecies.json') %>%
    httr2::req_url_query(!!!species_ids) %>%
    httr2::req_url_query(date = date)

  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #unnest
  out <- tidyr::unnest(out, tidyr::any_of("phenophases"))
  #return:
  tibble::as_tibble(out)
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
#' @param date Specify the date of interest as a `Date` or `character` of the
#'   format `"YYYY-MM-DD"`. To return data from all dates, use `date = "all"`.
#' @param return_all Deprecated.  Use `date = "all"` to return data from all
#'   dates.
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
                                          return_all = deprecated(),
                                          ...) {
  if (lifecycle::is_present(return_all)) {
    lifecycle::deprecate_warn(
      when = "1.4.0",
      what = "npn_get_phenophases_for_taxon(return_all)",
      details = c("Please use `date = 'all'` to return data from all dates.")
    )
    if (return_all == 1) {
      rlang::warn("Implicitly setting `date = 'all'`")
      date <- "all"
    }
  }
  if (is.null(date)) {
    rlang::abort(c(
      "The `date` argument is required.",
      i = "Please supply a date or use `date = 'all'` to return data from all dates."
    ))
  }
  if (length(date) > 1) {
    rlang::abort(
      "Please supply a single date or use `date = 'all'` to return data from all dates."
    )
  }

  #Check that date is Date or character
  if (!(inherits(date, "Date") | is.character(date))) {
    #TODO could to more here to ensure that date is formatted correctly
    rlang::abort(
      "`date` must be a `Date` object, a string in the form of 'YYYY-MM-DD', or 'all'."
    )
  }

  # Handle date = "all" option
  return_all <- NULL
  if (is.character(date)) {
    if (date == "all") {
      return_all <- 1
    }
  }

  req <- base_req %>%
    httr2::req_url_path_append('phenophases/getPhenophasesForTaxon.json') %>%
    httr2::req_url_query(
      !!!explode_query("family_id", family_ids),!!!explode_query("class_id", class_ids),
      !!!explode_query("order_id", order_ids),!!!explode_query("genus_id", genus_ids),
      date = as.character(date), #allows for Date objects as input
      return_all = return_all
    )
  resp <- httr2::req_perform(req)
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  #unnest
  out <- tidyr::unnest(out, tidyr::any_of("phenophases"))
  #return:
  tibble::as_tibble(out)
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
  #unnest list column
  out <- tidyr::unnest(out, tidyr::any_of("category_values"))
  #return:
  tibble::as_tibble(out)
}

