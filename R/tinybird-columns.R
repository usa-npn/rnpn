# Declared column types, one table per data product.
#
# The contract these enforce: **column types must not depend on which rows a
# query returns.** A fully-null column gets type-guessed where a populated one
# comes back numeric --- same code, different filter, different type. That is
# load-bearing rather than a corner case: 54% of real user searches return zero
# rows, and on magnitude phenometrics 20 of the 32 core columns are blanked by
# kingdom on every plant query.
#
# **Every table is keyed on lowercase names**, because the three metrics
# products emit legacy `PascalCase` on the wire and `tb_output_names()` renames
# the header before any type is looked up. See `R/tinybird-duckdb.R`.
#
# **Every table is a superset across grains and flags.** `taxon` and
# `phenophase_grain` change the core column set --- at `taxon = "genus"` the
# species block is replaced by a genus block --- so a superset keyed by name
# handles every combination with no branching. That is exactly what
# [dplyr::any_of()] was chosen for, and it is also why unknown columns pass
# through untouched: the dev schema gained two columns mid-session on
# 2026-08-14 and a strict spec enumerating an exact set would have broken that
# afternoon.
#
# Typing rules, applied uniformly: `*_id` and `*_sample_size` are integer,
# counts (`num*`) are integer, `mean_*` / `se_*` / `sd_*` / `proportion_*` are
# double, and everything whose shape is not certain is character. Character is
# the safe declaration for an unknown: it is lossless, where a wrong `integer`
# silently produces `NA` for every row.

#' Concatenate column-type blocks, rejecting a contradiction
#'
#' The blocks overlap by design --- `pheno_class_id` is a flag column on one
#' product and a core grain key on another --- so duplicate names are expected
#' and dropped. A duplicate name with two *different* types is a mistake, and
#' failing at build time is the only place it can be caught cheaply.
#' @noRd
tb_col_types <- function(...) {
  all <- c(...)
  if (length(all) == 0) {
    return(all)
  }
  for (nm in unique(names(all)[duplicated(names(all))])) {
    declared <- unique(unname(all[names(all) == nm]))
    if (length(declared) > 1) {
      stop(
        "Column `", nm, "` is declared as both ",
        paste(declared, collapse = " and "), ".",
        call. = FALSE
      )
    }
  }
  all[!duplicated(names(all))]
}

# --- shared blocks -----------------------------------------------------------

#' Per-download bookkeeping columns
#' @noRd
tb_cols_meta <- c(
  request_id = "character",
  product    = "character"
)

#' Site geography, core on status, individual and site phenometrics
#' @noRd
tb_cols_site_geo <- c(
  site_id             = "integer",
  latitude            = "double",
  longitude           = "double",
  elevation_in_meters = "integer",
  state               = "character"
)

#' The core taxon block at the default `taxon = "species"` grain
#'
#' `genus`, `species`, `common_name` and `kingdom` are core on every product and
#' are **not** gated by `include_taxonomic_detail`; that flag carries genus
#' *rank* metadata and above.
#' @noRd
tb_cols_taxon_core <- c(
  species_id  = "integer",
  genus       = "character",
  species     = "character",
  common_name = "character",
  kingdom     = "character"
)

#' The higher-taxonomy ranks
#'
#' These are the 11 `include_taxonomic_detail` columns on status and individual
#' phenometrics, and on site and magnitude phenometrics they are *also* the core
#' block at a coarser `taxon` grain. Same names, same types, one declaration.
#' @noRd
tb_cols_ranks <- c(
  class_id           = "integer",
  class_name         = "character",
  class_common_name  = "character",
  order_id           = "integer",
  order_name         = "character",
  order_common_name  = "character",
  family_id          = "integer",
  family_name        = "character",
  family_common_name = "character",
  genus_id           = "integer",
  genus_common_name  = "character"
)

#' `include_species_detail`, identical on all four products
#' @noRd
tb_cols_species_detail <- c(
  species_functional_type = "character",
  species_category        = "character",
  lifecycle_duration      = "character",
  growth_habit            = "character",
  usda_plants_symbol      = "character",
  itis_number             = "integer"
)

#' `include_phenophase_detail` on the three metrics products
#'
#' The two `pheno_class_*` columns are core rather than flag-gated at
#' `phenophase_grain = "pheno_class"` on site and magnitude. Same names either
#' way, so the declaration covers both.
#' @noRd
tb_cols_phenophase_detail <- c(
  phenophase_category = "character",
  pheno_class_id      = "integer",
  pheno_class_name    = "character"
)

#' `include_climate` on individual phenometrics
#'
#' The same 21 columns status data carries, taken at `first_yes_date`.
#' @noRd
tb_cols_climate_daily <- c(
  gdd          = "double",
  gddf         = "double",
  tmax         = "double",
  tmaxf        = "double",
  tmin         = "double",
  tminf        = "double",
  tmax_winter  = "double",
  tmax_spring  = "double",
  tmax_summer  = "double",
  tmax_fall    = "double",
  tmin_winter  = "double",
  tmin_spring  = "double",
  tmin_summer  = "double",
  tmin_fall    = "double",
  prcp         = "double",
  acc_prcp     = "double",
  prcp_winter  = "double",
  prcp_spring  = "double",
  prcp_summer  = "double",
  prcp_fall    = "double",
  daylength    = "double"
)

# --- status and intensity ----------------------------------------------------

#' Declared column types for status & intensity data
#'
#' Unchanged from M1 and deliberately so: this table plus the existing status
#' test suite is the regression lock for the M3 restructure. Only the core 20
#' columns are declared; the flag groups are left to pass through as they always
#' have, so that nothing about this product's behavior moves in a release that
#' was meant to add three others.
#' @noRd
status_col_types <- tb_col_types(
  tb_cols_meta,
  c(observation_id = "integer"),
  tb_cols_site_geo,
  tb_cols_taxon_core,
  c(
    individual_id          = "integer",
    phenophase_id          = "integer",
    phenophase_description = "character",
    observation_date       = "Date",
    day_of_year            = "integer",
    phenophase_status      = "integer",
    intensity_category_id  = "integer",
    intensity_value        = "character",
    abundance_value        = "integer"
  )
)

# --- individual phenometrics -------------------------------------------------

#' Declared column types for individual phenometrics
#'
#' 25 core columns plus every `include_*` group. `dataset_id`,
#' `observedby_person_id` and `lpl_certified_date` are **arrays on the wire** ---
#' an island spans many observations --- and are declared `character` for that
#' reason. Declaring `dataset_id` as `integer`, which its name and its
#' status-data namesake both suggest, produces a silent `NA` for every row.
#' @noRd
ipm_col_types <- tb_col_types(
  tb_cols_meta,
  tb_cols_site_geo,
  tb_cols_taxon_core,
  c(
    individual_id          = "integer",
    phenophase_id          = "integer",
    phenophase_description = "character",
    first_yes_year         = "integer",
    first_yes_month        = "integer",
    first_yes_day          = "integer",
    first_yes_doy          = "integer",
    first_yes_julian_date  = "integer",
    numdays_since_prior_no = "integer",
    last_yes_year          = "integer",
    last_yes_month         = "integer",
    last_yes_day           = "integer",
    last_yes_doy           = "integer",
    last_yes_julian_date   = "integer",
    numdays_until_next_no  = "integer"
  ),
  # include_submission (2) --- both arrays, positionally aligned to each other
  c(
    observedby_person_id = "character",
    lpl_certified_date   = "character"
  ),
  # include_observation_detail (3) --- dataset_id is an array
  c(
    observed_status_conflict_flag = "character",
    dataset_id                    = "character",
    partner_group                 = "character"
  ),
  # include_series_detail (4) --- IPM only
  c(
    numys_in_series    = "integer",
    numdays_in_series  = "integer",
    multiple_firsty    = "character",
    multiple_observers = "character"
  ),
  tb_cols_species_detail,
  tb_cols_ranks,
  c(site_name = "character"),
  c(plant_nickname = "character", patch = "character"),
  tb_cols_phenophase_detail,
  tb_cols_climate_daily
)

# --- site phenometrics -------------------------------------------------------

#' Declared column types for site phenometrics
#'
#' **The `mean_*` and `se_*` columns are `double`, not `integer`.** This is the
#' single most likely typing mistake in the package: their `_doy` and `_year`
#' names read as integers and their status-data siblings genuinely are.
#' @noRd
site_col_types <- tb_col_types(
  tb_cols_meta,
  tb_cols_site_geo,
  tb_cols_taxon_core,
  c(
    phenophase_id          = "integer",
    phenophase_description = "character",
    first_yes_sample_size  = "integer",
    last_yes_sample_size   = "integer"
  ),
  c(
    mean_first_yes_year         = "double",
    mean_first_yes_doy          = "double",
    mean_first_yes_julian_date  = "double",
    se_first_yes_in_days        = "double",
    mean_numdays_since_prior_no = "double",
    se_numdays_since_prior_no   = "double",
    mean_last_yes_year          = "double",
    mean_last_yes_doy           = "double",
    mean_last_yes_julian_date   = "double",
    se_last_yes_in_days         = "double",
    mean_numdays_until_next_no  = "double",
    se_numdays_until_next_no    = "double"
  ),
  # the coarser `taxon` grains promote these into core; the names do not change
  tb_cols_ranks,
  # include_observation_detail (3) --- the individual-ids column is an array
  c(
    partner_group                                = "character",
    observed_status_conflict_flag                = "character",
    observed_status_conflict_flag_individual_ids = "character"
  ),
  c(site_name = "character"),
  tb_cols_species_detail,
  tb_cols_phenophase_detail,
  # include_dispersion (10)
  c(
    sd_first_yes_in_days      = "double",
    min_first_yes_doy         = "integer",
    max_first_yes_doy         = "integer",
    median_first_yes_doy      = "double",
    sd_numdays_since_prior_no = "double",
    sd_last_yes_in_days       = "double",
    min_last_yes_doy          = "integer",
    max_last_yes_doy          = "integer",
    median_last_yes_doy       = "double",
    sd_numdays_until_next_no  = "double"
  ),
  # include_series_detail (2) --- the individual-ids column is an array
  c(
    num_individuals_with_multiple_firsty = "integer",
    multiple_firsty_individual_ids       = "character"
  ),
  # include_climate (20) --- a different set from the daily one on IPM
  c(
    mean_agdd        = "double",
    mean_agdd_in_f   = "double",
    se_agdd          = "double",
    se_agdd_in_f     = "double",
    tmax_winter      = "double",
    tmax_spring      = "double",
    tmax_summer      = "double",
    tmax_fall        = "double",
    tmin_winter      = "double",
    tmin_spring      = "double",
    tmin_summer      = "double",
    tmin_fall        = "double",
    prcp_winter      = "double",
    prcp_spring      = "double",
    prcp_summer      = "double",
    prcp_fall        = "double",
    mean_accum_prcp  = "double",
    se_accum_prcp    = "double",
    mean_daylength   = "double",
    se_daylength     = "double"
  )
)

# --- magnitude phenometrics --------------------------------------------------

#' Declared column types for magnitude phenometrics
#'
#' `start_date` and `end_date` are the only `Date` columns in any of the three
#' metrics products --- neither individual nor site phenometrics emits a date
#' column at all, only year/month/day/DOY integers.
#'
#' 20 of the 32 core columns are kingdom-conditional. For `Animalia`,
#' `numindividuals_with_yes_record` and
#' `proportion_individuals_with_yes_record` are NULL; for anything else,
#' `numsites_with_yes_record`, `proportion_sites_with_yes_record` and all 18
#' abundance columns are NULL. That fires on **every plant query**, which is the
#' majority case, and it is precisely the all-NULL-column failure these
#' declarations exist to prevent.
#'
#' The hyphens are real: legacy kept them
#' (`mean_num_animals_in-phase` in `vignette("V_magnitude_phenometrics")`), so
#' substituting `_` would be a divergence invented here rather than a fix. Such
#' columns need backticks in R: `` df$`total_numanimals_in-phase` ``.
#' @noRd
magnitude_col_types <- tb_col_types(
  tb_cols_meta,
  tb_cols_taxon_core,
  c(
    phenophase_id          = "integer",
    phenophase_description = "character",
    year                   = "integer",
    start_date             = "Date",
    end_date               = "Date"
  ),
  c(
    status_records_sample_size             = "integer",
    individuals_sample_size                = "integer",
    sites_sample_size                      = "integer",
    num_yes_records                        = "integer",
    numindividuals_with_yes_record         = "integer",
    numsites_with_yes_record               = "integer",
    proportion_yes_records                 = "double",
    proportion_individuals_with_yes_record = "double",
    proportion_sites_with_yes_record       = "double"
  ),
  # the 18 abundance columns, NULL for every non-animal query
  c(
    `in-phase_sites_sample_size`                       = "integer",
    `in-phase_site_visits_sample_size`                 = "integer",
    `total_numanimals_in-phase`                        = "double",
    `mean_numanimals_in-phase`                         = "double",
    `se_numanimals_in-phase`                           = "double",
    `in-phase_per_hr_sites_sample_size`                = "integer",
    `in-phase_per_hr_site_visits_sample_size`          = "integer",
    `mean_numanimals_in-phase_per_hr`                  = "double",
    `se_numanimals_in-phase_per_hr`                    = "double",
    `in-phase_per_hr_per_acre_sites_sample_size`       = "integer",
    `in-phase_per_hr_per_acre_site_visits_sample_size` = "integer",
    `mean_numanimals_in-phase_per_hr_per_acre`         = "double",
    `se_numanimals_in-phase_per_hr_per_acre`           = "double"
  ),
  # the coarser `taxon` grains promote these into core; the names do not change
  tb_cols_ranks,
  tb_cols_species_detail,
  tb_cols_phenophase_detail,
  # include_dispersion (3)
  c(
    `sd_numanimals_in-phase`                 = "double",
    `sd_numanimals_in-phase_per_hr`          = "double",
    `sd_numanimals_in-phase_per_hr_per_acre` = "double"
  ),
  # include_observation_detail (4)
  c(
    `in-phase_search_method`        = "character",
    `in-phase_per_hr_search_method` = "character",
    start_date_doy                  = "integer",
    end_date_doy                    = "integer"
  )
)

# --- header fixups -----------------------------------------------------------

#' Corrections applied after the mechanical `tolower()` rename
#'
#' Named `lowercased wire name -> legacy name`, applied in the same SELECT list
#' as the rename (and immediately after `names()<-` on the readr path), so both
#' engines produce identical names.
#'
#' **Empty, and measured to be empty.** Every export header was diffed against
#' a legacy download of the same query on 2026-08-25, and `tolower()` was exact
#' for all three products --- no column name needs correcting:
#'
#' | product | query | names only in the export | names only in legacy |
#' |---|---|:-:|---|
#' | individual | 2024, species 3 | 0 | the four rank ids |
#' | site | 2024, species 3 | 0 | 0 |
#' | magnitude | 2019, species 3, frequency 30 | 0 | the four rank ids |
#'
#' The rank ids (`class_id`, `order_id`, `family_id`, `genus_id`) are not a
#' naming difference: legacy emitted them implicitly and the new pipes gate them
#' behind `include_taxonomic_detail`, which is deliberate and documented.
#'
#' The one imperfection this mechanism was built for did not survive contact
#' with the service. `vignette("V_magnitude_phenometrics")` refers to
#' `proportion_yes_record` (singular) and `mean_num_animals_in-phase`, which
#' suggested a mapping was needed; `npn_download_magnitude_phenometrics()`
#' actually returns `proportion_yes_records` and `mean_numanimals_in-phase` ---
#' exactly what `tolower()` produces. The vignette text was stale, not the
#' mapping.
#'
#' The vectors stay in place because they cost nothing and the pipes can still
#' rename a column later. Re-run the diff after any column change.
#' @noRd
status_name_fixups <- character()

#' @rdname status_name_fixups
#' @noRd
ipm_name_fixups <- character()

#' @rdname status_name_fixups
#' @noRd
site_name_fixups <- character()

#' @rdname status_name_fixups
#' @noRd
magnitude_name_fixups <- character()
