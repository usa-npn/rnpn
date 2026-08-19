# Data tables describing the Tinybird export interface.
#
# Everything about *names* lives in this file and nowhere else. Three naming
# conventions are in play and no two of them agree: the R-facing argument, the
# export payload field, and the count-pipe parameter. Keeping the translations
# here as data means changing an argument name is an edit to a vector rather
# than surgery on the request builder.

#' Base URLs, read through options
#'
#' Functions rather than constants so that `options()` set *after* the package
#' is loaded still take effect, which is what lets tests redirect the host
#' without touching code and what makes dev -> prod a one-line default change.
#' @noRd
base_npn_data_url <- function() {
  getOption("rnpn.data_url", "https://services2-dev.usanpn.org")
}

#' @rdname base_npn_data_url
#' @noRd
base_tinybird_url <- function() {
  # The *regional* host is mandatory; api.tinybird.co returns 403
  # workspace-not-found.
  getOption("rnpn.tinybird_url", "https://api.us-west-2.aws.tinybird.co")
}

#' Products reachable through the async export core
#'
#' One entry per data product. Adding the remaining three products (M3) is an
#' entry here plus a thin wrapper function; nothing in the core changes.
#' @noRd
tb_products <- list(
  status = list(
    path = "v1/data/observations/export",
    download_type = "Status and Intensity",
    count_pipe = "status_data_count"
  )
)

#' The 42 properties of `TinybirdExportRequest`
#'
#' Names are the accepted payload fields; values record whether the field is a
#' JSON array, which is what stops `jsonlite`'s `auto_unbox` from collapsing a
#' single-element filter to a scalar.
#'
#' `additionalProperties: false` is declared in the spec but **not enforced by
#' the server**: an unrecognized field returns 202 and is silently dropped, so a
#' typo widens the query instead of erroring. This vector is the only backstop
#' that exists.
#' @noRd
tb_export_properties <- c(
  startDate                  = "scalar",
  endDate                    = "scalar",
  state                      = "array",
  bottom_left_x1             = "scalar",
  bottom_left_y1             = "scalar",
  upper_right_x2             = "scalar",
  upper_right_y2             = "scalar",
  species_ids                = "array",
  species_names              = "array",
  network_ids                = "array",
  dataset_ids                = "array",
  phenophaseCategories       = "array",
  stations                   = "array",
  individual_ids             = "array",
  partnerGroups              = "array",
  integrated_datasets        = "array",
  kingdoms                   = "array",
  functional_types           = "array",
  phenophase_categories      = "array",
  class_ids                  = "array",
  order_ids                  = "array",
  family_ids                 = "array",
  genus_ids                  = "array",
  phenophase_ids             = "array",
  pheno_class_ids            = "array",
  person_ids                 = "array",
  observation_ids            = "array",
  include_submission         = "scalar",
  include_observation_detail = "scalar",
  include_species_detail     = "scalar",
  include_taxonomic_detail   = "scalar",
  include_site_detail        = "scalar",
  include_individual_detail  = "scalar",
  include_phenophase_detail  = "scalar",
  include_climate            = "scalar",
  downloadType               = "scalar",
  bottom_left_constraint     = "scalar",
  upper_right_constraint     = "scalar",
  additionalFields           = "array",
  additionalFieldsDisplay    = "array",
  ancillary_data             = "array",
  qualityFlags               = "scalar"
)

#' Payload fields the caller may never set
#'
#' `downloadType` identifies the product and is set by the function.
#' @noRd
tb_reserved_fields <- c("downloadType")

#' R-facing argument name -> export payload field
#'
#' `site_ids` is the R-facing name against three competing spellings: the legacy
#' argument is `station_ids`, the payload field is `stations`, and the returned
#' column is `site_id`. `site_ids` wins because it is the only one the user sees
#' again --- they filter on `site_ids` and then `group_by(site_id)`.
#'
#' `phenophase_categories` is sent snake_case only. The payload carries both
#' spellings and they are aliases on the export endpoint, but the count pipe
#' honors snake_case exclusively, so it is the only spelling that works on both
#' paths.
#' @noRd
tb_payload_names <- c(
  start_date            = "startDate",
  end_date              = "endDate",
  species_ids           = "species_ids",
  phenophase_ids        = "phenophase_ids",
  site_ids              = "stations",
  states                = "state",
  phenophase_categories = "phenophase_categories"
)

#' Export payload field -> count-pipe parameter
#'
#' Only fields the count pipe actually honors appear here; the pipe silently
#' drops anything it does not recognize, so unmapped filters are simply not
#' sent and the count becomes an over-estimate. That is acceptable because the
#' guard only warns.
#'
#' `stations` is the deliberate exception: it is mapped and sent even though the
#' pipe ignores it today. It is harmless now and starts working for free once
#' the server-side fix lands.
#' @noRd
tb_count_names <- c(
  startDate             = "start_date",
  endDate               = "end_date",
  state                 = "states",
  species_ids           = "species_ids",
  stations              = "site_ids",
  phenophase_ids        = "phenophase_ids",
  phenophase_categories = "phenophase_categories",
  pheno_class_ids       = "pheno_class_ids",
  network_ids           = "network_ids",
  dataset_ids           = "dataset_ids",
  person_ids            = "person_ids",
  observation_ids       = "observation_ids",
  kingdoms              = "kingdoms",
  functional_types      = "functional_types",
  class_ids             = "class_ids",
  order_ids             = "order_ids",
  family_ids            = "family_ids",
  genus_ids             = "genus_ids",
  bottom_left_x1        = "bottom_left_lng",
  bottom_left_y1        = "bottom_left_lat",
  upper_right_x2        = "upper_right_lng",
  upper_right_y2        = "upper_right_lat"
)

#' Declared column types for status & intensity data
#'
#' The contract this enforces: **column types must not depend on which rows a
#' query returns.** A fully-null column gets type-guessed where a populated one
#' comes back numeric --- same code, different filter, different type. That is
#' load-bearing rather than a corner case here, because 54% of real user
#' searches return zero rows.
#'
#' Unknown columns are deliberately absent and pass through untouched: the dev
#' schema gained two columns mid-session on 2026-08-14, and a strict spec
#' enumerating an exact set would have broken that afternoon.
#' @noRd
status_col_types <- c(
  request_id             = "character",
  product                = "character",
  observation_id         = "integer",
  site_id                = "integer",
  latitude               = "double",
  longitude              = "double",
  elevation_in_meters    = "integer",
  state                  = "character",
  species_id             = "integer",
  genus                  = "character",
  species                = "character",
  common_name            = "character",
  kingdom                = "character",
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

#' The declared types, in the two dialects that need them
#'
#' `status_col_types` is the single source of truth. R gets it through
#' `coerce_known_cols()` on the eager path; DuckDB gets it through a cast list
#' on the lazy path, where R cannot reach into the table.
#' @noRd
tb_coercers <- list(
  character = as.character,
  integer   = as.integer,
  double    = as.double,
  Date      = as.Date
)

#' @rdname tb_coercers
#' @noRd
tb_duckdb_types <- c(
  character = "VARCHAR",
  integer   = "INTEGER",
  double    = "DOUBLE",
  Date      = "DATE"
)

#' Per-download bookkeeping, constant for every row
#'
#' `request_id` is the job id and `product` is the product name. Two constant
#' character columns at 20M rows is ~320MB of pointers for zero information, on
#' exactly the requests already under memory pressure, so they are dropped
#' rather than returned. They are not attached as attributes either: dplyr verbs
#' drop attributes silently, which is a trap rather than a feature.
#'
#' `as = "path"` hands over the raw file with both columns present. The
#' asymmetry is deliberate.
#' @noRd
tb_metadata_cols <- c("request_id", "product")

#' Row count above which `tb_preflight()` warns
#'
#' The exact value genuinely does not matter: every threshold from 5M to 40M
#' catches the same 21-22 queries out of 3,094,874 genuine user searches. The
#' distribution has a body up to ~4M rows, one outlier at 11.7M, then 21
#' requests for the entire corpus. There is nothing in between to be precise
#' about.
#' @noRd
tb_row_warn_threshold <- 5e6

#' Measured memory cost of status data as a tibble
#'
#' 176.1 bytes/row measured on a real 4,355,717-row export, against a 163
#' bytes/row projection from a small sample. The working multiplier accounts for
#' copy-on-modify during a dplyr pipeline.
#' @noRd
tb_bytes_per_row <- 176

#' @rdname tb_bytes_per_row
#' @noRd
tb_working_multiplier <- 1.9
