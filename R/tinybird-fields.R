# Data tables describing the Tinybird export interface.
#
# Everything about *names* lives in this file and nowhere else. Three naming
# conventions are in play and no two of them agree: the R-facing argument, the
# export payload field, and the count-pipe parameter. Keeping the translations
# here as data means changing an argument name is an edit to a vector rather
# than surgery on the request builder.
#
# Everything product-specific is data too. `tb_products` carries a product's
# endpoint, its accepted payload fields, its enum-valued scalars, its column
# types, whether it requires dates, whether its header needs lowercasing, and
# how its size is estimated. Nothing in the core branches on which product it
# is holding --- adding a fifth is an entry here plus a wrapper function.

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

# --- accepted payload fields, per product ------------------------------------

#' Fields every product's mapper forwards
#'
#' These are exactly what `mapSharedFilters()` emits, plus the site filter,
#' plus the date pair. Anything a mapper does not forward is not here: see
#' `tb_props_status` for the nine that were removed.
#' @noRd
tb_props_shared <- c(
  "startDate",
  "endDate",
  "state",
  "bottom_left_x1",
  "bottom_left_y1",
  "upper_right_x2",
  "upper_right_y2",
  "species_ids",
  "network_ids",
  "dataset_ids",
  "phenophaseCategories",
  "stations",
  "kingdoms",
  "functional_types",
  "phenophase_categories",
  "class_ids",
  "order_ids",
  "family_ids",
  "genus_ids",
  "phenophase_ids",
  "pheno_class_ids",
  "person_ids",
  "downloadType"
)

#' Accepted payload fields for status & intensity data
#'
#' Nine fields M1 accepted are gone, verified by reading all four mappers:
#' `species_names`, `partnerGroups`, `integrated_datasets`, `qualityFlags`,
#' `additionalFields`, `additionalFieldsDisplay`, `ancillary_data`,
#' `bottom_left_constraint` and `upper_right_constraint` are declared by the
#' request schema and forwarded to Tinybird by **no** mapper on **any** product
#' (`statusDataMapper.ts:53-54` names most of them as deliberately dropped).
#' They passed M1's `...` validation and did nothing, which is the precise
#' failure that validation exists to prevent --- it just happened to be our list
#' that was wrong rather than the user's spelling.
#'
#' `individual_ids` is gone too: `statusDataMapper` does not emit it, and
#' `obs_search_keys.pipe` documents individual-level filtering as metrics-only.
#'
#' That leaves 32 fields, of which 31 are reachable filters and one is
#' `downloadType`.
#' @noRd
tb_props_status <- c(
  tb_props_shared,
  "observation_ids",
  "include_submission",
  "include_observation_detail",
  "include_species_detail",
  "include_taxonomic_detail",
  "include_site_detail",
  "include_individual_detail",
  "include_phenophase_detail",
  "include_climate"
)

#' @rdname tb_props_status
#' @noRd
tb_props_individual <- c(
  tb_props_shared,
  "individual_ids",
  "num_days_quality_filter_individual",
  "include_submission",
  "include_observation_detail",
  "include_series_detail",
  "include_species_detail",
  "include_taxonomic_detail",
  "include_site_detail",
  "include_individual_detail",
  "include_phenophase_detail",
  "include_climate"
)

#' @rdname tb_props_status
#' @noRd
tb_props_site <- c(
  tb_props_shared,
  "individual_ids",
  "num_days_quality_filter",
  "taxon",
  "phenophase_grain",
  "include_observation_detail",
  "include_species_detail",
  "include_taxonomic_detail",
  "include_site_detail",
  "include_phenophase_detail",
  "include_series_detail",
  "include_dispersion",
  "include_climate"
)

#' @rdname tb_props_status
#' @noRd
tb_props_magnitude <- c(
  tb_props_shared,
  "observation_ids",
  "frequency",
  "taxon",
  "phenophase_grain",
  "include_observation_detail",
  "include_species_detail",
  "include_taxonomic_detail",
  "include_phenophase_detail",
  "include_dispersion"
)

#' Whether a payload field is a JSON array
#'
#' A property of the *field*, not of the product, so one table serves all four.
#' This is what stops `jsonlite`'s `auto_unbox` from collapsing a
#' single-element filter to a scalar and sending `"species_ids": 3` where the
#' spec asks for `[3]`.
#' @noRd
tb_field_kinds <- c(
  startDate                          = "scalar",
  endDate                            = "scalar",
  state                              = "array",
  bottom_left_x1                     = "scalar",
  bottom_left_y1                     = "scalar",
  upper_right_x2                     = "scalar",
  upper_right_y2                     = "scalar",
  species_ids                        = "array",
  network_ids                        = "array",
  dataset_ids                        = "array",
  phenophaseCategories               = "array",
  stations                           = "array",
  individual_ids                     = "array",
  observation_ids                    = "array",
  kingdoms                           = "array",
  functional_types                   = "array",
  phenophase_categories              = "array",
  class_ids                          = "array",
  order_ids                          = "array",
  family_ids                         = "array",
  genus_ids                          = "array",
  phenophase_ids                     = "array",
  pheno_class_ids                    = "array",
  person_ids                         = "array",
  taxon                              = "scalar",
  phenophase_grain                   = "scalar",
  frequency                          = "scalar",
  num_days_quality_filter            = "scalar",
  num_days_quality_filter_individual = "scalar",
  include_submission                 = "scalar",
  include_observation_detail         = "scalar",
  include_species_detail             = "scalar",
  include_taxonomic_detail           = "scalar",
  include_site_detail                = "scalar",
  include_individual_detail          = "scalar",
  include_phenophase_detail          = "scalar",
  include_series_detail              = "scalar",
  include_dispersion                 = "scalar",
  include_climate                    = "scalar",
  downloadType                       = "scalar"
)

#' Why a field valid on one product is invalid on another
#'
#' Keyed `"<product>.<field>"` first and bare `"<field>"` second, so a reason
#' that differs by product can say so and one that does not is written once.
#' Purely explanatory: an absent entry costs a line of the error message and
#' nothing else.
#' @noRd
tb_field_notes <- c(
  "magnitude.include_climate" = "`magnitude_metrics` aggregates across locations, so it has no climate columns.",
  "magnitude.include_site_detail" = "`magnitude_metrics` aggregates sites away, so there is no site to name.",
  "magnitude.individual_ids" = "`magnitude_metrics` has no individual-level filter.",
  "status.individual_ids" = "Individual-level filtering is a metrics-product filter; `obs_search_keys` does not accept it.",
  "site.include_submission" = "A site row spans many observers, so there is no single submission to report.",
  "site.include_individual_detail" = "A site row spans many individuals, so there is no nickname or patch to report.",
  observation_ids = "Only status data and magnitude phenometrics filter on observation ids \u2014 an island or a site summary spans many observations.",
  include_series_detail = "Only individual and site phenometrics have a phenological series.",
  include_dispersion = "Only site and magnitude phenometrics emit dispersion statistics.",
  taxon = "Only site and magnitude phenometrics have a taxonomic aggregation grain.",
  phenophase_grain = "Only site and magnitude phenometrics have a phenophase aggregation grain.",
  frequency = "Only magnitude phenometrics buckets its output over time.",
  num_days_quality_filter = "`num_days_quality_filter` is a site phenometrics filter; individual phenometrics spells it `num_days_quality_filter_individual`.",
  num_days_quality_filter_individual = "`num_days_quality_filter_individual` is an individual phenometrics filter; site phenometrics spells it `num_days_quality_filter`.",
  species_names = "Accepted by the request schema and forwarded by no mapper. Use `species_ids`.",
  additionalFields = "Accepted by the request schema and forwarded by no mapper.",
  ancillary_data = "Accepted by the request schema and forwarded by no mapper.",
  qualityFlags = "Accepted by the request schema and forwarded by no mapper.",
  partnerGroups = "Accepted by the request schema and forwarded by no mapper. Use `network_ids`.",
  integrated_datasets = "Accepted by the request schema and forwarded by no mapper. Use `dataset_ids`."
)

#' Payload fields the caller may never set
#'
#' `downloadType` identifies the product and is set by the function.
#' @noRd
tb_reserved_fields <- c("downloadType")

# --- the products ------------------------------------------------------------

#' Products reachable through the async export core
#'
#' One entry per data product, carrying **everything** that is specific to it.
#' The core takes the entry and never asks which product it is holding, which is
#' what makes a fifth product an entry rather than a code change.
#'
#' Fields:
#' * `name` --- the R-facing key, matching this entry's name in the list. Used
#'   in messages, in the job registry, and by `rnpn.preflight_ratios`.
#' * `key` --- the value of the `product` column the sink writes into every row.
#' * `label` --- how the product is named in an error message.
#' * `path`, `download_type` --- the endpoint and its `downloadType` value.
#' * `properties` --- the payload fields `...` accepts (section 8.1).
#' * `scalars` --- enum-valued arguments and their permitted values (section 8.2). The
#'   pipes silently fall back to their default on an unrecognized value, so
#'   `taxon = "Genus"` would return species-grain data labelled as nothing.
#' * `dates_required`, `windowed` --- see `tb_check_dates()`.
#' * `lower_names`, `col_types`, `name_fixups` --- ingest (sections 4 and 5).
#' * `bytes_per_row`, `estimate`, `precision` --- the advisory guard (section 7).
#'   `estimate = NULL` is the whole off switch for one product.
#'
#' `bytes_per_row` is measured, per product, on a real export at the default
#' flag set: 176.1 for status data on a 4,355,717-row export, and on 2026-08-25
#' 133.4 (individual, 4,868 rows), 189.3 (site, 1,314) and 196.3 (magnitude,
#' 30,237). Turning on `include_climate` or the taxonomic flags widens a row, so
#' these under-state a flag-heavy query. It feeds only the warning text, which
#' never blocks anything, so that is the right side to be wrong on.
#' @noRd
tb_products <- list(
  status = list(
    name           = "status",
    key            = "observations",
    label          = "status data",
    path           = "v1/data/observations/export",
    download_type  = "Status and Intensity",
    properties     = tb_props_status,
    scalars        = list(),
    dates_required = FALSE,
    windowed       = FALSE,
    lower_names    = FALSE,
    col_types      = status_col_types,
    name_fixups    = status_name_fixups,
    bytes_per_row  = 176,
    precision      = "exact",
    estimate       = tb_estimate_count(1)
  ),
  individual = list(
    name           = "individual",
    key            = "individual_metrics",
    label          = "individual phenometrics",
    path           = "v1/data/individual_phenometrics/export",
    download_type  = "Individual Phenometrics",
    properties     = tb_props_individual,
    scalars        = list(),
    dates_required = TRUE,
    windowed       = TRUE,
    lower_names    = TRUE,
    col_types      = ipm_col_types,
    name_fixups    = ipm_name_fixups,
    bytes_per_row  = 133,
    precision      = "estimate",
    estimate       = tb_estimate_count(20)
  ),
  site = list(
    name           = "site",
    key            = "site_metrics",
    label          = "site phenometrics",
    path           = "v1/data/site_phenometrics/export",
    download_type  = "Site Phenometrics",
    properties     = tb_props_site,
    scalars        = list(
      taxon            = c("species", "genus", "family", "order", "class"),
      phenophase_grain = c("phenophase", "pheno_class")
    ),
    dates_required = TRUE,
    windowed       = TRUE,
    lower_names    = TRUE,
    col_types      = site_col_types,
    name_fixups    = site_name_fixups,
    bytes_per_row  = 189,
    precision      = "estimate",
    estimate       = tb_estimate_count(115)
  ),
  magnitude = list(
    name           = "magnitude",
    key            = "magnitude_metrics",
    label          = "magnitude phenometrics",
    path           = "v1/data/magnitude_phenometrics/export",
    download_type  = "Magnitude Phenometrics",
    properties     = tb_props_magnitude,
    scalars        = list(
      # `none` is legitimate here and a ValidationError on site phenometrics,
      # so even the server's own answer is product-dependent.
      taxon            = c("species", "genus", "family", "order", "class", "none"),
      phenophase_grain = c("phenophase", "pheno_class")
    ),
    dates_required = TRUE,
    windowed       = FALSE,
    lower_names    = TRUE,
    col_types      = magnitude_col_types,
    name_fixups    = magnitude_name_fixups,
    bytes_per_row  = 196,
    precision      = "estimate",
    estimate       = tb_estimate_magnitude
  )
)

#' The product entry whose sink writes this `product` column value
#'
#' The inverse of `tb_products[[x]]$key`. Returns `NULL` for anything
#' unrecognized, so a server-side rename degrades to "cannot tell" rather than
#' to a wrong answer.
#' @noRd
tb_product_by_key <- function(key) {
  if (!is.character(key) || length(key) != 1 || is.na(key)) {
    return(NULL)
  }
  for (product in tb_products) {
    if (identical(product$key, key)) {
      return(product)
    }
  }
  NULL
}

# --- name translation --------------------------------------------------------

#' R-facing argument name -> export payload field
#'
#' `site_ids` is the R-facing name against three competing spellings: the legacy
#' argument is `station_ids`, the payload field is `stations`, and the returned
#' column is `site_id`. `site_ids` wins because it is the only one the user sees
#' again --- they filter on `site_ids` and then `group_by(site_id)`.
#'
#' Everywhere else **the service's spelling wins**: `taxon`, `phenophase_grain`,
#' `frequency`, `num_days_quality_filter`, `individual_ids`. Legacy's
#' `taxonomy_aggregate` is a boolean where `taxon` is a five-way enum, so the
#' legacy name cannot express the new argument without losing most of it.
#'
#' `phenophase_categories` is sent snake_case. `phenophaseCategories` is **not**
#' an alias for it: `sharedFilters.ts:115-125` maps `phenophaseCategories` to
#' `phenophase_short_names` and `phenophase_categories` to an exact-match filter
#' on `o.phenophase_category`. Two filters, two columns. The named argument
#' sends the snake_case one, which the count pipe also honors;
#' `phenophaseCategories` stays reachable through `...` as its own filter.
#' @noRd
tb_payload_names <- c(
  start_date                         = "startDate",
  end_date                           = "endDate",
  species_ids                        = "species_ids",
  phenophase_ids                     = "phenophase_ids",
  site_ids                           = "stations",
  individual_ids                     = "individual_ids",
  states                             = "state",
  phenophase_categories              = "phenophase_categories",
  taxon                              = "taxon",
  phenophase_grain                   = "phenophase_grain",
  frequency                          = "frequency",
  num_days_quality_filter            = "num_days_quality_filter",
  num_days_quality_filter_individual = "num_days_quality_filter_individual"
)

#' Export payload field -> count-pipe parameter
#'
#' Only fields the count pipe actually honors appear here; the pipe silently
#' drops anything it does not recognize, so unmapped filters are simply not
#' sent and the count becomes an over-estimate. That is acceptable because the
#' guard only warns.
#'
#' `obs_search_keys` declares 22 parameters. `site_ids` started working on
#' 2026-08-19, which retired M1's deliberate over-estimate for the 27.2% of
#' queries that carry it. `individual_ids` matters now that two products expose
#' it, and `phenophase_short_names` is where `phenophaseCategories` goes.
#' @noRd
tb_count_names <- c(
  startDate             = "start_date",
  endDate               = "end_date",
  state                 = "states",
  species_ids           = "species_ids",
  stations              = "site_ids",
  individual_ids        = "individual_ids",
  phenophase_ids        = "phenophase_ids",
  phenophaseCategories  = "phenophase_short_names",
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

#' The bounding box, which is all four coordinates or none
#' @noRd
tb_bbox_fields <- c(
  "bottom_left_x1",
  "bottom_left_y1",
  "upper_right_x2",
  "upper_right_y2"
)

# --- ingest ------------------------------------------------------------------

#' The declared types, in the two dialects that need them
#'
#' A product's `col_types` table is the single source of truth. R gets it
#' through `coerce_known_cols()` on the eager path; DuckDB gets it through a
#' cast list on the lazy path, where R cannot reach into the table.
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
#' `request_id` is the job id and `product` is the sink's product key. Two
#' constant character columns at 20M rows is ~320MB of pointers for zero
#' information, on exactly the requests already under memory pressure, so they
#' are dropped rather than returned. They are not attached as attributes either:
#' dplyr verbs drop attributes silently, which is a trap rather than a feature.
#'
#' `as = "path"` hands over the raw file with both columns present, and with the
#' header in whatever case the service sent. The asymmetry is deliberate.
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

#' Working-memory multiplier over the tibble size
#'
#' Accounts for copy-on-modify during a dplyr pipeline. The per-product tibble
#' cost is `bytes_per_row` in the product entry; 176.1 bytes/row was measured
#' for status data on a real 4,355,717-row export.
#' @noRd
tb_working_multiplier <- 1.9
