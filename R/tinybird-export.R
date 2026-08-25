# The generic async core, and the four products wired to it.
#
# `tb_run_export()` knows nothing about any particular product. Everything that
# differs between them --- the endpoint, the accepted filters, the enum values,
# whether dates are required, whether the header needs lowercasing, the column
# types, how the size is estimated --- is carried by the product entry in
# `tb_products`. Adding a fifth product is an entry there plus a wrapper that
# gathers arguments; nothing below changes.

#' Ensure a cache entry exists, downloading it if it does not
#'
#' @param fetch A function of one argument (the destination path) that puts the
#'   artifact there. The submit/poll/download path and the resume-a-job path
#'   differ only in this closure.
#' @noRd
tb_obtain <- function(key, refresh = FALSE, fetch) {
  path <- tb_cache_path(key)
  if (!refresh && tb_cache_fresh(path)) {
    message(
      "Using a cached copy of this export. ",
      "Pass `refresh = TRUE` to download it again."
    )
    return(path)
  }
  tb_ensure_dir(npn_cache_dir())
  fetch(path)
  path
}

#' Keep a durable copy at a user-chosen path
#'
#' `file_path` is orthogonal to `as` and determines only *where the artifact is
#' kept*. The two compose: `as = "data", file_path = "status.csv.gz"` means
#' "give me a tibble **and** keep a permanent copy".
#' @noRd
tb_copy_to <- function(src, file_path, call = rlang::caller_env()) {
  dest <- path.expand(file_path)
  parent <- dirname(dest)
  if (!dir.exists(parent)) {
    rlang::abort(
      c(
        "`file_path` points into a directory that does not exist.",
        "x" = paste0("No such directory: ", parent),
        "i" = "Create it first, or choose another path."
      ),
      call = call
    )
  }
  if (!file.copy(src, dest, overwrite = TRUE)) {
    rlang::abort(
      paste0("Could not write the export to ", dest, "."),
      call = call
    )
  }
  dest
}

#' Refuse to type a file against the wrong product's table
#'
#' Every export carries its sink `product` value in the rows, so the file can
#' say what it is. Checking that against what the caller asked for catches a
#' cache-key collision, which would otherwise present as a tibble full of `NA`
#' --- silently, since every column of the wrong table is simply absent from
#' [dplyr::any_of()].
#'
#' Says nothing when the file has no readable `product` value (a header-only
#' result has none) or when the value is not one this package knows, so a
#' server-side rename degrades to no check rather than to a false alarm.
#' @noRd
tb_check_file_product <- function(path, product, call = rlang::caller_env()) {
  found <- tb_product_by_key(tb_file_product_key(path))
  if (is.null(found) || identical(found$key, product$key)) {
    return(invisible(NULL))
  }
  rlang::abort(
    c(
      "The downloaded file is not the data product it was read as.",
      "x" = paste0(
        "It carries `product = \"", found$key, "\"` (", found$label,
        "), but it was about to be typed as ", product$label, "."
      ),
      "i" = "Pass `refresh = TRUE` to download this query again."
    ),
    call = call
  )
}

#' Return a cached artifact in the requested shape
#'
#' `as` is the sole determinant of the return type. Nothing else influences it
#' --- not data volume, not `file_path`, not whether the cache hit. A function
#' whose class depends on how much data came back breaks in production the day
#' someone widens a date range, with an error message that says nothing about
#' the cause.
#'
#' `product` may be `NULL` only for `as = "path"`, which hands over the raw file
#' and reads nothing.
#' @noRd
tb_deliver <- function(
  cache_path,
  key,
  as,
  file_path = NULL,
  product = NULL,
  call = rlang::caller_env()
) {
  out_path <- cache_path
  if (!is.null(file_path)) {
    out_path <- tb_copy_to(cache_path, file_path, call = call)
  }
  if (identical(as, "path")) {
    return(out_path)
  }
  tb_check_file_product(cache_path, product, call = call)
  switch(
    as,
    data = tb_read_data(cache_path, product),
    lazy = tb_lazy_tbl(cache_path, key, product)
  )
}

#' Submit an export, wait for it, and return it in the requested shape
#'
#' The product-agnostic core: submit -> poll -> download -> ingest -> return.
#' @noRd
tb_run_export <- function(
  product,
  payload,
  as,
  file_path = NULL,
  wait = TRUE,
  timeout = 300,
  refresh = FALSE,
  call = rlang::caller_env()
) {
  tb_check_engine(as, call = call)

  if (!wait) {
    # A bare job-id string, no S3 class. The cache is bypassed: the promise here
    # is a job id, and there is no job behind a cache hit.
    return(tb_submit_job(product, payload, call = call))
  }

  key <- tb_query_key(payload)
  path <- tb_obtain(
    key,
    refresh = refresh,
    fetch = function(dest) {
      if (identical(as, "data")) {
        tb_preflight(product, payload)
      }
      job_id <- tb_submit_job(product, payload, call = call)
      url <- tb_await_job(job_id, timeout = timeout, first_delay = 5, call = call)
      tb_fetch_artifact(url, dest, call = call)
    }
  )
  tb_deliver(path, key, as, file_path, product = product, call = call)
}

#' The body every `npn_export_*()` wrapper shares
#'
#' The wrappers exist to name their product's arguments and to document them.
#' Everything they then do is identical, so it happens once, here: validate
#' `as`, parse and check the dates against the product's own rules, build and
#' validate the payload, run the export.
#'
#' @param named Arguments keyed by R-facing name, for `tb_build_payload()`.
#' @noRd
tb_export <- function(
  product,
  named,
  dots,
  as,
  file_path = NULL,
  wait = TRUE,
  timeout = 300,
  refresh = FALSE,
  call = rlang::caller_env()
) {
  as <- rlang::arg_match(as, c("data", "path", "lazy"), error_call = call)

  named$start_date <- tb_parse_date(
    named$start_date, arg = "start_date", call = call
  )
  named$end_date <- tb_parse_date(
    named$end_date, arg = "end_date", call = call
  )
  tb_check_dates(product, named$start_date, named$end_date, call = call)

  payload <- tb_build_payload(
    named = named,
    dots = dots,
    product = product,
    call = call
  )

  tb_run_export(
    product = product,
    payload = payload,
    as = as,
    file_path = file_path,
    wait = wait,
    timeout = timeout,
    refresh = refresh,
    call = call
  )
}

#' Download status and intensity data
#'
#' Requests a status and intensity export, waits for the service to build it,
#' and returns it. The service builds exports asynchronously, so a call blocks
#' while the job runs --- typically a few seconds, longer for a wide date range.
#'
#' `r lifecycle::badge("experimental")` This is a prototype sibling of
#' [npn_download_status_data()], not a replacement for it. Its arguments may
#' change.
#'
#' @param start_date,end_date Optional bounds on the observation date, as `Date`
#'   objects or `"YYYY-MM-DD"` strings. Both are optional; omitting them
#'   requests the entire corpus, which is very large.
#' @param species_ids Optional vector of species ids to filter on. See
#'   [npn_species()].
#' @param phenophase_ids Optional vector of phenophase ids to filter on. See
#'   [npn_phenophases()].
#' @param site_ids Optional vector of site ids to filter on. These match the
#'   `site_id` column of the returned data. See [npn_stations()].
#' @param states Optional vector of two-letter state abbreviations. Cannot be
#'   combined with a bounding box.
#' @param phenophase_categories Optional vector of phenophase category names.
#' @param ... Additional filters, passed to the service as-is. Names are checked
#'   against the fields *this data product* accepts and an unrecognized name is
#'   an error, because the service itself silently ignores anything it does not
#'   recognize --- an unchecked typo would widen your query rather than fail it,
#'   and a flag outside a product's own whitelist is dropped just as quietly.
#'   Useful ones include `pheno_class_ids`, `network_ids`, `functional_types`,
#'   the `include_*` output-column flags (set them to `"1"`), and the bounding
#'   box fields `bottom_left_x1`, `bottom_left_y1`, `upper_right_x2`,
#'   `upper_right_y2`, which must be supplied all four or not at all.
#' @param as What to return. `"data"` (the default) returns a tibble; `"path"`
#'   returns the path to the downloaded `.csv.gz` file without reading it;
#'   `"lazy"` returns a duckdb-backed table you can query with dplyr verbs
#'   without loading it into memory. This argument is the *only* thing that
#'   determines the return type. `"data"` and `"lazy"` use duckdb; set
#'   `options(rnpn.engine = "readr")` to parse with readr instead and skip
#'   duckdb entirely, which rules out `"lazy"` but needs nothing installed.
#' @param file_path Optional path at which to keep a durable copy of the
#'   downloaded `.csv.gz`. Independent of `as`: you can ask for a tibble and a
#'   saved file in the same call. The file is gzipped CSV, so name it
#'   accordingly. When `NULL` (the default) the download is kept only in the
#'   package cache.
#' @param wait If `TRUE` (the default), block until the export is ready. If
#'   `FALSE`, return the job id immediately and collect it later with
#'   [npn_get_job()].
#' @param timeout Seconds to wait before giving up. Timing out is an error, not
#'   a return value, and it does not cancel the job --- the error message tells
#'   you how to collect it later.
#' @param refresh If `TRUE`, ignore any cached copy of this exact query and
#'   download it again.
#'
#' @returns A tibble, a file path, or a lazy table, according to `as`. When
#'   `wait = FALSE`, the job id as a character scalar.
#'
#' @section Large queries:
#' When `as = "data"`, the size of the result is checked before the export is
#' submitted and a warning is issued if it is unlikely to fit comfortably in
#' memory. For status data the number comes from a real count pipe and is
#' exact. The check is advisory: it never blocks a download, and if it cannot be
#' performed the download proceeds regardless. Switch it off entirely with
#' `options(rnpn.preflight = FALSE)`.
#'
#' @section Keeping a lazy table:
#' A lazy table is a live connection to a file on disk, not data. It cannot be
#' saved with [saveRDS()] and reloaded in another session --- doing so gives you
#' an object that errors with `Invalid connection`. This is how dplyr behaves
#' against every database backend. To keep results across sessions, either
#' [dplyr::collect()] the table into a tibble first, or use `as = "path"` and
#' keep the file.
#'
#' @seealso [npn_export_individual_phenometrics()],
#'   [npn_export_site_phenometrics()], [npn_export_magnitude_phenometrics()],
#'   [npn_get_job()], [npn_cache_clear()]
#' @export
#' @examples \dontrun{
#' # a tibble
#' obs <- npn_export_status_data(
#'   start_date = "2024-05-01",
#'   end_date = "2024-05-03",
#'   species_ids = 3
#' )
#'
#' # the same query, kept on disk and queried in place
#' tbl <- npn_export_status_data(
#'   start_date = "2024-05-01",
#'   end_date = "2024-05-03",
#'   species_ids = 3,
#'   as = "lazy"
#' )
#' tbl %>%
#'   dplyr::group_by(state, common_name) %>%
#'   dplyr::summarise(n = dplyr::n()) %>%
#'   dplyr::collect()
#' }
npn_export_status_data <- function(
  start_date = NULL,
  end_date = NULL,
  species_ids = NULL,
  phenophase_ids = NULL,
  site_ids = NULL,
  states = NULL,
  phenophase_categories = NULL,
  ...,
  as = c("data", "path", "lazy"),
  file_path = NULL,
  wait = TRUE,
  timeout = 300,
  refresh = FALSE
) {
  tb_export(
    product = tb_products$status,
    named = list(
      start_date = start_date,
      end_date = end_date,
      species_ids = species_ids,
      phenophase_ids = phenophase_ids,
      site_ids = site_ids,
      states = states,
      phenophase_categories = phenophase_categories
    ),
    dots = rlang::list2(...),
    as = as,
    file_path = file_path,
    wait = wait,
    timeout = timeout,
    refresh = refresh
  )
}

#' Download individual phenometrics
#'
#' Requests an individual phenometrics export, waits for the service to build
#' it, and returns it. One row is one phenological "island": an individual plant
#' or animal, one phenophase, one onset/end pair.
#'
#' `r lifecycle::badge("experimental")` This is a prototype sibling of
#' [npn_download_individual_phenometrics()], not a replacement for it. Its
#' arguments may change, and the geospatial arguments of the legacy function
#' (`six_leaf_layer`, `agdd_layer`, `wkt`, ...) have no equivalent here.
#'
#' @inheritParams npn_export_status_data
#' @param start_date,end_date **Required.** The bounds of the date range, as
#'   `Date` objects or `"YYYY-MM-DD"` strings. Unlike status data, this product
#'   cannot be requested without them: an unfiltered call would sort tens of
#'   millions of rows on the server.
#' @param individual_ids Optional vector of individual ids to filter on. These
#'   match the `individual_id` column of the returned data.
#'
#' @returns A tibble, a file path, or a lazy table, according to `as`. When
#'   `wait = FALSE`, the job id as a character scalar.
#'
#' @section Column names:
#' The service emits this product's columns in legacy `PascalCase`
#' (`First_Yes_DOY`). `as = "data"` and `as = "lazy"` return them lowercased
#' (`first_yes_doy`), which is what the legacy download and every vignette use.
#' `as = "path"` hands over the raw file with the header untouched.
#'
#' @section Seasonal windows:
#' The service decomposes `[start_date, end_date]` into one disjoint window per
#' phenological year, anchored on the **month-day** of the two dates. A
#' multi-year range therefore returns that season in each year, not the
#' continuous span between them:
#'
#' ```r
#' npn_export_individual_phenometrics(
#'   start_date = "2019-01-01",
#'   end_date = "2021-06-30"
#' )
#' # returns Jan 1 - Jun 30 of 2019, of 2020, and of 2021.
#' # July through December of 2019 and 2020 are NOT in the result.
#' ```
#'
#' This is the legacy function's `period_start` / `period_end` behavior arriving
#' through the date arguments instead of through two more of them. A range whose
#' `end_date` falls no later in the year than its `start_date` wraps the turn of
#' the year and must span at least one full year, or it contains no complete
#' window and is an error.
#'
#' [npn_export_magnitude_phenometrics()] is **never** windowed; do not carry
#' this conclusion across to it.
#'
#' @section Large queries:
#' As in [npn_export_status_data()], except that the number is an **estimate**:
#' no count pipe exists for this product, so the matching observation count is
#' scaled by a rule of thumb (~20 observations per island). Correct the ratio
#' with `options(rnpn.preflight_ratios = c(individual = 30))`, or switch the
#' check off with `options(rnpn.preflight = FALSE)`.
#'
#' @seealso [npn_export_status_data()], [npn_get_job()]
#' @export
#' @examples \dontrun{
#' ipm <- npn_export_individual_phenometrics(
#'   start_date = "2019-01-01",
#'   end_date = "2019-12-31",
#'   species_ids = 3
#' )
#' }
npn_export_individual_phenometrics <- function(
  start_date = NULL,
  end_date = NULL,
  species_ids = NULL,
  phenophase_ids = NULL,
  site_ids = NULL,
  individual_ids = NULL,
  states = NULL,
  phenophase_categories = NULL,
  ...,
  as = c("data", "path", "lazy"),
  file_path = NULL,
  wait = TRUE,
  timeout = 300,
  refresh = FALSE
) {
  tb_export(
    product = tb_products$individual,
    named = list(
      start_date = start_date,
      end_date = end_date,
      species_ids = species_ids,
      phenophase_ids = phenophase_ids,
      site_ids = site_ids,
      individual_ids = individual_ids,
      states = states,
      phenophase_categories = phenophase_categories
    ),
    dots = rlang::list2(...),
    as = as,
    file_path = file_path,
    wait = wait,
    timeout = timeout,
    refresh = refresh
  )
}

#' Download site phenometrics
#'
#' Requests a site phenometrics export, waits for the service to build it, and
#' returns it. One row summarizes one site, one taxon and one phenophase over
#' one phenological year --- the mean and standard error of the individual-level
#' onsets and ends at that site.
#'
#' `r lifecycle::badge("experimental")` This is a prototype sibling of
#' [npn_download_site_phenometrics()], not a replacement for it. Its arguments
#' may change, and the geospatial arguments of the legacy function have no
#' equivalent here.
#'
#' @inheritParams npn_export_status_data
#' @inheritParams npn_export_individual_phenometrics
#' @param taxon The taxonomic rank rows are aggregated to: one of `"species"`
#'   (the default), `"genus"`, `"family"`, `"order"` or `"class"`. This
#'   **changes the core column set** --- at `"genus"` the
#'   `species_id`/`species`/`common_name` block is replaced by
#'   `genus_id`/`genus`/`genus_common_name`. Supersedes the legacy
#'   `taxonomy_aggregate` argument, which was a boolean.
#' @param phenophase_grain What a row's phenophase is: `"phenophase"` (the
#'   default) or `"pheno_class"`. At `"pheno_class"`, `phenophase_id` and
#'   `phenophase_description` are replaced by `pheno_class_id` and
#'   `pheno_class_name`. Supersedes the legacy `pheno_class_aggregate`.
#' @param num_days_quality_filter The quality-control window in days: an
#'   individual's onset is included only when the preceding "no" is within this
#'   many days. A non-negative whole number; the service defaults to 30.
#'
#' @returns A tibble, a file path, or a lazy table, according to `as`. When
#'   `wait = FALSE`, the job id as a character scalar.
#'
#' @section Column names:
#' As in [npn_export_individual_phenometrics()]: `PascalCase` on the wire,
#' lowercased for `as = "data"` and `as = "lazy"`. Note that the `mean_*` and
#' `se_*` columns are doubles even where their names end in `_doy` or `_year`.
#'
#' @section Seasonal windows:
#' Identical to [npn_export_individual_phenometrics()], and load-bearing here
#' rather than merely convenient: the service groups by site, taxon and
#' phenophase grain with **no year in the key**, so an unwindowed three-year
#' call would return one cross-year mean where the windowed call returns three
#' rows. See that function's help topic for the worked example.
#'
#' @section Large queries:
#' As in [npn_export_individual_phenometrics()], with a ratio of ~115
#' observations per summary row. Override it with
#' `options(rnpn.preflight_ratios = c(site = 200))`.
#'
#' @seealso [npn_export_status_data()], [npn_get_job()]
#' @export
#' @examples \dontrun{
#' sites <- npn_export_site_phenometrics(
#'   start_date = "2019-01-01",
#'   end_date = "2019-12-31",
#'   species_ids = 3,
#'   taxon = "genus"
#' )
#' }
npn_export_site_phenometrics <- function(
  start_date = NULL,
  end_date = NULL,
  species_ids = NULL,
  phenophase_ids = NULL,
  site_ids = NULL,
  individual_ids = NULL,
  states = NULL,
  phenophase_categories = NULL,
  taxon = NULL,
  phenophase_grain = NULL,
  num_days_quality_filter = NULL,
  ...,
  as = c("data", "path", "lazy"),
  file_path = NULL,
  wait = TRUE,
  timeout = 300,
  refresh = FALSE
) {
  tb_export(
    product = tb_products$site,
    named = list(
      start_date = start_date,
      end_date = end_date,
      species_ids = species_ids,
      phenophase_ids = phenophase_ids,
      site_ids = site_ids,
      individual_ids = individual_ids,
      states = states,
      phenophase_categories = phenophase_categories,
      taxon = taxon,
      phenophase_grain = phenophase_grain,
      num_days_quality_filter = num_days_quality_filter
    ),
    dots = rlang::list2(...),
    as = as,
    file_path = file_path,
    wait = wait,
    timeout = timeout,
    refresh = refresh
  )
}

#' Download magnitude phenometrics
#'
#' Requests a magnitude phenometrics export, waits for the service to build it,
#' and returns it. One row summarizes one taxon and one phenophase over one time
#' bucket: how many records, how many were "yes", and --- for animals --- the
#' abundance statistics.
#'
#' `r lifecycle::badge("experimental")` This is a prototype sibling of
#' [npn_download_magnitude_phenometrics()], not a replacement for it. Its
#' arguments may change, and the geospatial arguments of the legacy function
#' have no equivalent here.
#'
#' @inheritParams npn_export_status_data
#' @inheritParams npn_export_individual_phenometrics
#' @inheritParams npn_export_site_phenometrics
#' @param taxon The taxonomic rank rows are aggregated to: one of `"species"`
#'   (the default), `"genus"`, `"family"`, `"order"`, `"class"` or `"none"`.
#'   `"none"` aggregates across taxa entirely and leaves `kingdom` as the only
#'   taxon column; it is accepted here and rejected by
#'   [npn_export_site_phenometrics()].
#' @param frequency The width of a time bucket: a positive whole number of days,
#'   or the string `"months"` for calendar months. The service defaults to 30.
#'   Supersedes the legacy `period_frequency` argument.
#'
#' @returns A tibble, a file path, or a lazy table, according to `as`. When
#'   `wait = FALSE`, the job id as a character scalar.
#'
#' @section Column names:
#' As in [npn_export_individual_phenometrics()]: `PascalCase` on the wire,
#' lowercased for `as = "data"` and `as = "lazy"`. This product is the only one
#' with hyphens in its column names, which are preserved because the legacy
#' download has them too --- reach those columns with backticks:
#' `` df$`total_numanimals_in-phase` ``.
#'
#' `start_date` and `end_date` are the only `Date` columns any of these products
#' returns.
#'
#' @section Kingdom blanks columns:
#' 20 of the 32 core columns are conditional on kingdom. On a plant query the 18
#' abundance columns, `numsites_with_yes_record` and
#' `proportion_sites_with_yes_record` are entirely `NA`; on an animal query
#' `numindividuals_with_yes_record` and
#' `proportion_individuals_with_yes_record` are. Their types are declared, so an
#' all-`NA` column still comes back numeric rather than being guessed.
#'
#' @section Seasonal windows:
#' This product is **never** windowed, unlike
#' [npn_export_individual_phenometrics()] and
#' [npn_export_site_phenometrics()]. Its buckets anchor to `start_date` and
#' `year`, `start_date` and `end_date` are output columns, so time is already
#' resolved in the rows. `[start_date, end_date]` means the continuous span it
#' looks like it means.
#'
#' @section Large queries:
#' The estimate here is arithmetic rather than a count: one row per taxon-grain
#' value, per phenophase-grain value, per time bucket. Two honest limitations
#' follow. It **over-estimates** at coarser `taxon` grains, where many species
#' collapse into one row, and it **under-estimates** when `pheno_class_ids` or
#' `phenophase_categories` narrow the phenophases without narrowing
#' `phenophase_ids`. It only warns, so neither is worth engineering around;
#' `options(rnpn.preflight = FALSE)` switches it off.
#'
#' @seealso [npn_export_status_data()], [npn_get_job()]
#' @export
#' @examples \dontrun{
#' mag <- npn_export_magnitude_phenometrics(
#'   start_date = "2019-01-01",
#'   end_date = "2019-12-31",
#'   species_ids = 3,
#'   frequency = "months"
#' )
#' }
npn_export_magnitude_phenometrics <- function(
  start_date = NULL,
  end_date = NULL,
  species_ids = NULL,
  phenophase_ids = NULL,
  site_ids = NULL,
  states = NULL,
  phenophase_categories = NULL,
  taxon = NULL,
  phenophase_grain = NULL,
  frequency = NULL,
  ...,
  as = c("data", "path", "lazy"),
  file_path = NULL,
  wait = TRUE,
  timeout = 300,
  refresh = FALSE
) {
  tb_export(
    product = tb_products$magnitude,
    named = list(
      start_date = start_date,
      end_date = end_date,
      species_ids = species_ids,
      phenophase_ids = phenophase_ids,
      site_ids = site_ids,
      states = states,
      phenophase_categories = phenophase_categories,
      taxon = taxon,
      phenophase_grain = phenophase_grain,
      frequency = frequency
    ),
    dots = rlang::list2(...),
    as = as,
    file_path = file_path,
    wait = wait,
    timeout = timeout,
    refresh = refresh
  )
}

#' Work out which data product a job belongs to
#'
#' A job id does not carry its product, and the four products have four column
#' type tables. Three answers, in order of authority:
#'
#' 1. what the caller said, if anything;
#' 2. the session registry, written by every submission --- which covers
#'    `wait = FALSE` and a timed-out call entirely;
#' 3. the `product` column in the downloaded file, which survives a new session
#'    and a cache hit.
#'
#' All three fail only for a zero-row result collected in a fresh session, since
#' a header-only file has no `product` value to read.
#' @noRd
tb_resolve_product <- function(
  job_id,
  product = NULL,
  path = NULL,
  call = rlang::caller_env()
) {
  if (!is.null(product)) {
    if (is.list(product)) {
      return(product)
    }
    name <- rlang::arg_match(
      product,
      names(tb_products),
      error_arg = "product",
      error_call = call
    )
    return(tb_products[[name]])
  }

  registered <- tb_registered_product(job_id)
  if (!is.null(registered)) {
    return(registered)
  }

  if (!is.null(path)) {
    found <- tb_product_by_key(tb_file_product_key(path))
    if (!is.null(found)) {
      return(found)
    }
  }

  rlang::abort(
    c(
      paste0(
        'Cannot tell which data product job "', job_id, '" belongs to.'
      ),
      "x" = paste0(
        "This session did not submit it, and the result carries no `product` ",
        "value \u2014 a zero-row result has none."
      ),
      "i" = paste0(
        'Pass it explicitly: npn_get_job("', job_id, '", product = "site")'
      )
    ),
    call = call
  )
}

#' Collect a previously submitted export job
#'
#' Downloads the result of an export job that was submitted earlier, either with
#' `wait = FALSE` or by a call that timed out. Timing out does not cancel the
#' job: the server keeps building it and the result stays available for about a
#' week.
#'
#' @param job_id The job id, as reported when the job was submitted or in the
#'   timeout error message.
#' @param as,file_path,timeout,refresh As documented in
#'   [npn_export_status_data()].
#' @param product Which data product the job was for: one of `"status"`,
#'   `"individual"`, `"site"` or `"magnitude"`. Rarely needed --- see Details.
#'
#' @returns A tibble, a file path, or a lazy table, according to `as`.
#'
#' @details
#' A job id does not carry its data product, and the four products have four
#' sets of column types. This function works it out for itself: it remembers
#' every job this session submitted, and failing that it reads the `product`
#' column that every export carries. `product` is only needed when neither
#' applies --- a zero-row result, collected in a session that did not submit it
#' --- and the error message says so when that happens. `as = "path"` never
#' needs it at all, since nothing is read.
#'
#' The large-query warning is not issued here. It describes the records a
#' *filter* matches, and a job id does not carry the filters that produced it.
#'
#' @seealso [npn_export_status_data()]
#' @export
#' @examples \dontrun{
#' job <- npn_export_status_data(species_ids = 3, wait = FALSE)
#' obs <- npn_get_job(job)
#' }
npn_get_job <- function(
  job_id,
  as = c("data", "path", "lazy"),
  file_path = NULL,
  timeout = 300,
  refresh = FALSE,
  product = NULL
) {
  as <- rlang::arg_match(as)
  if (!is.character(job_id) || length(job_id) != 1 || is.na(job_id)) {
    rlang::abort("`job_id` must be a single job id string.")
  }
  tb_check_engine(as)

  key <- tb_job_key(job_id)
  path <- tb_obtain(
    key,
    refresh = refresh,
    fetch = function(dest) {
      # first_delay = 0: a resumed job is usually finished already, so there is
      # nothing to gain by sleeping before the first check.
      url <- tb_await_job(job_id, timeout = timeout, first_delay = 0)
      tb_fetch_artifact(url, dest)
    }
  )

  # `as = "path"` reads nothing, so it needs no product and must not be able to
  # fail for want of one.
  resolved <- if (identical(as, "path")) {
    NULL
  } else {
    tb_resolve_product(job_id, product = product, path = path)
  }
  tb_deliver(path, key, as, file_path, product = resolved)
}
