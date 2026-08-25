# rnpn (development version)

## New features

* Experimental `npn_export_status_data()` downloads status and intensity data
  through the new asynchronous export service. It takes a `start_date`/`end_date`
  span rather than `years`, and its `as` argument returns a tibble
  (`"data"`), a file path (`"path"`), or a lazy duckdb-backed table (`"lazy"`)
  that can be queried with dplyr without loading the data into memory. `duckdb`
  and `dbplyr` are needed only for `"data"` and `"lazy"`, and are checked for
  before a download is submitted. See `vignette("IX_async_exports")`.
* Experimental `npn_export_individual_phenometrics()`,
  `npn_export_site_phenometrics()` and `npn_export_magnitude_phenometrics()`
  download the three derived data products through the same asynchronous
  service, with the same `as` contract, cache and job handling.
  * `start_date` and `end_date` are **required** for all three, and are checked
    before anything is submitted.
  * For individual and site phenometrics the date range is decomposed by the
    service into one window per phenological year, anchored on the **month-day**
    of the two dates. `start_date = "2019-01-01", end_date = "2021-06-30"`
    returns January to June of 2019, of 2020 and of 2021 — not the continuous
    span between them. This is the legacy `period_start` / `period_end`
    behavior arriving through the date arguments. Magnitude phenometrics is
    never windowed.
  * The service emits these three products' columns in legacy `PascalCase`;
    `as = "data"` and `as = "lazy"` return them lowercased, matching the legacy
    downloads and the vignettes. `as = "path"` hands over the raw file
    unchanged. Magnitude's hyphenated names are preserved, as in the legacy
    download, and need backticks: ``df$`total_numanimals_in-phase` ``. The
    lowercased names were verified column-for-column against a legacy download
    of the same query, for all three products.
  * Legacy `taxonomy_aggregate` and `pheno_class_aggregate` are superseded by
    `taxon` and `phenophase_grain`, which name the grain rather than toggling
    it; `period_frequency` is now `frequency`. Values are validated locally,
    because the service falls back to its default on an unrecognized one rather
    than erroring.
  * The geospatial arguments of the legacy functions (`six_leaf_layer`,
    `agdd_layer`, `additional_layers`, `wkt`) have no equivalent here.
* `vignette("V_magnitude_phenometrics")` referred to two columns by names the
  service does not return (`proportion_yes_record` and
  `mean_num_animals_in-phase`); corrected to `proportion_yes_records` and
  `mean_numanimals_in-phase`.
* `npn_get_job()` collects an export submitted earlier with `wait = FALSE`, or
  one whose call timed out. Timing out does not cancel the job. It works out
  which data product a job was for on its own; the new `product` argument names
  it for the one case that defeats that, a zero-row result collected in a
  session that did not submit it.
* The size warning issued for `as = "data"` now covers all four products. Only
  status data has a count endpoint, so the other three report an estimate and
  say so. `options(rnpn.preflight = FALSE)` switches the check off, and
  `options(rnpn.preflight_ratios = )` corrects the ratios the estimates use.
* Filters passed through `...` are now checked against the fields *each
  product* accepts, rather than one shared list. Nine fields that no data
  product forwards (`species_names`, `additionalFields`, `ancillary_data` and
  others) are no longer accepted, since they silently did nothing. The
  states-versus-bounding-box and all-four-coordinates rules are now checked
  locally rather than costing a round trip.
* `npn_cache_clear()` empties the download cache. The cache lives in the session
  temp directory by default; set `options(rnpn.cache_dir = )` to keep downloads
  across sessions.
* `options(rnpn.engine = "readr")` parses downloads with readr instead of
  duckdb, returning the same tibble without loading duckdb at all. Useful when
  duckdb is unavailable or unwelcome, such as under a debugger, where its
  native lock can deadlock. `as = "lazy"` still requires duckdb.

The existing `npn_download_*()` functions are unchanged.

* Reverts required R version from ≥ 4.1.0 to ≥ 3.5.0.

# rnpn 1.4.1

* -9999 is now converted to `NA` for all columns in data returned by `npn_download_*()` functions (#119, #121).
* compatibility with vcr v2.0.0 (fixed in #125 by @skott)

## Bug fixes

* fixed bug that caused some `npn_download_*()` functions to error when sections of the data were `NA` (#107 reported by @ezylstra)
* Fixed a bug where returned value of `npn_get_point_data()` was inconsistent depending on whether it was cached or not (same bug and solution as #42)
* Fixed a bug in data download functions that errored uniformatively if no data was returned.  Now an empty tibble is returned.


# rnpn 1.4.0

## New features

* `npn_download_individual_phenometrics()` and `npn_download_site_phenometrics()` gain `period_start` and `period_end` arguments for defining a custom "window" or season for phenometrics.

## Deprcations & changes

* The `speciesid` argument of `npn_stations_with_spp()` has been deprecated in favor of `species_id` for uniformity.
* Changed behavior of `kingdom` arguments in `npn_species_state()` and `npn_species_types()`.  Now provide either `"Plantae"`, `"Animalia"`, or `c("Plantae", "Animalia")` (the default). A column for `kingdom` is added to the return value of `npn_species_types()`.
* The `return_all` argument of `npn_get_phenophases_for_taxon()` has been deprecated.  Use `date = "all"` to return data for all dates instead. `return_all = 1` will continue to work (with a warning) in this version.

## Bug fixes

* Fixed bug that caused an error when `agdd_layer` was used in download functions.
* Download times and memory requirements had increased drastically with changes to phenometrics functions in v1.3.0 (#104).  This is now fixed with #105.  The only user-facing difference should be that there is no longer a progress indicator when retrieving data (sorry about that).

# rnpn 1.3.0

## Dependency changes

* `nnpn` no longer depends on the `sp` or `raster` packages
* `terra` is now a suggested dependency and users will be prompted to install it only when it is needed
* `rnpn` now requires the `xml2` package instead of `XML`
* `rnpn` now has `dplyr` as a dependency instead of `plyr`
* `rnpn` now uses `httr2` instead of `httr` and `curl` internally for functions that get observational data
* data download functions now return tibbles instead of `data.table` objects.  `rnpn` no longer depends on `data.table`

## Changes to function arguments

* `npn_phenophase_details()` now takes a vector of phenophase IDs rather than a list
* Documented a behavior of `npn_species_type()` where setting `kingdom` to `NULL` returns results for *both* `Plantae` and `Animalia`. 
* `...` is no longer used for functions that get observational data

## Changes to function outputs

* Functions that previously returned `data.frame` objects now return tibbles. Where they previously returned `NULL` on errors, they now return empty 0x0 tibbles.
* Missing values returned by download functions are now automatically converted from -9999 to `NA`
* Missing values returned by `npn_stations_by_state()` previously returned as the string `"emptyvalue"` are now returned as `NA`s.
* `npn_groups(use_hierarchy = TRUE)` now returns a nested list rather than a tibble with a list-column.
* `npn_abundance_categories()`, `npn_phenophases_by_species()`, and `npn_get_phenophases_for_taxon()` now return tibbles with any list-columns unnested.


## Bug fixes

* Fixed a bug (#42) where returned value of `npn_get_agdd_point_data()` was inconsistent depending on whether it was cached or not.

# rnpn 1.2.9 (2024-08-18)

### NEW FEATURES

* Fixed failed tests due vignette calling geoserver directly

# rnpn 1.2.8 (2024-02-08)

### NEW FEATURES

* Fixed failed tests due to server migration

# rnpn 1.2.7 (2024-01-23)

### NEW FEATURES

* Migrate back end to cloud instances

# rnpn 1.2.6 (2023-08-28)

### NEW FEATURES

* Remove rgdal dependencies
* Update maintainer to Jeff Switzer

# rnpn 1.2.5 (2022-04-20)

### NEW FEATURES

* New vignette, #8 on Data Cleaning
* Changes to reflect repository being transferred from ropensci to usa-npn in github

# rnpn 1.2.4 (2021-11-10)

### NEW FEATURES

* Skipping more API-dependent unit tests.

# rnpn 1.2.3 (2021-10-22)

### NEW FEATURES

* Skipping failing API-dependent unit tests and improved documentation.

# rnpn 1.2.2 (2021-10-04)

### NEW FEATURES

* Fixing failing unit tests

# rnpn 1.2.1 (2021-04-05)

### NEW FEATURES

* Fixing failing unit tests

# rnpn 1.2.0 (2021-03-19)

### NEW FEATURES

* Graceful fails when NPN data services are unavailable

# rnpn 1.1.1 (2020-10-27)

### NEW FEATURES

* Total overhaul of the rNPN package
* Added functions for directly downloading different observation record data types
* Added additional utility and lookup type functions
* Added functions for downloading USA-NPN raster data and geospatial values by latitude/longitude
* Deprecated the following functions: lookup_names, npn_allobssp, npn_indsatstations, npn_indspatstations, npn_species_comm, npn_species_itis, npn_species_sci, npn_stationsbystate, npn_stationswithspp

# rnpn 0.1.0

### NEW FEATURES

* released to CRAN
