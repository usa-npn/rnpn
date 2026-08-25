# Offline tests for request building. vcr cannot record this flow (it does not
# work with httr2 downloads, https://github.com/ropensci/vcr/issues/270), so
# these unit tests carry the risk.

test_that("the mapping table translates R names to payload fields", {
  payload <- tb_build_payload(
    named = list(
      start_date = "2024-05-01",
      end_date = "2024-05-03",
      species_ids = c(3, 6),
      site_ids = 4881,
      states = "AZ",
      phenophase_categories = "Leaves"
    ),
    product = tb_products$status
  )

  expect_equal(payload$startDate, "2024-05-01")
  expect_equal(payload$endDate, "2024-05-03")
  expect_equal(payload$species_ids, c(3, 6))
  # three competing spellings; the payload wants `stations`
  expect_equal(payload$stations, 4881)
  # `state`, an array despite the singular name
  expect_equal(payload$state, "AZ")
  expect_equal(payload$downloadType, "Status and Intensity")
  expect_null(payload$site_ids)
  expect_null(payload$states)
})

test_that("phenophase_categories is sent snake_case only", {
  payload <- tb_build_payload(
    named = list(phenophase_categories = "Leaves"),
    product = tb_products$status
  )
  # snake_case is the only spelling that works on both the export endpoint and
  # the count pipe
  expect_equal(payload$phenophase_categories, "Leaves")
  expect_null(payload$phenophaseCategories)
})

test_that("empty and NULL filters are dropped", {
  payload <- tb_build_payload(
    named = list(
      start_date = NULL,
      species_ids = integer(0),
      states = "AZ"
    ),
    product = tb_products$status
  )
  expect_named(payload, c("state", "downloadType"), ignore.order = TRUE)
})

test_that("`...` accepts documented payload fields", {
  payload <- tb_build_payload(
    named = list(species_ids = 3),
    dots = list(
      pheno_class_ids = c(1, 2),
      # all four or none --- a partial box is now a local error
      bottom_left_x1 = -110, bottom_left_y1 = 31,
      upper_right_x2 = -109, upper_right_y2 = 32
    ),
    product = tb_products$status
  )
  expect_equal(payload$pheno_class_ids, c(1, 2))
  expect_equal(payload$bottom_left_x1, -110)
})

test_that("`...` rejects unrecognized fields", {
  # This is the guard that matters most: `additionalProperties: false` is in the
  # spec but not enforced, so an unchecked typo widens the query silently.
  expect_error(
    tb_build_payload(
      dots = list(specie_ids = 3),
      product = tb_products$status
    ),
    "Unrecognized filter"
  )
  expect_error(
    tb_build_payload(
      dots = list(format = "parquet"),
      product = tb_products$status
    ),
    "Unrecognized filter"
  )
})

test_that("a near-miss field name gets a suggestion", {
  expect_error(
    tb_build_payload(
      dots = list(network_id = 1),
      product = tb_products$status
    ),
    "network_ids"
  )
})

test_that("`...` rejects unnamed arguments", {
  expect_error(
    tb_build_payload(
      dots = list(3),
      product = tb_products$status
    ),
    "must be named"
  )
})

test_that("`...` cannot set the product or duplicate a named argument", {
  expect_error(
    tb_build_payload(
      dots = list(downloadType = "something else"),
      product = tb_products$status
    ),
    "identifies the data product"
  )
  expect_error(
    tb_build_payload(
      named = list(site_ids = 1),
      dots = list(stations = 2),
      product = tb_products$status
    ),
    "supplied twice"
  )
})

test_that("dates accept Date objects and YYYY-MM-DD strings", {
  expect_equal(tb_parse_date(as.Date("2024-05-01")), "2024-05-01")
  expect_equal(tb_parse_date("2024-05-01"), "2024-05-01")
  expect_null(tb_parse_date(NULL))
})

test_that("dates reject anything else", {
  expect_error(tb_parse_date("05-01-2024"), "must be a Date")
  expect_error(tb_parse_date("2024-13-01"), "must be a Date")
  expect_error(tb_parse_date(2024), "must be a Date")
  expect_error(tb_parse_date(c("2024-05-01", "2024-05-02")), "single date")
})

test_that("hashing normalizes element order", {
  # without element sorting, species_ids = c(3, 5) and c(5, 3) are the same
  # query but miss each other in the cache
  a <- tb_build_payload(
    named = list(species_ids = c(3, 5)),
    product = tb_products$status
  )
  b <- tb_build_payload(
    named = list(species_ids = c(5, 3)),
    product = tb_products$status
  )
  expect_equal(tb_payload_hash(a), tb_payload_hash(b))
})

test_that("hashing normalizes key order and value type", {
  a <- list(startDate = "2024-05-01", species_ids = 3)
  b <- list(species_ids = "3", startDate = "2024-05-01")
  expect_equal(tb_payload_hash(a), tb_payload_hash(b))
})

test_that("different queries hash differently", {
  a <- list(species_ids = 3)
  b <- list(species_ids = 4)
  expect_false(identical(tb_payload_hash(a), tb_payload_hash(b)))
})

test_that("array fields survive JSON encoding as arrays", {
  payload <- tb_json_payload(list(species_ids = 3, startDate = "2024-05-01"))
  json <- as.character(jsonlite::toJSON(payload, auto_unbox = TRUE))
  expect_match(json, '"species_ids":\\[3\\]')
  expect_match(json, '"startDate":"2024-05-01"')
})

test_that("count params use the count pipe's spellings", {
  payload <- tb_build_payload(
    named = list(
      start_date = "2024-05-01",
      states = c("AZ", "NM"),
      site_ids = 4881
    ),
    product = tb_products$status
  )
  params <- tb_count_params(payload)

  expect_equal(params$start_date, "2024-05-01")
  # plural here; `state=` singular is not a parameter at all
  expect_equal(params$states, "AZ,NM")
  # the pipe gained this on 2026-08-19, retiring M1's deliberate over-estimate
  expect_equal(params$site_ids, "4881")
  # the pipe has no notion of the product
  expect_null(params$downloadType)
})

test_that("the bounding box is translated to the pipe's lat/lng spellings", {
  # a separate payload, because states and a bounding box cannot coexist
  payload <- tb_build_payload(
    dots = list(
      bottom_left_x1 = -110, bottom_left_y1 = 31,
      upper_right_x2 = -109, upper_right_y2 = 32
    ),
    product = tb_products$status
  )
  params <- tb_count_params(payload)

  expect_equal(params$bottom_left_lng, "-110")
  expect_equal(params$bottom_left_lat, "31")
  expect_equal(params$upper_right_lng, "-109")
  expect_equal(params$upper_right_lat, "32")
})

test_that("the count params include the two the pipe gained", {
  # obs_search_keys declares 22 parameters: `individual_ids` matters now that
  # two products expose it, and `phenophaseCategories` maps to
  # `phenophase_short_names`, which is a different filter from
  # `phenophase_categories` and not an alias for it
  expect_equal(unname(tb_count_names[["individual_ids"]]), "individual_ids")
  expect_equal(
    unname(tb_count_names[["phenophaseCategories"]]),
    "phenophase_short_names"
  )
  expect_equal(
    unname(tb_count_names[["phenophase_categories"]]),
    "phenophase_categories"
  )
})

# --- the product table -------------------------------------------------------

test_that("every product's tables are internally consistent", {
  for (product in tb_products) {
    # the entry's `name` has to match its key in the list, because
    # `rnpn.preflight_ratios` and the job registry both address it by name
    expect_identical(tb_products[[product$name]]$key, product$key)
    # every accepted field has a declared JSON kind
    expect_true(all(product$properties %in% names(tb_field_kinds)))
    # the scalars a product validates must be fields it accepts
    expect_true(all(names(product$scalars) %in% product$properties))
    # and nothing is accepted twice
    expect_false(any(duplicated(product$properties)))
  }
})

test_that("every named argument is mapped and accepted somewhere", {
  expect_true(all(tb_payload_names %in% names(tb_field_kinds)))
  expect_true(all(names(tb_count_names) %in% names(tb_field_kinds)))
  accepted <- unique(unlist(lapply(tb_products, function(p) p$properties)))
  expect_true(all(tb_payload_names %in% accepted))
})

test_that("the status field set drops the nine no mapper forwards", {
  # they passed M1's validation and did nothing, which is the precise failure
  # the validation exists to prevent --- our list was the wrong one
  inert <- c(
    "species_names", "partnerGroups", "integrated_datasets", "qualityFlags",
    "additionalFields", "additionalFieldsDisplay", "ancillary_data",
    "bottom_left_constraint", "upper_right_constraint"
  )
  expect_length(intersect(inert, tb_props_status), 0)
  # and individual_ids, which statusDataMapper does not emit
  expect_false("individual_ids" %in% tb_props_status)
  expect_length(tb_props_status, 32)
})

# --- per-product `...` validation (plan section 8.1) -------------------------

test_that("a field valid on one product is rejected on another", {
  # each product's mapper silently drops the flags outside its own whitelist,
  # so an unchecked field returns 202 and a CSV with columns missing
  expect_error(
    tb_build_payload(
      dots = list(include_climate = "1"),
      product = tb_products$magnitude
    ),
    "Unrecognized filter field `include_climate` for magnitude phenometrics"
  )
  expect_error(
    tb_build_payload(
      dots = list(individual_ids = 4),
      product = tb_products$magnitude
    ),
    "Unrecognized filter"
  )
  expect_error(
    tb_build_payload(
      dots = list(observation_ids = 4),
      product = tb_products$site
    ),
    "Unrecognized filter"
  )
  expect_error(
    tb_build_payload(
      dots = list(frequency = 30),
      product = tb_products$site
    ),
    "Unrecognized filter"
  )

  # and each is accepted on the product that does have it
  expect_no_error(
    tb_build_payload(
      dots = list(include_climate = "1"),
      product = tb_products$site
    )
  )
  expect_no_error(
    tb_build_payload(
      dots = list(individual_ids = 4),
      product = tb_products$individual
    )
  )
  expect_no_error(
    tb_build_payload(
      dots = list(observation_ids = 4),
      product = tb_products$status
    )
  )
})

test_that("the rejection says why, not just that", {
  err <- expect_error(
    tb_build_payload(
      dots = list(include_climate = "1"),
      product = tb_products$magnitude
    )
  )
  expect_match(conditionMessage(err), "aggregates across locations")
})

test_that("suggestions come from the product's own set", {
  # `frequency` is a real field, just not on this product, so it must not be
  # offered as the fix for a typo of something else
  err <- expect_error(
    tb_build_payload(
      dots = list(network_id = 1),
      product = tb_products$magnitude
    )
  )
  expect_match(conditionMessage(err), "network_ids")
})

# --- enum-valued scalars (plan section 8.2) ---------------------------------

test_that("`taxon` and `phenophase_grain` are checked against the product", {
  # the pipes fall back to their default on an unrecognized value, so a typo
  # returns data at the wrong grain rather than failing
  expect_error(
    tb_build_payload(
      named = list(taxon = "Genus"),
      product = tb_products$site
    ),
    "must be one of"
  )
  expect_error(
    tb_build_payload(
      named = list(phenophase_grain = "phenoclass"),
      product = tb_products$magnitude
    ),
    "must be one of"
  )
})

test_that('`taxon = "none"` is magnitude-only', {
  # the one asymmetry a shared enum would have flattened
  expect_no_error(
    tb_build_payload(
      named = list(taxon = "none"),
      product = tb_products$magnitude
    )
  )
  expect_error(
    tb_build_payload(
      named = list(taxon = "none"),
      product = tb_products$site
    ),
    "must be one of"
  )
})

test_that("the numeric scalars are checked locally", {
  expect_no_error(
    tb_build_payload(
      named = list(frequency = 14),
      product = tb_products$magnitude
    )
  )
  expect_no_error(
    tb_build_payload(
      named = list(frequency = "months"),
      product = tb_products$magnitude
    )
  )
  expect_error(
    tb_build_payload(
      named = list(frequency = 0),
      product = tb_products$magnitude
    ),
    "whole number"
  )
  expect_error(
    tb_build_payload(
      named = list(frequency = "weekly"),
      product = tb_products$magnitude
    ),
    "whole number"
  )
  expect_no_error(
    tb_build_payload(
      named = list(num_days_quality_filter = 0),
      product = tb_products$site
    )
  )
  expect_error(
    tb_build_payload(
      named = list(num_days_quality_filter = -1),
      product = tb_products$site
    ),
    "whole number"
  )
  # reachable through `...` on individual phenometrics, and checked there too
  expect_error(
    tb_build_payload(
      dots = list(num_days_quality_filter_individual = 2.5),
      product = tb_products$individual
    ),
    "whole number"
  )
})

# --- cross-field rules (plan section 8.3) -----------------------------------

test_that("states and a bounding box cannot both be supplied", {
  for (product in tb_products) {
    expect_error(
      tb_build_payload(
        named = list(states = "AZ"),
        dots = list(
          bottom_left_x1 = -110, bottom_left_y1 = 31,
          upper_right_x2 = -109, upper_right_y2 = 32
        ),
        product = product
      ),
      "not both"
    )
  }
})

test_that("a bounding box is all four coordinates or none", {
  for (product in tb_products) {
    expect_error(
      tb_build_payload(
        dots = list(bottom_left_x1 = -110, bottom_left_y1 = 31),
        product = product
      ),
      "all four coordinates"
    )
    expect_no_error(
      tb_build_payload(
        dots = list(
          bottom_left_x1 = -110, bottom_left_y1 = 31,
          upper_right_x2 = -109, upper_right_y2 = 32
        ),
        product = product
      )
    )
  }
})

# --- date rules (plan section 6) --------------------------------------------

test_that("three products refuse to be called without dates", {
  for (name in c("individual", "site", "magnitude")) {
    expect_error(
      tb_check_dates(tb_products[[name]], NULL, NULL),
      "are required for"
    )
    expect_error(
      tb_check_dates(tb_products[[name]], "2019-01-01", NULL),
      "are required for"
    )
  }
  # status data is the one that may be called without them
  expect_no_error(tb_check_dates(tb_products$status, NULL, NULL))
})

test_that("end_date before start_date fails locally", {
  expect_error(
    tb_check_dates(tb_products$magnitude, "2019-06-01", "2019-01-01"),
    "must not be before"
  )
})

test_that("a wrapping range shorter than a year is refused for windowed products", {
  # deriveWindows() throws "Date range does not span a complete seasonal year";
  # reproducing it converts a 400 into an error naming the two arguments
  # the only range that reaches this rule: `end_date` on the same month-day as
  # `start_date`, so the window has nowhere to close. Anything earlier in the
  # same year is caught first by the date-order rule.
  expect_error(
    tb_check_dates(tb_products$individual, "2019-10-01", "2019-10-01"),
    "complete seasonal year"
  )
  # the same wrapping range, given a full year to wrap into
  expect_no_error(
    tb_check_dates(tb_products$site, "2019-10-01", "2020-09-30")
  )
  # a non-wrapping range is always fine, however short
  expect_no_error(
    tb_check_dates(tb_products$individual, "2019-01-01", "2019-01-02")
  )
  # magnitude is never windowed, so the same range is legitimate there
  expect_no_error(
    tb_check_dates(tb_products$magnitude, "2019-10-01", "2019-10-01")
  )
})
