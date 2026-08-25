# The pre-flight guard.
#
# Advisory only. It fires exactly one warning and can never block, refuse, or
# prompt.
#
# Only one product has a count pipe: `tinybird/src/endpoints/` contains exactly
# one `*_count.pipe`, and it counts raw observation rows. The other three
# products therefore *estimate*, and the estimator is carried by the product
# entry --- `estimate = NULL` is the whole off switch for a product.

#' How long the advisory guard may spend before giving up
#'
#' The count itself takes ~0.4s. Anything beyond a few seconds means something
#' is wrong, and the right response is to skip the warning rather than delay a
#' download for it.
#' @noRd
tb_guard_timeout <- function() {
  getOption("rnpn.guard_timeout", 10)
}

#' Fetch a Tinybird JWT, reusing the cached one until it expires
#'
#' Tokens last an hour, so fetching one per call would be pure latency. Cached
#' in `pkg.env` alongside the package's other in-memory caches.
#' @noRd
tb_token <- function() {
  cached <- pkg.env$tb_token
  if (!is.null(cached) && cached$expires_at > Sys.time() + 60) {
    return(cached$token)
  }

  # Short timeout on purpose. The guard is advisory and fails open, so it must
  # never be able to add meaningful latency to a download that is going to
  # happen anyway.
  body <- tb_base_req(timeout = tb_guard_timeout()) %>%
    httr2::req_url_path_append("v1/data/token") %>%
    httr2::req_perform() %>%
    httr2::resp_body_json()

  expires <- as.POSIXct(
    body$expires_at,
    format = "%Y-%m-%dT%H:%M:%OS",
    tz = "UTC"
  )
  if (is.na(expires)) {
    expires <- Sys.time() + 300
  }
  pkg.env$tb_token <- list(token = body$token, expires_at = expires)
  body$token
}

#' Forget the cached Tinybird token
#'
#' Used by tests, which must not share state.
#' @noRd
tb_token_reset <- function() {
  pkg.env$tb_token <- NULL
  invisible(NULL)
}

#' Count the observation rows a payload would match
#'
#' No `product` argument: `status_data_count` is the only count pipe that
#' exists, so a parameter offering a choice would be a lie. The three derived
#' products scale this number instead --- see `tb_estimate_count()`.
#'
#' **Fails open.** If the token fetch or the count call fails for any reason
#' this returns `NULL` and the caller proceeds as though the count were under
#' threshold. The guard is advisory and must never be able to block a download.
#'
#' The pipe declares its transport as `POST application/x-www-form-urlencoded`.
#' A GET with query parameters works today and stays, but a long `site_ids` list
#' will eventually exceed a URL length limit somewhere in the path. Failing open
#' means the failure mode is a missing warning, not a broken download.
#'
#' @returns The row count, or `NULL` if it could not be determined.
#' @noRd
tb_count_rows <- function(payload) {
  tryCatch(
    {
      body <- httr2::request(base_tinybird_url()) %>%
        httr2::req_timeout(tb_guard_timeout()) %>%
        httr2::req_user_agent(npn_user_agent) %>%
        httr2::req_url_path_append("v0/pipes", "status_data_count.json") %>%
        httr2::req_url_query(!!!tb_count_params(payload)) %>%
        httr2::req_auth_bearer_token(tb_token()) %>%
        httr2::req_perform() %>%
        httr2::resp_body_json()

      count <- body$data[[1]]$total_records
      if (is.numeric(count)) count else NULL
    },
    error = function(e) NULL
  )
}

#' Estimate rows as a fixed fraction of the matching observation count
#'
#' The three derived products have no count pipe of their own, so their row
#' counts are the observation count divided by a rule of thumb: ~20 observations
#' per phenological island, ~115 per site/taxon/phenophase summary.
#'
#' The ratios are loose historical guidelines, not measurements. They are read
#' through `getOption("rnpn.preflight_ratios")` **at call time**, not captured
#' when the product table is built, for the same reason the base URLs are: a
#' value that can be corrected from the console during a live investigation is
#' worth more than one that needs a package rebuild.
#'
#' @param divisor Observations per output row.
#' @returns A function of `(product, payload)` returning an estimated row count,
#'   or `NULL` when the count could not be obtained.
#' @noRd
tb_estimate_count <- function(divisor) {
  force(divisor)
  function(product, payload) {
    ratio <- divisor
    overrides <- getOption("rnpn.preflight_ratios")
    if (!is.null(overrides) && product$name %in% names(overrides)) {
      supplied <- overrides[[product$name]]
      if (
        is.numeric(supplied) && length(supplied) == 1 &&
          !is.na(supplied) && supplied > 0
      ) {
        ratio <- supplied
      }
    }
    n <- tb_count_rows(payload)
    if (is.null(n)) NULL else n / ratio
  }
}

#' Estimate magnitude rows by grain arithmetic
#'
#' Magnitude rows are one per taxon-grain value x phenophase-grain value x time
#' bucket, so they are arithmetic rather than a count --- and no count call is
#' made at all.
#'
#' Two honest limitations, both stated in the help topic rather than engineered
#' around: it over-estimates at coarser `taxon` grains, where many species
#' collapse into one genus or family row, and `pheno_class_ids` /
#' `phenophase_categories` narrow phenophases without narrowing
#' `phenophase_ids`, so those queries fall back to the open-filter default and
#' under-estimate.
#' @noRd
tb_estimate_magnitude <- function(product, payload) {
  n_taxa <- length(payload$species_ids)
  n_pheno <- length(payload$phenophase_ids)
  # the SME's figures for an open filter
  if (n_taxa == 0) n_taxa <- 2000
  if (n_pheno == 0) n_pheno <- 10
  n_taxa * n_pheno * tb_bucket_count(payload)
}

#' How many time buckets a magnitude query spans
#'
#' `frequency` is a day count or the literal `"months"`, and defaults to the
#' pipe's own default of 30.
#' @noRd
tb_bucket_count <- function(payload) {
  span <- tb_span_days(payload$startDate, payload$endDate)
  if (is.null(span)) {
    return(1)
  }
  freq <- payload$frequency %|||% 30
  days <- if (identical(as.character(freq)[[1]], "months")) {
    30.44
  } else {
    suppressWarnings(as.numeric(freq)[[1]])
  }
  if (is.na(days) || days <= 0) {
    days <- 30
  }
  max(1, ceiling(span / days))
}

#' Inclusive day count between two wire-format dates, or `NULL`
#' @noRd
tb_span_days <- function(start, end) {
  if (is.null(start) || is.null(end)) {
    return(NULL)
  }
  span <- suppressWarnings(
    as.numeric(as.Date(end) - as.Date(start)) + 1
  )
  if (length(span) != 1 || is.na(span)) NULL else span
}

#' Warn when a query is too large to comfortably materialize
#'
#' Fires only for `as = "data"`, the sole mode that materializes rows. Running a
#' count for `"path"` or `"lazy"` would be pure latency for a warning that
#' cannot apply.
#'
#' Warn, never refuse, never prompt. One `rlang::warn()` behaves identically at
#' the console, inside `Rscript`, in a knitted document, and under `targets`.
#' An `if (interactive())` prompt would fire only at the console --- the one
#' place the user can already hit Ctrl-C --- and is actively harmful in CI,
#' where a hang is most expensive and least visible.
#'
#' Three levers switch it off: `options(rnpn.preflight = FALSE)` disables it
#' entirely, `options(rnpn.preflight_ratios = )` corrects the divisors, and
#' `estimate = NULL` in a product entry removes one product's guard for good.
#' @noRd
tb_preflight <- function(product, payload, threshold = tb_row_warn_threshold) {
  if (!isTRUE(getOption("rnpn.preflight", TRUE))) {
    return(invisible(NULL))
  }
  estimate <- product$estimate
  if (is.null(estimate)) {
    return(invisible(NULL))
  }

  # Fails open on anything the estimator itself throws, for the same reason
  # `tb_count_rows()` does.
  n <- tryCatch(estimate(product, payload), error = function(e) NULL)
  if (is.null(n) || length(n) != 1 || !is.finite(n) || n < threshold) {
    return(invisible(NULL))
  }
  rlang::warn(
    tb_size_warning(n, product = product),
    class = "rnpn_large_query_warning"
  )
  invisible(n)
}

#' The text of the large-query warning
#'
#' Split out so it can be tested without a network.
#'
#' `precision` is not decoration. Overclaiming precision on a number derived
#' from a 1:115 rule of thumb would be worse than not warning at all, because
#' the next thing a user does is trust it --- so an estimated count is rounded
#' to two significant figures and said to be approximate, while the status
#' count, which comes from a real count pipe, is reported exactly.
#' @noRd
tb_size_warning <- function(n, product = tb_products$status) {
  exact <- identical(product$precision, "exact")
  if (!exact) {
    n <- signif(n, 2)
  }
  gb <- n * product$bytes_per_row / 2^30
  paste0(
    if (exact) "This query matches " else "This query is estimated to return about ",
    format(n, big.mark = ",", scientific = FALSE, trim = TRUE),
    " records \u2014 ",
    if (exact) "about " else "roughly ",
    formatC(gb, format = "f", digits = 1),
    " GB as a tibble, and ",
    if (exact) "roughly " else "about ",
    formatC(gb * tb_working_multiplier, format = "f", digits = 1),
    " GB while dplyr works on it. That may exceed available memory. Consider ",
    'as = "path" to download without loading, as = "lazy" to query it in ',
    "place, or narrowing your filters."
  )
}
