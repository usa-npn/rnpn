# Building, validating, and hashing an export request payload.
#
# Pure functions: nothing here touches the network or the disk.
#
# Every check is per-product, driven by the product entry, because the products
# disagree: `taxon = "none"` is legitimate on magnitude phenometrics and a
# `ValidationError` on site phenometrics, `include_climate` exists on three of
# the four, and dates are required on three of the four. None of that is
# branched on here --- it is read out of `tb_products`.

#' Coerce a user-supplied date to the wire format
#'
#' Accepts a `Date` or a `"YYYY-MM-DD"` string and errors on anything else.
#' `validate_mmdd()` is the wrong shape for this --- it validates a bare `MM-DD`
#' recurring-window bound, not a calendar date --- so this is a sibling rather
#' than a reuse.
#'
#' @param x A `Date` or `"YYYY-MM-DD"` string; `NULL` passes through.
#' @returns A `"YYYY-MM-DD"` character scalar, or `NULL`.
#' @noRd
tb_parse_date <- function(
  x,
  arg = rlang::caller_arg(x),
  call = rlang::caller_env()
) {
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  if (length(x) != 1) {
    rlang::abort(
      c(
        paste0("`", arg, "` must be a single date."),
        "x" = paste0("It has length ", length(x), "."),
        "i" = "Use `start_date` and `end_date` to describe one span."
      ),
      call = call
    )
  }

  if (inherits(x, "Date")) {
    if (is.na(x)) {
      rlang::abort(paste0("`", arg, "` must not be `NA`."), call = call)
    }
    return(format(x, "%Y-%m-%d"))
  }

  if (is.character(x) && grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) {
    parsed <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (!is.na(parsed)) {
      return(format(parsed, "%Y-%m-%d"))
    }
  }

  rlang::abort(
    c(
      paste0("`", arg, "` must be a Date or a 'YYYY-MM-DD' string."),
      "x" = paste0("You supplied ", tb_describe(x), "."),
      "i" = 'For example: as.Date("2024-05-01") or "2024-05-01".'
    ),
    call = call
  )
}

#' A short human description of a bad value, for error messages
#' @noRd
tb_describe <- function(x) {
  if (is.character(x)) {
    return(paste0('"', x[[1]], '"'))
  }
  paste0("a ", paste(class(x), collapse = "/"), " value")
}

#' Drop `NULL` and zero-length entries from a list
#' @noRd
tb_drop_empty <- function(x) {
  x[vapply(x, function(v) !is.null(v) && length(v) > 0, logical(1))]
}

#' @noRd
tb_quote_list <- function(x) {
  paste0("`", x, "`", collapse = ", ")
}

# --- date rules (plan section 6) ---------------------------------------------

#' Validate the date pair against the product's own rules
#'
#' Three products require dates: `IndividualPhenometricsRequestSchema`,
#' `SitePhenometricsRequestSchema` and `MagnitudePhenometricsRequestSchema` all
#' declare `startDate` and `endDate` non-optional, and the `individual_metrics`
#' pipe declares both `required=True` because an unfiltered call would sort ~50M
#' rows and OOM. Checking here turns a 400 phrased in the server's vocabulary
#' into an error that names the two arguments, and costs no round trip.
#'
#' The wrapping-range rule reproduces the one check in `deriveWindows()` that a
#' user can trip: a range whose `end_date` falls no later in the year than its
#' `start_date` wraps the turn of the year, and must therefore span at least one
#' full year to contain a complete window. The rest of `deriveWindows()` is
#' deliberately **not** reimplemented --- nothing here needs the window list, and
#' a second copy of a server-side rule with no consumer is a liability.
#' @noRd
tb_check_dates <- function(
  product,
  start_date,
  end_date,
  call = rlang::caller_env()
) {
  if (
    isTRUE(product$dates_required) &&
      (is.null(start_date) || is.null(end_date))
  ) {
    rlang::abort(
      c(
        paste0(
          "`start_date` and `end_date` are required for ",
          product$label, "."
        ),
        "i" = "Unlike status data, this product cannot be requested without a date range."
      ),
      call = call
    )
  }

  if (is.null(start_date) || is.null(end_date)) {
    return(invisible(NULL))
  }

  if (end_date < start_date) {
    rlang::abort(
      c(
        "`end_date` must not be before `start_date`.",
        "x" = paste0("You supplied ", start_date, " to ", end_date, ".")
      ),
      call = call
    )
  }

  if (!isTRUE(product$windowed) || tb_spans_seasonal_year(start_date, end_date)) {
    return(invisible(NULL))
  }

  rlang::abort(
    c(
      "`start_date` and `end_date` do not span a complete seasonal year.",
      "x" = paste0(
        "`end_date` (", end_date, ") falls no later in the year than ",
        "`start_date` (", start_date, "), so the range wraps around the turn ",
        "of the year and must span at least one full year."
      ),
      "i" = paste0(
        "The service decomposes the range into one window per phenological ",
        "year, anchored on the month-day of `start_date`, and a wrapping ",
        "range shorter than a year contains no complete window."
      )
    ),
    call = call
  )
}

#' Does this range contain at least one complete seasonal window?
#'
#' Mirrors `windows.ts::deriveWindows()`: a non-wrapping range always does, and
#' a wrapping one needs at least a year's difference between the two years.
#' @noRd
tb_spans_seasonal_year <- function(start_date, end_date) {
  s <- as.integer(strsplit(start_date, "-", fixed = TRUE)[[1]])
  e <- as.integer(strsplit(end_date, "-", fixed = TRUE)[[1]])
  non_wrapping <- e[[2]] > s[[2]] || (e[[2]] == s[[2]] && e[[3]] > s[[3]])
  non_wrapping || (e[[1]] - s[[1]]) >= 1
}

# --- scalar rules (plan section 8.2) -----------------------------------------

#' Field-intrinsic scalar checks
#'
#' Keyed by payload field. These rules do not vary by product --- a frequency is
#' a frequency wherever it is accepted --- so they live here rather than in the
#' product entry. The rules that *do* vary by product are the enums, and those
#' are `product$scalars`.
#' @noRd
tb_scalar_validators <- list(
  frequency = function(value, field, call) {
    if (identical(as.character(value)[[1]], "months")) {
      return(invisible(NULL))
    }
    tb_check_whole_number(value, field, min = 1, call = call)
  },
  num_days_quality_filter = function(value, field, call) {
    tb_check_whole_number(value, field, min = 0, call = call)
  },
  num_days_quality_filter_individual = function(value, field, call) {
    tb_check_whole_number(value, field, min = 0, call = call)
  }
)

#' @noRd
tb_check_whole_number <- function(value, field, min, call) {
  ok <- length(value) == 1 &&
    is.numeric(value) &&
    !is.na(value) &&
    value >= min &&
    value == trunc(value)
  if (ok) {
    return(invisible(NULL))
  }
  rlang::abort(
    c(
      paste0(
        "`", field, "` must be a single whole number ",
        if (min > 0) paste0("of at least ", min) else "of 0 or more",
        if (identical(field, "frequency")) ', or the string "months"' else "",
        "."
      ),
      "x" = paste0("You supplied ", tb_describe(value), ".")
    ),
    call = call
  )
}

#' Validate this product's enum-valued and numeric scalars
#'
#' Not defensive tidiness. **The pipes silently fall back to their default on an
#' unrecognized value**, so `taxon = "Genus"` would return species-grain data
#' labelled as nothing --- the same failure class as an invalid filter *value*,
#' and the client is the only place it can be caught. The server's own answer is
#' product-dependent: `taxon = "none"` is legitimate on magnitude phenometrics
#' and a `ValidationError` on site phenometrics.
#' @noRd
tb_validate_scalars <- function(payload, product, call = rlang::caller_env()) {
  for (field in names(product$scalars)) {
    if (!is.null(payload[[field]])) {
      tb_check_enum(
        payload[[field]],
        field,
        product$scalars[[field]],
        call = call
      )
    }
  }
  for (field in intersect(names(payload), names(tb_scalar_validators))) {
    tb_scalar_validators[[field]](payload[[field]], field, call)
  }
  invisible(payload)
}

#' @noRd
tb_check_enum <- function(value, field, allowed, call) {
  value <- as.character(value)
  if (length(value) == 1 && value %in% allowed) {
    return(invisible(NULL))
  }
  rlang::abort(
    c(
      paste0("`", field, "` must be one of ", tb_quote_list(allowed), "."),
      "x" = if (length(value) == 1) {
        paste0('You supplied "', value, '".')
      } else {
        paste0("You supplied ", length(value), " values.")
      },
      "i" = paste0(
        "The service falls back to its default on an unrecognized value ",
        "rather than erroring, so an unchecked typo would return data at the ",
        "wrong grain instead of failing."
      )
    ),
    call = call
  )
}

# --- cross-field rules (plan section 8.3) ------------------------------------

#' The two cross-field rules `mapSharedFilters()` enforces on every product
#'
#' Both are a 400 server-side, phrased in payload-field names the R user never
#' typed. Checking here costs nothing and applies to all four products.
#' @noRd
tb_check_cross_fields <- function(payload, call = rlang::caller_env()) {
  supplied_bbox <- intersect(tb_bbox_fields, names(payload))

  if (length(payload$state) > 0 && length(supplied_bbox) > 0) {
    rlang::abort(
      c(
        "Supply either `states` or a bounding box, not both.",
        "x" = paste0(
          "You supplied `states` and ",
          tb_quote_list(supplied_bbox), "."
        ),
        "i" = "The service rejects a request carrying both."
      ),
      call = call
    )
  }

  if (length(supplied_bbox) > 0 && length(supplied_bbox) < 4) {
    rlang::abort(
      c(
        "A bounding box needs all four coordinates.",
        "x" = paste0(
          "Missing ",
          tb_quote_list(setdiff(tb_bbox_fields, supplied_bbox)), "."
        ),
        "i" = paste0(
          "Supply ", tb_quote_list(tb_bbox_fields), " together, or none of them."
        )
      ),
      call = call
    )
  }

  invisible(payload)
}

# --- `...` validation (plan section 8.1) -------------------------------------

#' Validate the fields supplied through `...`, against this product's set
#'
#' This is not optional. `additionalProperties: false` is declared in the spec
#' and **not enforced by the server**, and the count pipe behaves the same way,
#' so an unvalidated typo widens the query rather than erroring and there is no
#' server-side backstop anywhere.
#'
#' It matters more with four products than it did with one, because each
#' product's mapper *also* silently drops the flags outside its own whitelist.
#' Passing `include_climate` to magnitude phenometrics gets a 202 and a CSV with
#' no climate columns and no complaint anywhere.
#'
#' @param dots A named list of extra payload fields.
#' @param product The product entry, whose `properties` is the accepted set.
#' @param named_payload The payload fields already claimed by named arguments.
#' @noRd
tb_validate_dots <- function(
  dots,
  product,
  named_payload = character(),
  call = rlang::caller_env()
) {
  if (length(dots) == 0) {
    return(invisible(dots))
  }

  nms <- rlang::names2(dots)
  if (any(nms == "")) {
    rlang::abort(
      c(
        "All arguments passed through `...` must be named.",
        "i" = "For example: `pheno_class_ids = c(1, 2)`."
      ),
      call = call
    )
  }

  reserved <- intersect(nms, tb_reserved_fields)
  if (length(reserved) > 0) {
    rlang::abort(
      c(
        paste0("Cannot set ", tb_quote_list(reserved), " through `...`."),
        "i" = "It identifies the data product and is set by this function."
      ),
      call = call
    )
  }

  unknown <- setdiff(nms, product$properties)
  if (length(unknown) > 0) {
    notes <- vapply(
      unknown,
      function(field) tb_field_note(product, field),
      character(1)
    )
    rlang::abort(
      c(
        paste0(
          "Unrecognized filter ",
          if (length(unknown) > 1) "fields " else "field ",
          tb_quote_list(unknown),
          " for ", product$label, "."
        ),
        tb_bullets(notes[!is.na(notes)]),
        tb_bullets(tb_suggestions(unknown, product$properties)),
        "i" = paste0(
          "The service silently ignores fields it does not recognize, so an ",
          "unchecked typo would return incomplete data rather than failing."
        )
      ),
      call = call
    )
  }

  clash <- intersect(nms, named_payload)
  if (length(clash) > 0) {
    r_names <- names(tb_payload_names)[match(clash, tb_payload_names)]
    rlang::abort(
      c(
        paste0("Filter ", tb_quote_list(clash), " was supplied twice."),
        "x" = paste0(
          "It is the payload field behind ", tb_quote_list(r_names), "."
        ),
        "i" = paste0("Use ", tb_quote_list(r_names), " only.")
      ),
      call = call
    )
  }

  invisible(dots)
}

#' Why this field is not accepted here, if there is anything useful to say
#'
#' Product-specific reason first, general reason second, `NA` if neither.
#' @noRd
tb_field_note <- function(product, field) {
  for (key in c(paste0(product$name, ".", field), field)) {
    if (key %in% names(tb_field_notes)) {
      return(unname(tb_field_notes[[key]]))
    }
  }
  NA_character_
}

#' Label a character vector as cli info bullets, or drop it entirely
#'
#' `rlang::set_names(character(0), "i")` is a length mismatch, so an empty set
#' of hints has to become `NULL` rather than an empty named vector.
#' @noRd
tb_bullets <- function(x, type = "i") {
  if (length(x) == 0) {
    return(NULL)
  }
  rlang::set_names(unname(x), rep(type, length(x)))
}

#' Nearest accepted field name, for a typo
#' @noRd
tb_suggestions <- function(unknown, valid) {
  out <- character(0)
  for (u in unknown) {
    d <- utils::adist(u, valid, ignore.case = TRUE)[1, ]
    if (min(d) <= max(2, nchar(u) %/% 3)) {
      out <- c(out, paste0("Did you mean `", valid[which.min(d)], "`?"))
    }
  }
  out
}

# --- assembly ----------------------------------------------------------------

#' Assemble and validate an export request payload
#'
#' @param named A list keyed by *R-facing* argument names (see
#'   `tb_payload_names`). Entries that are `NULL` or empty are dropped.
#' @param dots A list of extra fields keyed by *payload* field name.
#' @param product The product entry. Supplies the accepted field set, the
#'   enum-valued scalars, and the `downloadType` value.
#' @returns A named list keyed by payload field name.
#' @noRd
tb_build_payload <- function(
  named = list(),
  dots = list(),
  product,
  call = rlang::caller_env()
) {
  named <- tb_drop_empty(named)

  unmapped <- setdiff(names(named), names(tb_payload_names))
  if (length(unmapped) > 0) {
    # An internal wiring mistake, not a user error.
    rlang::abort(
      paste0("No payload field is mapped for ", tb_quote_list(unmapped), ".")
    )
  }
  names(named) <- unname(tb_payload_names[names(named)])

  unsupported <- setdiff(names(named), product$properties)
  if (length(unsupported) > 0) {
    # Also a wiring mistake: a wrapper offered an argument its product does not
    # accept. Caught by the offline suite rather than by a user.
    rlang::abort(
      paste0(
        product$label, " does not accept ",
        tb_quote_list(unsupported), "."
      )
    )
  }

  dots <- tb_drop_empty(dots)
  tb_validate_dots(dots, product, named_payload = names(named), call = call)

  payload <- c(named, dots)
  tb_check_cross_fields(payload, call = call)
  tb_validate_scalars(payload, product, call = call)

  payload[["downloadType"]] <- product$download_type
  payload
}

#' Mark array-valued fields so JSON encoding keeps them as arrays
#'
#' `jsonlite` unboxes length-1 vectors, which would send `"species_ids": 3`
#' where the spec asks for `[3]`. `I()` opts a field out of that.
#' @noRd
tb_json_payload <- function(payload) {
  kinds <- tb_field_kinds[names(payload)]
  is_array <- !is.na(kinds) & kinds == "array"
  payload[is_array] <- lapply(payload[is_array], I)
  payload
}

#' Normalize a payload for hashing
#'
#' Element sorting is load-bearing: without it `species_ids = c(3, 5)` and
#' `c(5, 3)` are the same query but miss each other in the cache. Values are
#' compared as text so that `3` and `"3"` also agree.
#' @noRd
tb_normalize_payload <- function(payload) {
  payload <- tb_drop_empty(payload)
  payload <- lapply(payload, function(v) sort(as.character(v)))
  payload[order(names(payload))]
}

#' Cache key for a payload
#' @noRd
tb_payload_hash <- function(payload) {
  rlang::hash(jsonlite::toJSON(
    tb_normalize_payload(payload),
    auto_unbox = FALSE
  ))
}

#' Translate an export payload into count-pipe query parameters
#'
#' Fields the pipe does not honor are dropped rather than sent. That makes the
#' count an over-estimate for those queries, which costs an occasional spurious
#' warning --- cheap, because the guard only warns.
#' @noRd
tb_count_params <- function(payload) {
  payload <- tb_drop_empty(payload)
  keep <- intersect(names(payload), names(tb_count_names))
  params <- payload[keep]
  names(params) <- unname(tb_count_names[keep])
  lapply(params, function(v) paste(as.character(v), collapse = ","))
}
