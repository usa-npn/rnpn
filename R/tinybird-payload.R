# Building, validating, and hashing an export request payload.
#
# Pure functions: nothing here touches the network or the disk.

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

#' Validate the fields supplied through `...`
#'
#' This is not optional. `additionalProperties: false` is declared in the spec
#' and **not enforced by the server**, and the count pipe behaves the same way,
#' so an unvalidated typo widens the query rather than erroring and there is no
#' server-side backstop anywhere.
#'
#' @param dots A named list of extra payload fields.
#' @param named_payload The payload fields already claimed by named arguments.
#' @noRd
tb_validate_dots <- function(dots, named_payload = character(), call = rlang::caller_env()) {
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
        paste0(
          "Cannot set ",
          tb_quote_list(reserved),
          " through `...`."
        ),
        "i" = "It identifies the data product and is set by this function."
      ),
      call = call
    )
  }

  unknown <- setdiff(nms, names(tb_export_properties))
  if (length(unknown) > 0) {
    hints <- tb_suggestions(unknown)
    rlang::abort(
      c(
        paste0(
          "Unrecognized filter ",
          if (length(unknown) > 1) "fields " else "field ",
          tb_quote_list(unknown),
          "."
        ),
        if (length(hints) > 0) rlang::set_names(hints, "i"),
        "i" = "The service silently ignores fields it does not recognize, so an unchecked typo would widen your query instead of failing."
      ),
      call = call
    )
  }

  clash <- intersect(nms, named_payload)
  if (length(clash) > 0) {
    r_names <- names(tb_payload_names)[match(clash, tb_payload_names)]
    rlang::abort(
      c(
        paste0(
          "Filter ",
          tb_quote_list(clash),
          " was supplied twice."
        ),
        "x" = paste0(
          "It is the payload field behind ",
          tb_quote_list(r_names),
          "."
        ),
        "i" = paste0("Use ", tb_quote_list(r_names), " only.")
      ),
      call = call
    )
  }

  invisible(dots)
}

#' Nearest accepted field name, for a typo
#' @noRd
tb_suggestions <- function(unknown) {
  valid <- names(tb_export_properties)
  out <- character(0)
  for (u in unknown) {
    d <- utils::adist(u, valid, ignore.case = TRUE)[1, ]
    if (min(d) <= max(2, nchar(u) %/% 3)) {
      out <- c(
        out,
        paste0("Did you mean `", valid[which.min(d)], "`?")
      )
    }
  }
  out
}

#' @noRd
tb_quote_list <- function(x) {
  paste0("`", x, "`", collapse = ", ")
}

#' Assemble an export request payload
#'
#' @param named A list keyed by *R-facing* argument names (see
#'   `tb_payload_names`). Entries that are `NULL` or empty are dropped.
#' @param dots A list of extra fields keyed by *payload* field name.
#' @param download_type The product identifier, set by the caller and never
#'   exposed to the user.
#' @returns A named list keyed by payload field name, with array-valued fields
#'   marked so that they survive JSON encoding as arrays.
#' @noRd
tb_build_payload <- function(
  named = list(),
  dots = list(),
  download_type,
  call = rlang::caller_env()
) {
  named <- tb_drop_empty(named)

  unmapped <- setdiff(names(named), names(tb_payload_names))
  if (length(unmapped) > 0) {
    # An internal wiring mistake, not a user error.
    rlang::abort(
      paste0(
        "No payload field is mapped for ",
        tb_quote_list(unmapped),
        "."
      )
    )
  }
  names(named) <- unname(tb_payload_names[names(named)])

  dots <- tb_drop_empty(dots)
  tb_validate_dots(dots, named_payload = names(named), call = call)

  payload <- c(named, dots)
  payload[["downloadType"]] <- download_type
  payload
}

#' Mark array-valued fields so JSON encoding keeps them as arrays
#'
#' `jsonlite` unboxes length-1 vectors, which would send `"species_ids": 3`
#' where the spec asks for `[3]`. `I()` opts a field out of that.
#' @noRd
tb_json_payload <- function(payload) {
  kinds <- tb_export_properties[names(payload)]
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
