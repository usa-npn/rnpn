# dev/harness.R -- scratchpad for incremental development of rnpn.
#
# This file is NOT part of the package (see .Rbuildignore). It exists so you can
# poke at one function at a time without reinstalling anything.
#
# HOW TO USE IT
#   1. Open this file in VS Code.
#   2. Press Ctrl+Shift+S ("R: Source current file") or just put the cursor on a
#      line and hit Ctrl+Enter to send that one line to the R terminal.
#   3. Edit a function in R/, run `reload()`, re-run your call. That's the loop.
#
# Nothing here needs the package to be installed. `load_all()` reads the R/
# directory directly, so your edits are live after every `reload()`.

# ---------------------------------------------------------------------------
# Setup: load the package source
# ---------------------------------------------------------------------------

# load_all() simulates library(rnpn) but from source, and -- crucially -- it
# also exposes INTERNAL functions that aren't in NAMESPACE. That means you can
# call npn_get_common_query_vars(), explode_query(), bind_rows_safe(), etc.
# directly, which you cannot do with an installed package.
devtools::load_all(".")

reload <- function() devtools::load_all(".", quiet = TRUE)

# ---------------------------------------------------------------------------
# Harness helpers
# ---------------------------------------------------------------------------

#' Run an expression with full HTTP request/response logging.
#'
#' Every rnpn function ultimately goes through httr2, so this shows you the
#' exact URL, headers, and body that got sent. This is the single most useful
#' debugging tool in this package.
#'
#'   trace_req(npn_stations("AZ"))
#'   trace_req(npn_species_id(3), level = 3)   # level 3 also dumps the body
trace_req <- function(expr, level = 2) {
  httr2::with_verbosity(expr, verbosity = level)
}

#' Break into the debugger the *next* time `f` is called, then clear itself.
#'
#'   dbg(npn_get_common_query_vars)
#'   npn_download_status_data("harness", years = 2013, species_ids = 3)
#'   # -> drops you into the R Debugger at the top of npn_get_common_query_vars
#'
#' Works on internal functions too. Use `dbg_off(f)` if you used debug() instead.
dbg <- function(f) debugonce(f)
dbg_off <- function(f) undebug(f)

#' Peek at what a function returns without printing 50k rows.
#'
#'   peek(npn_stations())
peek <- function(x, n = 5) {
  cat("class: ", paste(class(x), collapse = "/"), "\n", sep = "")
  if (is.data.frame(x)) {
    cat("dim:   ", nrow(x), "x", ncol(x), "\n", sep = " ")
    cat("cols:  ", paste(names(x), collapse = ", "), "\n\n", sep = "")
    print(utils::head(x, n))
  } else {
    utils::str(x, max.level = 2, list.len = 10)
  }
  invisible(x)
}

#' Run one test file (or one test) without running the whole suite.
#'
#'   t1("stations")   # runs tests/testthat/test-npn-stations.R
#'   t1("stations", desc = "npn_stations_by_location functions")  # one test_that
#'
#' Note: these tests replay recorded API responses from tests/testthat/_vcr/.
#' If a test's behavior disagrees with what you see interactively, the cassette
#' is stale -- delete the matching .yml in _vcr/ and re-run to re-record.
t1 <- function(file, desc = NULL) {
  devtools::test(filter = file, desc = desc)
}

#' Long-running tests are skipped by default. Turn them on for a session.
long_tests_on  <- function() Sys.setenv(RNPN_SKIP_LONG_TESTS = "FALSE")
long_tests_off <- function() Sys.setenv(RNPN_SKIP_LONG_TESTS = "TRUE")

# --- post-mortem HTTP inspection -------------------------------------------
# You do NOT have to re-run a call with trace_req() to see what it sent. httr2
# remembers the last exchange, so after ANY rnpn call (or after a failure) you
# can just ask. This is usually faster than re-running.
#
#   npn_stations("AZ")
#   last_req()    # the request that went out
#   last_resp()   # the response that came back
last_req  <- function() httr2::last_request()
last_resp <- function() httr2::last_response()

#' Body of the last response, parsed.
last_body <- function() {
  r <- httr2::last_response()
  if (is.null(r)) return(NULL)
  tryCatch(httr2::resp_body_json(r), error = function(e) httr2::resp_body_string(r))
}

# --- error handling ---------------------------------------------------------
#' Drop into a frame picker whenever an error is thrown.
#'
#' Type a frame number to inspect that frame's variables, `0` to quit. This is
#' the R equivalent of "break on unhandled exception" and works in the Debug
#' Console. rnpn raises errors via rlang::abort(), so `rlang::last_trace()`
#' also gives a nice tree after the fact.
on_error_recover <- function() options(error = utils::recover)
on_error_default <- function() options(error = NULL)

# --- vcr / cassettes --------------------------------------------------------
#' Bypass recorded cassettes and hit the live API.
#'
#' Use when a test passes/fails differently than the same call run by hand --
#' that means the cassette in tests/testthat/_vcr/ is stale.
vcr_off <- function() vcr::turn_off()
vcr_on  <- function() vcr::turn_on()


# ---------------------------------------------------------------------------
# Scratch area -- edit freely, this is yours
# ---------------------------------------------------------------------------

## --- cheap calls, good for smoke-testing a change --------------------------
# peek(npn_stations("AZ"))
# peek(npn_species_id(3))
# blah <- npn_species_id(3)
# peek(npn_phenophases())
# peek(npn_species_types())

## --- see the actual HTTP traffic ------------------------------------------
# trace_req(npn_stations_with_spp(100))

## --- test an INTERNAL function in isolation (the main reason for load_all) --
# q <- npn_get_common_query_vars(
#   request_source = "harness",
#   species_ids    = c(3, 100),
#   states         = "AZ",
#   climate_data   = FALSE
# )
# str(q)
#
# explode_query("species_id", c(100, 103))
# bind_rows_safe(data.frame(a = "1"), data.frame(a = "2", b = "x"))

## --- a real download (slower; hits the live API) ---------------------------
# obs <- npn_download_status_data(
#   request_source = "harness",
#   years          = 2013,
#   species_ids    = 3,
#   station_ids    = 4881
# )
# peek(obs)
