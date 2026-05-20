# ============================================================================
# test_helpers.R
# ----------------------------------------------------------------------------
# Shared setup for the HIV Intervention Impact Calculator test suite.
#
# WHAT THIS FILE DOES:
#   1. Sources Mock-Up_logic_V2.R once, which triggers the SharePoint loads
#      and populates all globals (hiv_params, intervention_params,
#      intervention_groups, MORTALITY_RATES, etc.).
#   2. Provides fixture builders so each test starts from a known context
#      rather than the live country CSV.
#   3. Provides scoped override helpers (with_hiv_params, with_mortality_rates,
#      with_intervention_groups) that snapshot the global, swap in fixture
#      values for the duration of an expression, then restore. This lets us
#      test functions that read globals without permanently mutating them.
#
# REQUIREMENTS:
#   - Internet access (the sourced file downloads from SharePoint at load).
#   - Packages: testthat, shiny, bslib, DT, ggplot2, dplyr, tidyr, scales,
#     httr, readr, readxl.
#
# CONVENTIONS:
#   - All assertion failures should print enough info to derive the expected
#     value by hand from the doc block. Use expect_close() for floats with
#     tolerance 1e-6; expect_equal() for integers post-round().
# ============================================================================

suppressPackageStartupMessages({
  library(testthat)
})

# ---------------------------------------------------------------------------
# 1. Source the main logic file
# ---------------------------------------------------------------------------
# Path is taken from an environment variable so the suite can be run from
# anywhere; defaults to the uploads path used during development.
LOGIC_PATH <- Sys.getenv("HIV_LOGIC_PATH",
                        unset = "/mnt/user-data/uploads/Mock-Up_logic_V2.R")

if (!file.exists(LOGIC_PATH)) {
  stop(sprintf(
    "Cannot find logic file at '%s'. Set HIV_LOGIC_PATH env var to override.",
    LOGIC_PATH))
}

message("Sourcing logic file (this hits SharePoint and may take a few seconds)...")
tryCatch(
  source(LOGIC_PATH, local = FALSE),
  error = function(e) {
    stop(sprintf(
      "Failed to source %s. Most likely cause: SharePoint URLs in the file ",
      "are unreachable. Original error: %s", LOGIC_PATH, conditionMessage(e)))
  }
)
message("Source complete. Globals available: hiv_params, intervention_params, ",
        "intervention_groups, MORTALITY_RATES, MTCT_RATES, INFANT_MORTALITY_RATES, ",
        "regional_presets.")

# ---------------------------------------------------------------------------
# 2. Fixture builders
# ---------------------------------------------------------------------------
# make_fixture_context() returns a context list with deliberately round numbers
# so every downstream calculation can be derived on paper:
#
#   total_population        = 1,000,000
#   hiv_prevalence          = 0.05          -> plhiv = 50,000
#   percent_diagnosed       = 90            -> diagnosed = 45,000
#   percent_on_art          = 80            -> on_art   = 36,000
#   percent_suppressed      = 90            -> suppressed = 32,400
#   new_infections_per_year = 5,000
#   aids_deaths_per_year    = 2,500
#   birth_rate              = 25 (per 1000) -> births = 25,000
#   prop_pop_male           = 50            -> equal sex split
#   prop_pop_under_14       = 40            -> adult_pop = 600,000
#   anc_multiplier          = 1             -> hiv_exposed_births = 25,000 * 0.05 = 1,250
#   circ_prevalence         = 30 (%)        -> 30% of males circumcised
#   prop_high_risk          = 0.05          -> 5% of HIV-neg active are KP
#   rr_high                 = 4
#
# Override any field with a named arg, e.g. make_fixture_context(plhiv = 100000).
# ---------------------------------------------------------------------------
make_fixture_context <- function(...) {
  defaults <- list(
    total_population        = 1e6,
    hiv_prevalence          = 0.05,
    plhiv                   = 50000,
    percent_diagnosed       = 90,
    percent_on_art          = 80,
    percent_suppressed      = 90,
    new_infections_per_year = 5000,
    current_diagnoses       = 45000,
    aids_deaths_per_year    = 2500,
    birth_rate              = 25,
    prop_pop_male           = 50,
    prop_pop_under_14       = 40,
    anc_multiplier          = 1,
    circ_prevalence         = 30,
    prop_high_risk          = 0.05,
    rr_high                 = 4,
    test_yield              = 0.05,
    prior_year_tests        = NULL,
    prop_retesting          = 0.30,
    yield_multipliers       = list()
  )
  overrides <- list(...)
  modifyList(defaults, overrides)
}

# Minimal scenario_interventions list used by FOI tests. Mirrors the keys
# compute_prevention_adjustments expects. Zeros mean "no scale-up beyond
# what calibration already absorbed".
make_fixture_interventions <- function(...) {
  defaults <- list(
    prep_oral             = 0,
    prep_lenacapavir      = 0,
    condoms               = 0,
    pep                   = 0,
    vmmc                  = 0,
    eff_prep_oral         = 0.99,
    eff_prep_len          = 1.00,
    eff_condom            = 0.80,
    eff_pep               = 0.80,
    acts_per_year_high    = 100,
    acts_per_year_gen     = 50,
    condom_use_rate_high  = 0.75,
    condom_use_rate_gen   = 0.55
  )
  overrides <- list(...)
  modifyList(defaults, overrides)
}

# ---------------------------------------------------------------------------
# 3. Scoped global override helpers
# ---------------------------------------------------------------------------
# Pattern: snapshot the current value, assign a replacement at .GlobalEnv,
# register an on.exit restore on the *calling* frame. Tests can call these
# inline and the restore fires when the test_that() block exits.
#
# Usage inside a test_that() block:
#   with_hiv_params(list(prop_retest_default = 0.5))
#   result <- calculate_scenario_outcomes(...)
#
# The override is undone automatically at end of test_that().
# ---------------------------------------------------------------------------

with_hiv_params <- function(overrides, envir = parent.frame()) {
  stopifnot(is.list(overrides))
  snapshot <- hiv_params
  new_params <- modifyList(hiv_params, overrides)
  assign("hiv_params", new_params, envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute(assign("hiv_params", SNAP, envir = .GlobalEnv),
                          list(SNAP = snapshot)),
               add = TRUE),
          envir = envir)
  invisible(NULL)
}

with_mortality_rates <- function(overrides, envir = parent.frame()) {
  stopifnot(is.list(overrides))
  snapshot <- MORTALITY_RATES
  new_rates <- modifyList(MORTALITY_RATES, overrides)
  assign("MORTALITY_RATES", new_rates, envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute(assign("MORTALITY_RATES", SNAP, envir = .GlobalEnv),
                          list(SNAP = snapshot)),
               add = TRUE),
          envir = envir)
  invisible(NULL)
}

with_intervention_groups <- function(overrides, envir = parent.frame()) {
  stopifnot(is.list(overrides))
  snapshot <- intervention_groups
  new_groups <- modifyList(intervention_groups, overrides)
  assign("intervention_groups", new_groups, envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute(assign("intervention_groups", SNAP, envir = .GlobalEnv),
                          list(SNAP = snapshot)),
               add = TRUE),
          envir = envir)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# 4. Custom expectations
# ---------------------------------------------------------------------------
# expect_close: for floats, uses tolerance 1e-6 by default.
# expect_within_pct: for the FOI roundtrip and other "should be approximately"
#   checks where tolerance is a percentage of the expected value.
# ---------------------------------------------------------------------------

expect_close <- function(object, expected, tolerance = 1e-6,
                         label = NULL, ...) {
  expect_equal(object, expected, tolerance = tolerance,
               label = label, ...)
}

expect_within_pct <- function(object, expected, pct = 1) {
  pct_diff <- abs(object - expected) / max(abs(expected), 1e-12) * 100
  expect_lt(pct_diff, pct,
            label = sprintf("|%.4f - %.4f| / %.4f = %.3f%% (limit %g%%)",
                            object, expected, expected, pct_diff, pct))
}
