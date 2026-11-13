# ============================================================================
# test_11_integration.R
# ----------------------------------------------------------------------------
# End-to-end integration tests: run calculate_scenario_outcomes() TWICE
# (baseline + scenario) and feed the actual result lists into
# calculate_scenario_difference().
#
# WHY a separate file from test_10:
#   test_10 uses synthetic baseline/scenario lists with hand-picked field
#   values, which makes it fast and isolates failures to the diff function
#   itself. test_11 catches a different class of bug — field-name drift
#   between the two functions. If a key in calculate_scenario_outcomes()'s
#   return list is renamed (e.g. "end_diagnosed" -> "end_diagnoses") and
#   calculate_scenario_difference() isn't updated to match, test_10 still
#   passes (synthetic dict has the old key) but test_11 fails (real return
#   list has the new key).
#
# These tests are slow — each calls calculate_scenario_outcomes() twice.
# Keep the set small and focused on integration concerns, not model logic.
#
#   11.1  Every field in calculate_scenario_difference's output is non-NULL
#         when fed real result lists (catches missing-key bugs).
#   11.2  Self-comparison (baseline vs baseline) yields all-zero diffs even
#         when running the real simulator.
#   11.3  Scale-up scenario (more testing) produces sensible diff signs:
#         more new_diagnoses, fewer infections (eventually), positive
#         infections_averted, positive intervention cost diff.
#   11.4  Scale-down scenario (less testing than baseline) inverts those signs.
#   11.5  diff_total_cost = diff_intervention_cost + diff_art_provision_cost
#         when computed from real simulator outputs (cross-check the bookkeeping
#         identity from test 10.10, but against actual outputs).
# ============================================================================

source("helpers.R")

LIVE_PARAMS_INTEG <- list(
  sexually_active_frac            = 0.85,
  ltfu_rate_stable                = 0.044,
  ltfu_rate_unstable               = 0.14,
  spontaneous_reengagement_rate    = 0,
  retention_suppression_rate       = 0.41,
  tracking_reengagement_supp       = 0.9,
  prop_on_art_stable_diff          = 0,
  testing_reengagement_cap_frac    = 0.45,
  testing_art_init_supp            = 0.9,
  prop_retest_default              = 0.59,
  new_diagnoses_cap_prop           = 0.95,
  average_linkage_cap              = 0.93,
  pmtct_cascade_supp_discount      = 0.9,
  eid_supp_rate                    = 0.38,
  prop_cd4_ahd                     = 0.4
)

LIVE_MTCT_RATES_I <- list(on_art_suppressed = 0.0033,
                          on_art_unsuppressed = 0.037,
                          not_on_art = 0.2)
LIVE_INFANT_MORT_I <- list(untreated = 0.3, on_art = 0.06, suppressed = 0.03)
LIVE_MORT_I <- list(
  untreated_undiagnosed = 0.012, new_art_initiations = 0.006,
  treated = 0.0051, suppressed = 0.0051,
  ahd_new = 0.144, ahd_established = 0.028, ahd_untreated = 0.254,
  prop_ahd = list(undiagnosed = 0.154, diagnosed_not_art = 0.209,
                  new_initiations = 0.209, established_treated = 0.295,
                  established_supp = 0.043)
)

zero_interventions <- function() {
  setNames(as.list(rep(0, length(default_baseline_interventions))),
           names(default_baseline_interventions))
}

# Single helper that sets up all the source-time globals needed by the
# integration runs.
override_integ_globals <- function(envir = parent.frame()) {
  snap <- list(s = ANNUAL_LTFU_RATE_STABLE, u = ANNUAL_LTFU_RATE_UNSTABLE,
               sp = ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE,
               rs = RETENTION_SUPPRESSION_RATE,
               cd4 = CD4_AHD_TARGETING_YIELD, use = USE_MORTALITY_CALIBRATION,
               mort = MORTALITY_RATES, mtct = MTCT_RATES,
               infant = INFANT_MORTALITY_RATES)
  assign("ANNUAL_LTFU_RATE_STABLE",              0.044, envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE",            0.14,  envir = .GlobalEnv)
  assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", 0,     envir = .GlobalEnv)
  assign("RETENTION_SUPPRESSION_RATE",           0.41,  envir = .GlobalEnv)
  assign("CD4_AHD_TARGETING_YIELD",              0.4,   envir = .GlobalEnv)
  assign("USE_MORTALITY_CALIBRATION",            FALSE, envir = .GlobalEnv)
  assign("MORTALITY_RATES",                      LIVE_MORT_I,         envir = .GlobalEnv)
  assign("MTCT_RATES",                           LIVE_MTCT_RATES_I,   envir = .GlobalEnv)
  assign("INFANT_MORTALITY_RATES",               LIVE_INFANT_MORT_I,  envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute({
            assign("ANNUAL_LTFU_RATE_STABLE",              SNAP_S,    envir = .GlobalEnv)
            assign("ANNUAL_LTFU_RATE_UNSTABLE",            SNAP_U,    envir = .GlobalEnv)
            assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", SNAP_SP,   envir = .GlobalEnv)
            assign("RETENTION_SUPPRESSION_RATE",           SNAP_RS,   envir = .GlobalEnv)
            assign("CD4_AHD_TARGETING_YIELD",              SNAP_CD4,  envir = .GlobalEnv)
            assign("USE_MORTALITY_CALIBRATION",            SNAP_USE,  envir = .GlobalEnv)
            assign("MORTALITY_RATES",                      SNAP_MORT, envir = .GlobalEnv)
            assign("MTCT_RATES",                           SNAP_MTCT, envir = .GlobalEnv)
            assign("INFANT_MORTALITY_RATES",               SNAP_INF,  envir = .GlobalEnv)
          }, list(SNAP_S = snap$s, SNAP_U = snap$u, SNAP_SP = snap$sp,
                  SNAP_RS = snap$rs, SNAP_CD4 = snap$cd4, SNAP_USE = snap$use,
                  SNAP_MORT = snap$mort, SNAP_MTCT = snap$mtct,
                  SNAP_INF = snap$infant)),
          add = TRUE), envir = envir)
  invisible(NULL)
}

# Helper: standard ctx for integration tests
int_ctx <- function() {
  make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                       yield_multipliers = list())
}

# ---------------------------------------------------------------------------
# 11.1 All diff fields are populated when fed real result lists
# ---------------------------------------------------------------------------
# WHAT: Run the simulator twice, pass to calculate_scenario_difference, verify
#       every expected output key is present and NON-NULL.
# WHY: This is the field-name-drift catcher. If any key in
#      calculate_scenario_outcomes()'s return list is renamed without updating
#      calculate_scenario_difference()'s reads, the corresponding diff_* field
#      becomes NULL (R's $ operator returns NULL for missing keys).
# HOW: Standard fixture. Baseline = zero interventions. Scenario = modest
#      testing scale-up. List the 19 expected diff keys explicitly; for each,
#      assert is.numeric() and !is.null().
# ---------------------------------------------------------------------------
test_that("every diff field is populated when fed real simulator output", {
  with_hiv_params(LIVE_PARAMS_INTEG)
  override_integ_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.9
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  pops <- calculate_populations(int_ctx())
  
  base_interv <- zero_interventions()
  scen_interv <- zero_interventions()
  scen_interv$test_facility_general <- 10000
  
  baseline <- calculate_scenario_outcomes(int_ctx(), base_interv, pops,
                                          is_baseline = TRUE,
                                          baseline_interventions = base_interv)
  scenario <- calculate_scenario_outcomes(int_ctx(), scen_interv, pops,
                                          is_baseline = FALSE,
                                          baseline_interventions = base_interv,
                                          baseline_end_suppressed = baseline$end_suppressed)
  
  d <- calculate_scenario_difference(scenario, baseline)
  
  expected_keys <- c(
    "diff_diagnosed", "diff_on_art", "diff_suppressed",
    "diff_tests_performed", "diff_positive_tests", "diff_new_diagnoses",
    "diff_art_initiations",
    "diff_ltfu_new_effective", "diff_ltfu_prevented", "diff_ltfu_reengaged",
    "diff_new_infections", "diff_infant_infections", "diff_total_infections",
    "diff_deaths",
    "additional_infections_averted", "additional_deaths_averted",
    "diff_intervention_cost", "diff_art_provision_cost", "diff_total_cost",
    "scale_up_cost", "scale_down_savings"
  )
  for (k in expected_keys) {
    expect_false(is.null(d[[k]]), info = sprintf("key %s is NULL", k))
    expect_true(is.numeric(d[[k]]),
                info = sprintf("key %s is not numeric (got %s)", k, class(d[[k]])))
  }
})

# ---------------------------------------------------------------------------
# 11.2 Self-comparison: baseline vs identical baseline yields zero diffs
# ---------------------------------------------------------------------------
# WHAT: When the same result list is passed as both arguments, every diff_*
#       must equal 0, and scale_up_cost / scale_down_savings must be 0.
# WHY: Test 10.9 verifies this with synthetic identical lists. test_11.2
#      verifies it on REAL simulator output — catches a hypothetical bug
#      where some field has hidden internal state (e.g. a counter that
#      increments each time the field is read).
# HOW: Run once with zero interventions, pass the result list as both
#      scenario and baseline.
# ---------------------------------------------------------------------------
test_that("self-comparison of real simulator output yields all-zero diffs", {
  with_hiv_params(LIVE_PARAMS_INTEG)
  override_integ_globals()
  
  pops <- calculate_populations(int_ctx())
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(int_ctx(), interv, pops,
                                        is_baseline = TRUE,
                                        baseline_interventions = interv)
  
  d <- calculate_scenario_difference(result, result)
  
  expect_close(d$diff_diagnosed,           0)
  expect_close(d$diff_on_art,              0)
  expect_close(d$diff_suppressed,          0)
  expect_close(d$diff_new_infections,      0)
  expect_close(d$diff_deaths,              0)
  expect_close(d$additional_infections_averted, 0)
  expect_close(d$additional_deaths_averted,     0)
  expect_close(d$diff_intervention_cost,   0)
  expect_close(d$diff_art_provision_cost,  0)
  expect_close(d$diff_total_cost,          0)
  expect_close(d$scale_up_cost,            0)
  expect_close(d$scale_down_savings,       0)
})

# ---------------------------------------------------------------------------
# 11.3 Scale-up scenario: signs are coherent for a more-intervention scenario
# ---------------------------------------------------------------------------
# WHAT: Scenario with MORE testing than baseline should produce:
#         - diff_tests_performed > 0           (more tests)
#         - diff_new_diagnoses    > 0           (more people found)
#         - diff_art_initiations  > 0           (more linkages)
#         - diff_intervention_cost > 0          (it cost more)
#         - additional_deaths_averted ≥ 0       (more on ART → fewer deaths;
#           note: this can be very small in a single-year run because
#           established_treated mortality dominates)
#         - diff_diagnosed > 0                  (cascade improves)
# WHY: This is the canonical "scaling up testing helps" scenario. If any
#      sign is wrong, the tool would mislead users.
# HOW: baseline = zero. scenario = 10,000 facility tests.
# ---------------------------------------------------------------------------
test_that("scale-up scenario produces coherent positive signs", {
  with_hiv_params(LIVE_PARAMS_INTEG)
  override_integ_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.9
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 5
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  pops <- calculate_populations(int_ctx())
  
  base_interv <- zero_interventions()
  scen_interv <- zero_interventions()
  scen_interv$test_facility_general <- 10000
  
  baseline <- calculate_scenario_outcomes(int_ctx(), base_interv, pops,
                                          is_baseline = TRUE,
                                          baseline_interventions = base_interv)
  scenario <- calculate_scenario_outcomes(int_ctx(), scen_interv, pops,
                                          is_baseline = FALSE,
                                          baseline_interventions = base_interv,
                                          baseline_end_suppressed = baseline$end_suppressed)
  
  d <- calculate_scenario_difference(scenario, baseline)
  
  expect_gt(d$diff_tests_performed,    0)
  expect_gt(d$diff_new_diagnoses,      0)
  expect_gt(d$diff_art_initiations,    0)
  expect_gt(d$diff_diagnosed,          0)
  expect_gt(d$diff_intervention_cost,  0)
  expect_gte(d$additional_deaths_averted, 0)
  expect_gt(d$scale_up_cost,           0)
  expect_close(d$scale_down_savings,   0)
})

# ---------------------------------------------------------------------------
# 11.4 Scale-down scenario: signs flip
# ---------------------------------------------------------------------------
# WHAT: Baseline with HIGHER testing than scenario should invert the signs
#       from 11.3.
# WHY: Confirms the diff function isn't sign-biased — it works in both
#      scale-up and scale-down directions.
# HOW: baseline has 10,000 facility tests; scenario has 0. Diffs should be:
#         - diff_tests_performed   < 0
#         - diff_new_diagnoses     < 0
#         - diff_intervention_cost < 0   (scenario costs LESS)
#         - scale_up_cost          = 0
#         - scale_down_savings     > 0   (the savings carry the gap)
# ---------------------------------------------------------------------------
test_that("scale-down scenario inverts signs and routes cost gap to savings", {
  with_hiv_params(LIVE_PARAMS_INTEG)
  override_integ_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.9
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 5
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  pops <- calculate_populations(int_ctx())
  
  base_interv <- zero_interventions()
  base_interv$test_facility_general <- 10000
  scen_interv <- zero_interventions()  # nothing in scenario
  
  baseline <- calculate_scenario_outcomes(int_ctx(), base_interv, pops,
                                          is_baseline = TRUE,
                                          baseline_interventions = base_interv)
  scenario <- calculate_scenario_outcomes(int_ctx(), scen_interv, pops,
                                          is_baseline = FALSE,
                                          baseline_interventions = base_interv,
                                          baseline_end_suppressed = baseline$end_suppressed)
  
  d <- calculate_scenario_difference(scenario, baseline)
  
  expect_lt(d$diff_tests_performed,    0)
  expect_lt(d$diff_new_diagnoses,      0)
  expect_lt(d$diff_intervention_cost,  0)
  expect_close(d$scale_up_cost,        0)
  expect_gt(d$scale_down_savings,      0)
})

# ---------------------------------------------------------------------------
# 11.5 Cost identity: diff_total = diff_intervention + diff_art_provision
# ---------------------------------------------------------------------------
# WHAT: This identity is verified with synthetic data in test 10.10. Here we
#       verify it holds when the underlying lists come from the real simulator.
# WHY: If calculate_scenario_outcomes were to drift in how it builds
#      total_cost (e.g. adding a hidden charge that isn't in the two
#      sub-components), test 10 would still pass but this one would fail.
# HOW: Standard scale-up scenario from 11.3.
# ---------------------------------------------------------------------------
test_that("real-output cost identity: diff_total = diff_intervention + diff_art_provision", {
  with_hiv_params(LIVE_PARAMS_INTEG)
  override_integ_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.9
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 5
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  pops <- calculate_populations(int_ctx())
  
  base_interv <- zero_interventions()
  scen_interv <- zero_interventions()
  scen_interv$test_facility_general <- 10000
  
  baseline <- calculate_scenario_outcomes(int_ctx(), base_interv, pops,
                                          is_baseline = TRUE,
                                          baseline_interventions = base_interv)
  scenario <- calculate_scenario_outcomes(int_ctx(), scen_interv, pops,
                                          is_baseline = FALSE,
                                          baseline_interventions = base_interv,
                                          baseline_end_suppressed = baseline$end_suppressed)
  
  d <- calculate_scenario_difference(scenario, baseline)
  
  # Use a small tolerance for floating-point sum (multiple round() layers
  # in the source: each of total_cost, total_intervention_cost, and
  # art_provision_cost is independently rounded, and we then take diffs
  # of those rounded values, so accumulated drift can be a few dollars).
  expect_close(d$diff_total_cost,
               d$diff_intervention_cost + d$diff_art_provision_cost,
               tolerance = 4)
})