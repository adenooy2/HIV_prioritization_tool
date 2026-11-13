# ============================================================================
# test_03_testing.R
# ----------------------------------------------------------------------------
# Tests for the testing intervention block inside calculate_scenario_outcomes():
#
#   - effective yield (base_test_yield × country mult × dilution factor)
#   - yield dilution: factor = 1 below threshold; half-yield formula above
#   - index testing: exempt from dilution; capped at 2× new_infections_per_year
#   - country yield_multipliers override default yield
#   - prop_new_dx + prop_reeng = 1
#   - linkage cost = linked × linkage_cost
#   - ANC/PNC HIV testing routes into PMTCT cascade (not the general yield path)
#   - testing_reengagement_cap_frac binds when testing volume produces more
#     retests than the LTFU pool can absorb
#
# These tests call calculate_scenario_outcomes() — the giant integration
# function — because the testing logic is woven through it, not a separate
# function we can call in isolation. To keep numbers reproducible, each
# test overrides specific intervention_groups entries with known values so
# we can derive expected outputs on paper rather than relying on the live
# SharePoint-loaded parameters.
# ============================================================================

source("helpers.R")

SAFR <- 0.60   # sexually_active_frac, locked for derivations

# ---------------------------------------------------------------------------
# Helper: build a fixture intervention_groups with one testing modality
# fully specified. Other testing modalities and groups remain live so the
# function can still iterate over them; we just zero their inputs in the
# `interventions` argument so they don't contribute.
# ---------------------------------------------------------------------------
override_test_modality <- function(int_key, efficacy, unit_cost,
                                   linkage_rate, linkage_cost) {
  new_groups <- intervention_groups
  new_groups$testing$interventions[[int_key]]$efficacy     <- efficacy
  new_groups$testing$interventions[[int_key]]$unit_cost    <- unit_cost
  new_groups$testing$interventions[[int_key]]$linkage_rate <- linkage_rate
  new_groups$testing$interventions[[int_key]]$linkage_cost <- linkage_cost
  new_groups
}

# Helper: a baseline interventions list of all zeros (so only the modality
# under test contributes to outputs).
zero_interventions <- function() {
  setNames(as.list(rep(0, length(default_baseline_interventions))),
           names(default_baseline_interventions))
}

# ---------------------------------------------------------------------------
# 3.1 prop_new_dx + prop_reeng = 1
# ---------------------------------------------------------------------------
# WHAT: Positive tests are split into first-time diagnoses (prop_new_dx) and
#       re-engagement candidates (prop_reeng). These shares MUST sum to 1.
# WHY:  If they sum to <1, positive tests silently disappear; >1 double-counts.
# HOW:  Set prop_retesting = 0.30 in context. Then prop_reeng = 0.30 and
#       prop_new_dx = 1 - 0.30 = 0.70. Verify via positive_tests >= 0 invariant
#       in a low-volume run; direct read isn't possible without exposing the
#       internals, so we use the indirect identity:
#         (new_diagnoses + re_engagement) <= positive_tests + 1
#       under the testing_reengagement cap (where some retests become no-ops).
# ---------------------------------------------------------------------------
test_that("new_diagnoses + re_engagement does not exceed positive_tests", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0))
  
  ctx <- make_fixture_context(prop_retesting = 0.30, test_yield = 0.05)
  pops <- calculate_populations(ctx)
  
  # Override one testing modality with known parameters
  new_groups <- override_test_modality("test_facility_general",
                                       efficacy = 1.0, unit_cost = 5,
                                       linkage_rate = 0.8, linkage_cost = 10)
  with_intervention_groups(list(testing = new_groups$testing))
  
  interv <- zero_interventions()
  interv$test_facility_general <- 10000
  
  result <- calculate_scenario_outcomes(
    context = ctx,
    interventions = interv,
    populations = pops,
    is_baseline = TRUE,
    baseline_interventions = interv
  )
  
  expect_gte(result$positive_tests + 1,
             result$new_diagnoses + result$re_engagement)
})

# ---------------------------------------------------------------------------
# 3.2 Yield dilution factor: factor = 1 below threshold
# ---------------------------------------------------------------------------
# WHAT: When total planned tests <= prior_year_tests, yield_dilution_factor = 1.0
#       so positive_tests = number_reached × yield × efficacy (no dilution).
# WHY:  The dilution mechanism is meant to penalise over-saturation only.
#       Below threshold it must be a no-op.
# HOW:  Set prior_year_tests = 100,000 in context. Run one modality at 10,000
#       tests (well under threshold). Yield = 0.05 (test_yield in context),
#       efficacy = 1.0, no country mult.
#         positive_tests = 10,000 × 0.05 × 1.0 = 500
#         new_diagnoses  = 500 × prop_new_dx (0.70) = 350
#       (new_diagnoses_cap_prop = 0.95, undiagnosed pool = 5,000, so cap = 4,750:
#        350 well under cap, no binding.)
# ---------------------------------------------------------------------------
test_that("yield dilution = 1.0 when total tests are below threshold", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0))
  
  # Override the testing modality with deterministic parameters
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 5
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.8
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(
    test_yield       = 0.05,
    prior_year_tests = 100000,
    prop_retesting   = 0.30,
    yield_multipliers = list()
  )
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 10000
  
  result <- calculate_scenario_outcomes(
    context = ctx, interventions = interv, populations = pops,
    is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected positive_tests = 10,000 × 0.05 × 1.0 = 500
  # Of which prop_new_dx (0.70) become new diagnoses
  expect_close(result$positive_tests, 500)
  expect_close(result$new_diagnoses,  350)
  expect_close(result$tests_performed, 10000)
})

# ---------------------------------------------------------------------------
# 3.3 Yield dilution factor: half-yield above threshold
# ---------------------------------------------------------------------------
# WHAT: When total > threshold, dilution_factor = (threshold + (total - threshold) × 0.5) / total
# WHY:  This is the explicit half-yield-on-excess formula from lines 1385-1389.
# HOW:  threshold = 10,000; total = 20,000.
#         factor = (10,000 + (20,000 - 10,000) × 0.5) / 20,000
#                = (10,000 + 5,000) / 20,000
#                = 0.75
#       Run: number_reached = 20,000, yield = 0.05, efficacy = 1.0.
#         positive_tests = 20,000 × 0.05 × 0.75 × 1.0 = 750
#         new_diagnoses  = 750 × 0.70 = 525
# ---------------------------------------------------------------------------
test_that("yield dilution applies half-yield formula above threshold", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0))
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 5
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.8
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(
    test_yield       = 0.05,
    prior_year_tests = 10000,
    prop_retesting   = 0.30,
    yield_multipliers = list()
  )
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 20000
  
  result <- calculate_scenario_outcomes(
    context = ctx, interventions = interv, populations = pops,
    is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: 20,000 × 0.05 × 0.75 × 1.0 = 750 positive tests
  expect_close(result$positive_tests, 750)
  expect_close(result$new_diagnoses,  750 * 0.70)
  expect_close(result$tests_performed, 20000)
})

# ---------------------------------------------------------------------------
# 3.4 Index testing IS subject to yield dilution (same as other modalities)
# ---------------------------------------------------------------------------
# WHAT: test_index uses the same yield_dilution_factor as facility/community
#       testing; it also contributes to the dilution denominator.
# WHY:  As of [this change] index testing is no longer treated as exempt from
#       saturation — contact pools degrade alongside the broader programme
#       when total testing volume exceeds prior-year throughput.
# HOW:  Setup: prior_year_tests = 10,000; facility = 20,000, index = 1,000.
#       Dilution denominator INCLUDES index now:
#         total_planned = 20,000 + 1,000 = 21,000.
#         factor = (10,000 + (21,000 - 10,000) × 0.5) / 21,000
#                = 15,500 / 21,000 ≈ 0.738095…
#       Expected positive_tests:
#         (20,000 + 1,000) × 0.05 × 0.738095… × 1.0
#         = 21,000 × 0.05 × 15,500 / 21,000
#         = 1,050 × 15,500 / 21,000
#         = 775.0  (exact)
# ---------------------------------------------------------------------------
test_that("index testing is subject to yield dilution like other modalities", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0))
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 5
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.8
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 10
  ig_new$testing$interventions$test_index$efficacy     <- 1.0
  ig_new$testing$interventions$test_index$unit_cost    <- 5
  ig_new$testing$interventions$test_index$linkage_rate <- 0.8
  ig_new$testing$interventions$test_index$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(
    test_yield       = 0.05,
    prior_year_tests = 10000,
    prop_retesting   = 0.30,
    yield_multipliers = list()
  )
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 20000
  interv$test_index            <- 1000
  
  result <- calculate_scenario_outcomes(
    context = ctx, interventions = interv, populations = pops,
    is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: 21,000 × 0.05 × (15,500 / 21,000) = 775.0
  expect_close(result$positive_tests, 775)
})

# ---------------------------------------------------------------------------
# 3.5 Index testing volume is NOT capped (regression guard)
# ---------------------------------------------------------------------------
# WHAT: number_reached for test_index is bounded only by the eligible
#       population — there is no programmatic 2× new-infections cap.
# WHY:  Guard against re-introduction of the old finite-contact-pool cap.
#       The cap was removed alongside subjecting index to yield dilution.
# HOW:  new_infections_per_year = 5,000 (would have implied old cap = 10,000).
#       Request 15,000 with dilution disabled (prior_year_tests = NULL).
#         tests_performed should be 15,000 — not 10,000.
#         positive_tests   = 15,000 × 0.05 × 1.0 × 1.0 = 750
# ---------------------------------------------------------------------------
test_that("index testing volume is not capped by new_infections_per_year", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0))
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_index$efficacy     <- 1.0
  ig_new$testing$interventions$test_index$unit_cost    <- 5
  ig_new$testing$interventions$test_index$linkage_rate <- 0.8
  ig_new$testing$interventions$test_index$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(
    new_infections_per_year = 5000,
    test_yield              = 0.05,
    prior_year_tests        = NULL,   # disable global dilution
    prop_retesting          = 0.30,
    yield_multipliers       = list()
  )
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_index <- 15000   # would have been clipped to 10,000 under old cap
  
  result <- calculate_scenario_outcomes(
    context = ctx, interventions = interv, populations = pops,
    is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$tests_performed, 15000)  # uncapped
  expect_close(result$positive_tests,  750)    # 15,000 × 0.05 × 1.0 × 1.0
})

# ---------------------------------------------------------------------------
# 3.6 Country yield_multipliers override default yield
# ---------------------------------------------------------------------------
# WHAT: Each testing modality can have a country-specific yield multiplier
#       from baseline CSV; applied as: effective_yield = base × mult.
# WHY:  Modality effectiveness varies by setting; overrides let us calibrate.
# HOW:  Set yield_multipliers = list(test_facility_general = 2.0).
#       Base yield = 0.05; effective = 0.10. Run 10,000 tests below threshold
#       (no dilution).
#         positive_tests = 10,000 × 0.10 × 1.0 = 1,000
#       Doubling the multiplier doubles the positive test count, holding
#       everything else constant.
# ---------------------------------------------------------------------------
test_that("country yield_multipliers scale the base yield", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0))
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 5
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.8
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx_baseline <- make_fixture_context(
    test_yield       = 0.05,
    prior_year_tests = NULL,
    prop_retesting   = 0.30,
    yield_multipliers = list(test_facility_general = 1.0)
  )
  pops <- calculate_populations(ctx_baseline)
  
  ctx_doubled <- ctx_baseline
  ctx_doubled$yield_multipliers <- list(test_facility_general = 2.0)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 10000
  
  r1 <- calculate_scenario_outcomes(ctx_baseline, interv, pops,
                                    is_baseline = TRUE,
                                    baseline_interventions = interv)
  r2 <- calculate_scenario_outcomes(ctx_doubled, interv, pops,
                                    is_baseline = TRUE,
                                    baseline_interventions = interv)
  
  # Doubling mult doubles positive_tests (10,000 × 0.05 × 1.0 = 500 vs 1,000)
  expect_close(r1$positive_tests, 500)
  expect_close(r2$positive_tests, 1000)
})

# ---------------------------------------------------------------------------
# 3.7 Linkage cost = positive tests × linkage_cost (unit cost charged separately)
# ---------------------------------------------------------------------------
# WHAT: For each testing modality, two cost components accrue:
#       (a) unit_cost × number_reached  (every test, regardless of result)
#       (b) linkage_cost × positive tests      (costs associated with trying to get indiviudals into further care)

# HOW:  10,000 tests below threshold, yield = 0.05, prop_new_dx = 0.70,
#       prop_reeng = 0.30, linkage_rate = 0.8, efficacy = 1.0.
#         positive_tests = 500; new_dx = 350; retest_pos = 150
#         # Expected linkage cost (per POSITIVE, not per linked):
#   positives = 10,000 × 0.05 = 500
#   linkage cost = 500 × 10 = 5,000
# Expected unit cost: 10,000 × 5 = 50,000
# # Total intervention cost: 55,000
# ---------------------------------------------------------------------------
test_that("testing modality cost = unit × tests + linkage_cost × linked", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0))
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 5
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.8
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 10
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(
    test_yield       = 0.05,
    prior_year_tests = NULL,
    prop_retesting   = 0.30,
    yield_multipliers = list()
  )
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 10000
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops,
    is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected linkage cost (per POSITIVE, not per linked):
  #   positives = 10,000 × 0.05 = 500
  #   linkage cost = 500 × 10 = 5,000
  # Expected unit cost: 10,000 × 5 = 50,000
  # Total intervention cost: 55,000
  expect_close(result$total_intervention_cost, 55000)
})

# ---------------------------------------------------------------------------
# 3.8 ANC HIV testing routes into PMTCT cascade, not general yield
# ---------------------------------------------------------------------------
# WHAT: For anc_hiv_testing, the general new_diagnoses/re_engagement_testing
#       accumulators are SKIPPED (lines 1496-1518 guard). Instead, the post-
#       loop PMTCT routing block adds pmtct_new_diagnoses to new_diagnoses
#       using the PMTCT-specific yield (HIV+ undiagnosed pregnant / testable).
# WHY:  PMTCT-specific yield is much higher than general positivity; using the
#       general path would understate ANC's contribution to the cascade.
# HOW:  Run anc_hiv_testing = 100 (i.e. 100% coverage of pregnant_hiv_testable).
#       Override anc_hiv_testing efficacy = 1.0, unit_cost = 0, linkage_rate
#       = 1.0, linkage_cost = 0 to make the arithmetic clean.
#         pregnant_hiv_testable = 23,875 (from test 1.8)
#         pregnant_undiagnosed  = 125 (from test 1.7)
#         number_reached        = 23,875 × 1.0 = 23,875
#         anc_hiv_yield         = 125 / 23,875 = 0.005235...
#         pmtct_candidates      = 23,875 × 0.005235... × 1.0 = 125
#         pmtct_new_diagnoses   = min(125, 125) = 125
#       After loop, pmtct_new_diagnoses (125) is added to new_diagnoses.
#       Confirm new_diagnoses == 125 (no general-path contribution).
#       Also confirm tests_performed accrues the full 23,875.
# ---------------------------------------------------------------------------
test_that("anc_hiv_testing routes into PMTCT cascade exclusively", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       pmtct_cascade_supp_discount = 0.90,
                       average_linkage_cap = 1.0))
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$anc_hiv_testing$efficacy     <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$unit_cost    <- 0
  ig_new$testing$interventions$anc_hiv_testing$linkage_rate <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(
    test_yield       = 0.05,
    prior_year_tests = NULL,
    prop_retesting   = 0.30,
    yield_multipliers = list()
  )
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$anc_hiv_testing <- 100   # 100% coverage
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops,
    is_baseline = TRUE, baseline_interventions = interv
  )
  
  # The PMTCT yield should produce ~125 new diagnoses (= pregnant_undiagnosed).
  # Tolerance ±1 for round() inside the function.
  expect_lte(abs(result$new_diagnoses - 125), 1)
  # tests_performed should reflect the full ANC coverage volume.
  expect_close(result$tests_performed, 23875)
})

# ---------------------------------------------------------------------------
# 3.9 Testing re-engagement cap binds when volume exceeds LTFU pool
# ---------------------------------------------------------------------------
# WHAT: re_engagement_testing is capped at total_ltfu_pool × testing_reengagement_cap_frac.
# WHY:  Pathological volumes can produce more positive retests than the LTFU
#       pool can contain (e.g. Mozambique example in source comments). The
#       cap reflects the physical ceiling on testing-driven re-engagement.
# HOW:  Override testing_reengagement_cap_frac = 0.45.
#       Set baseline LTFU pool: prevalent ltfu = 9,000; ltfu_new = 2,340 (test 1.9).
#       total_ltfu_pool = 9,000 + ltfu_new_effective. With zero retention
#       interventions, ltfu_new_effective = ltfu_new = 2,340.
#         total_ltfu_pool          = 9,000 + 2,340 = 11,340
#         testing_reengagement_cap = 11,340 × 0.45 = 5,103
#       To force the cap to bind, run massive testing volume.
#       Use unit_cost = 0, linkage = 0 so cost is small and uninteresting.
#       Override prop_retesting = 0.30 -> retest_pos share = 0.30.
#       With 1,000,000 tests, yield = 0.10, efficacy = 1.0, no dilution
#       (prior_year_tests = NULL):
#         positive_tests (raw) = 1,000,000 × 0.10 × 1.0 = 100,000
#         retest_pos    (raw) = 100,000 × 0.30          = 30,000
#       30,000 >> 5,103 cap -> re_engagement = 5,103.
# ---------------------------------------------------------------------------
test_that("testing_reengagement_cap binds when retest volume exceeds LTFU pool", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0,
                       prop_on_art_stable_diff = 0))
  
  # Override LTFU rates so ltfu_new is deterministic (matches test 1.9 setup)
  old_stable   <- ANNUAL_LTFU_RATE_STABLE
  old_unstable <- ANNUAL_LTFU_RATE_UNSTABLE
  assign("ANNUAL_LTFU_RATE_STABLE",   0.05, envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE", 0.20, envir = .GlobalEnv)
  on.exit({
    assign("ANNUAL_LTFU_RATE_STABLE",   old_stable,   envir = .GlobalEnv)
    assign("ANNUAL_LTFU_RATE_UNSTABLE", old_unstable, envir = .GlobalEnv)
  }, add = TRUE)
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(
    test_yield       = 0.10,
    prior_year_tests = NULL,
    prop_retesting   = 0.30,
    yield_multipliers = list()
  )
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 1000000   # pathological volume
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops,
    is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: re_engagement = 11,340 × 0.45 = 5,103 (within rounding ±1)
  expect_lte(abs(result$re_engagement - 5103), 1)
})

# ---------------------------------------------------------------------------
# 3.10 New diagnoses cap: cannot diagnose more than X% of undiagnosed
# ---------------------------------------------------------------------------
# WHAT: new_diagnoses is capped at populations$undiagnosed × new_diagnoses_cap_prop.
# WHY:  Even with infinite testing, you can't find more than ~95% of undiagnosed
#       PLHIV in one year (residual non-testers).
# HOW:  undiagnosed = 5,000 (fixture); new_diagnoses_cap_prop = 0.95.
#         cap = 4,750
#       Run massive testing volume to ensure raw new_dx > cap.
#       Raw new_dx at 1M tests × 0.10 yield × 0.70 prop_new_dx = 70,000.
#       After cap, new_diagnoses = 4,750.
# ---------------------------------------------------------------------------
test_that("new_diagnoses is capped at undiagnosed × cap_prop", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.30,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.90,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 1.0))
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(
    test_yield       = 0.10,
    prior_year_tests = NULL,
    prop_retesting   = 0.30,
    yield_multipliers = list()
  )
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 1000000
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops,
    is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Cap: 5,000 × 0.95 = 4,750
  expect_lte(abs(result$new_diagnoses - 4750), 1)
})