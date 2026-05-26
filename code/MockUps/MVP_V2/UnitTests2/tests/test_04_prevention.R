# ============================================================================
# test_04_prevention.R
# ----------------------------------------------------------------------------
# Tests for the prevention cost loop and prevention-stratum interactions that
# weren't fully covered in test_02_strata_foi.R:
#
#   - PrEP + condom stacking in the high-risk stratum (multiplicative residual)
#   - VMMC + condom interaction: men shifted to circ pool retain condom coverage
#     (line 1062-1066 fix to a previous bug)
#   - Prevention cost loop (lines 2227-2249):
#       * condoms cost uses `intervention_value` (raw distributed count) not
#         `number_reached`
#       * other adult_infections interventions use `number_reached`
#       * infant_prophylaxis cost charged in intervention loop, not here
#
# Test 02 already covers:
#   - PrEP-only protection_high arithmetic (2.5)
#   - condom demand-weighted allocation (2.6)
#   - VMMC denominator and coverage cap (2.7)
#   - Monotonicity / sign checks (2.8, 2.9)
#   - Baseline FOI roundtrip with prevention (2.4)
#
# This file fills gaps in stacking interactions and the cost side.
# ============================================================================

source("helpers.R")

SAFR <- 0.85  # live sexually_active_frac

# Live live prevention efficacies are not in the param CSV (they live in
# intervention_params Excel). We override per test to keep derivations clean.

# Helper: build deterministic strata + populations + scenario context
prev_setup <- function() {
  with_hiv_params(list(sexually_active_frac = SAFR), envir = parent.frame())
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  list(ctx = ctx, pops = pops, sp = sp, strata = strata)
}

zero_interventions <- function() {
  setNames(as.list(rep(0, length(default_baseline_interventions))),
           names(default_baseline_interventions))
}

# ---------------------------------------------------------------------------
# Live-param fixture for cost-cap tests (4.8a/b/c, 4.9)
# ---------------------------------------------------------------------------
# Only sexually_active_frac is needed for the asserted values (the caps depend
# on adult_pop and uncircumcised_males_all from the fixture context, not on
# SAFR). Kept minimal and consistent with the rest of test_04, which only
# overrides SAFR via prev_setup().
LIVE_PARAMS_PREVENTION_04 <- list(sexually_active_frac = SAFR)

# Local copy of test_09_costs.R::base_ctx — each test file runs in its own
# testthat context, so helpers defined in other test files are not in scope.
base_ctx <- function() {
  make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                       yield_multipliers = list())
}

# ---------------------------------------------------------------------------
# 4.3 PrEP + condom stacking in high-risk (multiplicative)
# ---------------------------------------------------------------------------
# WHAT: protection_high = 1 - (1 - prep_oral_cov × eff) × (1 - prep_len_cov × eff_len) ×
#                              (1 - condom_cov_high × eff_condom)
#       Multiplicative residual prevents double-counting when interventions
#       overlap on the same person.
# WHY: Locks the stacking formula. Independent terms should NOT sum.
# HOW: n_high_risk = 24,225 (from 4.1 derivation).
#      Prep oral = 4,845 -> cov = 0.20. eff_prep_oral = 0.99.
#      Condoms = total such that condom_cov_high = 0.20. From 2.6 derivation:
#        condom_cov_high = total × use_rate_high / total_acts (live: use_rate_high = 0.83).
#        acts_high_total = 24,225 × 500 (live acts_per_year_high = 500) = 12,112,500
#        acts_gen_total  = 460,275 × 50 = 23,013,750
#        total_acts      = 35,126,250
#        For condom_cov_high = 0.20: total × 0.83 / 35,126,250 = 0.20
#                                  -> total = 0.20 × 35,126,250 / 0.83 = 8,464,759.0
#      eff_condom = 0.80.
#        residual_high   = (1 - 0.20 × 0.99) × (1 - 0 × 1.0) × (1 - 0.20 × 0.80)
#                        = (1 - 0.198)      × 1                × (1 - 0.16)
#                        = 0.802 × 1 × 0.84
#                        = 0.67368
#        protection_high = 1 - 0.67368 = 0.32632
# ---------------------------------------------------------------------------
test_that("PrEP + condom stack multiplicatively in high-risk stratum", {
  s   <- prev_setup()
  # Override condom behaviour params to live values from CSV
  interv <- make_fixture_interventions(
    prep_oral             = 4845,
    condoms               = 8464759,
    eff_prep_oral         = 0.99,
    eff_condom            = 0.80,
    acts_per_year_high    = 500,    # live value
    acts_per_year_gen     = 50,     # live value
    condom_use_rate_high  = 0.83,   # live value
    condom_use_rate_gen   = 0.48    # live value
  )
  adj <- compute_prevention_adjustments(interv, s$strata, s$pops, s$sp)

  # protection = 1 - 0.802 × 0.84 = 1 - 0.67368 = 0.32632
  expect_close(adj$protection_high, 0.32632, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 4.4 vmmc_coverage_frac alone does NOT change protection_gen_male_circ
# ---------------------------------------------------------------------------
# WHAT: VMMC shifts men between strata but doesn't itself appear in any
#       protection_* term. The condom terms for circ_male are independent
#       of vmmc_coverage_frac.
# WHY: Verifies the structural separation between coverage-frac (used for
#      stratum-shift in main FOI) and protection (used for residual within
#      stratum). A bug that conflates them would over-credit VMMC by both
#      shifting and protecting the same men.
# HOW: vmmc = 50,000; condoms = 0.
#      vmmc_coverage_frac > 0; all protection_* = 0.
# ---------------------------------------------------------------------------
test_that("VMMC alone leaves stratum protection_* at 0", {
  s   <- prev_setup()
  interv <- make_fixture_interventions(vmmc = 50000)
  adj <- compute_prevention_adjustments(interv, s$strata, s$pops, s$sp)

  expect_gt(adj$vmmc_coverage_frac, 0)
  expect_close(adj$protection_gen_male_circ, 0)
  expect_close(adj$protection_gen_male_unc,  0)
  expect_close(adj$protection_high,          0)
})

# ---------------------------------------------------------------------------
# 4.7 Cost loop: condoms uses `intervention_value` (raw count), not number_reached
# ---------------------------------------------------------------------------
# WHAT: Line 2242-2243: for condoms, `units_costed = intervention_value`.
#       For all other adult_infections interventions, `units_costed =
#       number_reached` (capped at eligible_pop).
# WHY: Locks the special-case for condoms. The intent is that condom unit
#      cost is per CONDOM DISTRIBUTED, not per person reached — so the cost
#      should not be capped at the sexually_active_negative pool size.
# HOW: Override condoms efficacy/unit_cost to known values. Set condoms =
#      10,000,000 (well above eligible_pop). Expected cost = 10M × unit_cost.
#      Without the special case, cost would be capped at sexually_active_negative
#      × unit_cost (much smaller).
# ---------------------------------------------------------------------------
test_that("condoms cost uses raw intervention_value, not number_reached", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.59,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.9,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 0.93,
                       pmtct_cascade_supp_discount = 0.9))

  ig_new <- intervention_groups
  ig_new$prevention$interventions$condoms$unit_cost <- 0.10
  ig_new$prevention$interventions$condoms$efficacy  <- 0.80
  with_intervention_groups(list(prevention = ig_new$prevention))

  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$condoms <- 1e7   # well above sexually_active_negative pool

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # Expected cost = 1e7 × 0.10 = 1,000,000
  expect_close(result$total_intervention_cost, 1e6)
})

# ---------------------------------------------------------------------------
# 4.8 PrEP cost capped at adult_pop (NOT high_risk_negative)
# ---------------------------------------------------------------------------
# WHAT: For prep_oral and prep_lenacapavir, cost =
#       min(intervention_value, adult_pop) × unit_cost.
#       This is DIFFERENT from infection impact, which still uses
#       high_risk_negative via the clip() in compute_prevention_adjustments.
#       Programs can distribute PrEP to anyone in the adult population, but
#       doses beyond the high-risk pool have no marginal FOI effect.
# WHY:  Decouples cost from effect so we can model real programs where
#       PrEP is offered to general-population adult walk-ins (real cost,
#       but those people would not have been infected anyway). Matches the
#       UI cap that prevents oral + lenacapavir summing above adult_pop.
# HOW:  high_risk_negative = 47,500 ; adult_pop = 600,000.
#       Set prep_oral = 100,000. 100,000 < adult_pop (600k), so the full
#       100,000 is costed. Expected cost = 100,000 × 80 = 8,000,000.
# ---------------------------------------------------------------------------
test_that("PrEP cost uses intervention_value capped at adult_pop", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_oral$unit_cost <- 80
  ig_new$prevention$interventions$prep_oral$efficacy  <- 0.99
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$prep_oral <- 1e5   # above high_risk_negative (47,500), below adult_pop (600,000)
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: 100,000 × 80 = 8,000,000 (full input costed because < adult_pop)
  expect_close(result$total_intervention_cost, 8e6)
})

# ---------------------------------------------------------------------------
# 4.8b PrEP cost capped at adult_pop when input exceeds it
# ---------------------------------------------------------------------------
# WHAT: Push prep_oral above adult_pop to verify the cap binds.
# HOW:  adult_pop = 600,000. Set prep_oral = 1,000,000.
#       Expected reached for costing = 600,000.
#       Expected cost = 600,000 × 80 = 48,000,000.
# ---------------------------------------------------------------------------
test_that("PrEP cost capped at adult_pop when input exceeds it", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_oral$unit_cost <- 80
  ig_new$prevention$interventions$prep_oral$efficacy  <- 0.99
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$prep_oral <- 1e6   # above adult_pop (600,000)
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: 600,000 × 80 = 48,000,000
  expect_close(result$total_intervention_cost, 4.8e7)
})

# ---------------------------------------------------------------------------
# 4.8c prep_lenacapavir uses same cap as prep_oral
# ---------------------------------------------------------------------------
# WHAT: Same behaviour for prep_lenacapavir — cost capped at adult_pop.
# HOW:  Set prep_lenacapavir = 100,000, unit_cost = 100.
#       100,000 < adult_pop (600,000), so the full input is costed.
#       Expected: 100,000 × 100 = 10,000,000.
# ---------------------------------------------------------------------------
test_that("prep_lenacapavir cost capped at adult_pop (same rule as prep_oral)", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_lenacapavir$unit_cost <- 100
  ig_new$prevention$interventions$prep_lenacapavir$efficacy  <- 1.00
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$prep_lenacapavir <- 1e5
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: 100,000 × 100 = 10,000,000
  expect_close(result$total_intervention_cost, 1e7)
})

# ---------------------------------------------------------------------------
# 4.9 VMMC cost capped at all uncircumcised males (HIV+ and HIV-)
# ---------------------------------------------------------------------------
# WHAT: VMMC eligible_pop = "uncircumcised_males_all" (HIV+ and HIV-).
#       Volume above this is capped before costing. The FOI side still
#       uses n_general_male_uncirc (HIV-negative only) — real programs
#       circumcise men regardless of HIV status, but only HIV-neg cases
#       generate infection-prevention benefit.
# WHY:  Matches the UI cap and real-program behaviour. The previous
#       behaviour (cap at HIV-neg only) silently truncated costs for any
#       VMMC volume between hiv_neg_uncirc and total_uncirc.
# HOW:  populations$uncircumcised_males_all
#         = total_population × (prop_pop_male/100) × (1 - circ_prevalence/100)
#         = 1,000,000 × 0.50 × 0.70 = 350,000
#       Set vmmc = 1,000,000; unit_cost = 50. Expected: 350,000 × 50 = 17,500,000.
# ---------------------------------------------------------------------------
test_that("VMMC cost capped at uncircumcised_males_all (HIV+ and HIV-)", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$vmmc$unit_cost <- 50
  ig_new$prevention$interventions$vmmc$efficacy  <- 0.60
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$vmmc <- 1e6  # well above uncircumcised_males_all (350,000)
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: 350,000 × 50 = 17,500,000
  expect_close(result$total_intervention_cost, 1.75e7)
})
# ---------------------------------------------------------------------------
# 4.10 Stratum partition cross-check: sum of n_strata = sexually_active_negative
# ---------------------------------------------------------------------------
# WHAT: At SAFR = 0.85, sum of n_high_risk + n_general_female +
#       n_general_male_uncirc + n_general_male_circ = sexually_active_negative.
# WHY: Sanity that test_02's identity (originally derived at SAFR=0.60) holds
#      at the live SAFR value too. The partition has no SAFR dependency in
#      its formula (it operates on whatever populations$sexually_active_negative
#      passes in), so this is really a denominator sanity check.
# HOW: At SAFR = 0.85, sexually_active_negative = 950,000 × 0.6 × 0.85 = 484,500.
# ---------------------------------------------------------------------------
test_that("strata partition sums to sexually_active_negative at live SAFR", {
  s <- prev_setup()
  total <- s$strata$n_high_risk + s$strata$n_general_female +
           s$strata$n_general_male_uncirc + s$strata$n_general_male_circ
  expect_close(total, s$strata$hiv_neg_active)
  expect_close(s$strata$hiv_neg_active, 484500)
})
