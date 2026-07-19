# ============================================================================
# test_04_prevention.R
# ----------------------------------------------------------------------------
# Tests for the prevention cost loop and prevention-stratum interactions that
# weren't fully covered in test_02_strata_foi.R:
#
#   - - PrEP + condom stacking in the high-risk stratum (PrEP products additive over disjoint regimens; condoms multiplicative on top)
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
  # Pin sexually_active_frac AND prep_high_risk_fold so derivations are
  # deterministic regardless of the live hiv_params Excel values (the live
  # fold value is unknown to the test author — same rationale as SAFR).
  with_hiv_params(list(sexually_active_frac = SAFR, prep_high_risk_fold = 3),
                  envir = parent.frame())
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
# Pins sexually_active_frac (drives the sexually_active_negative cap) and
# prep_high_risk_fold (drives PrEP allocation) so asserted values are
# deterministic regardless of the live hiv_params Excel sheet.
LIVE_PARAMS_PREVENTION_04 <- list(sexually_active_frac = SAFR, prep_high_risk_fold = 3)

# Local copy of test_09_costs.R::base_ctx — each test file runs in its own
# testthat context, so helpers defined in other test files are not in scope.
base_ctx <- function() {
  make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                       yield_multipliers = list())
}

# ---------------------------------------------------------------------------
#4.3 PrEP + condom stacking in high-risk (additive PrEP, direct targeting)
# ---------------------------------------------------------------------------
# WHAT: protection_fsw = 1 - (1 - prep_prot_fsw) * (1 - condom_cov_high*eff_condom),
#       where prep_prot_fsw = clip(cov_oral*eff_oral + cov_len*eff_len).
#       Oral and lenacapavir are mutually exclusive regimens, so PrEP protection
#       is ADDITIVE across them; condoms apply to the same people and so stay
#       multiplicative. No k-fold allocation -- cov_oral = prep_oral_fsw / n_fsw
#       directly. This test sets lenacapavir = 0, so it pins the condom
#       interaction only; the additive PrEP term is pinned by test 2.5c.
# HOW:  n_fsw = 484,500 * 0.025 = 12,112.5 (SAFR=0.85, prev_setup()).
#       prep_oral_fsw = 1,211.25 (10% of n_fsw) -> cov_oral = 0.10.
#       prep_prot_fsw = clip(0.10 * 0.99 + 0 * eff_len) = 0.099.
#       Condoms: unchanged from the old derivation (n_fsw+n_msm = 24,225,
#       same combined "high" pool as before) -> condom_cov_high = 0.20.
#         residual_fsw = (1-0.099) * (1-0.20*0.80)
#                      = 0.901 * 0.84 = 0.75684
#         protection_fsw = 1 - 0.75684 = 0.24316
test_that("PrEP + condom stack multiplicatively per group (direct targeting)", {
  s   <- prev_setup()
  interv <- make_fixture_interventions(
    prep_oral_fsw         = 1211.25,
    condoms               = 8464759,
    eff_prep_oral_fsw     = 0.99,
    eff_condom            = 0.80,
    acts_per_year_high    = 500, acts_per_year_gen = 50,
    condom_use_rate_high  = 0.83, condom_use_rate_gen = 0.48
  )
  adj <- compute_prevention_adjustments(interv, s$strata, s$pops, s$sp)
  expect_close(adj$protection_fsw, 0.24316, tolerance = 1e-4)
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
  expect_close(adj$protection_fsw,           0)
  expect_close(adj$protection_msm,           0)
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
# 4.8 PrEP cost capped at sexually_active_negative
# ---------------------------------------------------------------------------
## ---- 4.8: NEW (below cap) ----
test_that("PrEP cost uses intervention_value capped at n_fsw (below cap)", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_oral_fsw$unit_cost <- 80
  ig_new$prevention$interventions$prep_oral_fsw$person_years_on_prep <- 1  # pin 12 mo -> prep_oral_cost_frac = 1.000, so expected costs are unchanged
  ig_new$prevention$interventions$prep_oral_fsw$efficacy  <- 0.99
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  interv <- zero_interventions()
  interv$prep_oral_fsw <- 5000   # below n_fsw (12,112.5)
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  # Expected: 5,000 x 80 = 400,000 (full input costed, below cap)
  expect_close(result$total_intervention_cost, 4e5)
})
# ---------------------------------------------------------------------------
# 4.8b PrEP cost capped at sexually_active_negative when input exceeds it
# ---------------------------------------------------------------------------
test_that("PrEP cost capped at n_fsw when input exceeds it", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_oral_fsw$unit_cost <- 80
  ig_new$prevention$interventions$prep_oral_fsw$person_years_on_prep <- 1  # pin 12 mo -> prep_oral_cost_frac = 1.000, so expected costs are unchanged
  ig_new$prevention$interventions$prep_oral_fsw$efficacy  <- 0.99
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  interv <- zero_interventions()
  interv$prep_oral_fsw <- 50000   # above n_fsw (12,112.5)
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  # Expected: 12,112.5 x 80 = 969,000
  expect_close(result$total_intervention_cost, 969000)
})

# ---------------------------------------------------------------------------
# 4.8c prep_lenacapavir uses same cap as prep_oral
# ---- 4.8c: NEW (lenacapavir, same rule, below cap) ----
test_that("prep_lenacapavir cost capped at n_fsw (same rule as prep_oral)", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_lenacapavir_fsw$unit_cost <- 100
  ig_new$prevention$interventions$prep_lenacapavir_fsw$efficacy  <- 1.00
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  interv <- zero_interventions()
  interv$prep_lenacapavir_fsw <- 5000   # below n_fsw
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  # Expected: 5,000 x 100 = 500,000
  expect_close(result$total_intervention_cost, 5e5)
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
  total <- s$strata$n_fsw + s$strata$n_msm + s$strata$n_agyw +
    s$strata$n_general_female +
    s$strata$n_general_male_uncirc + s$strata$n_general_male_circ
  expect_close(total, s$strata$hiv_neg_active)
  expect_close(s$strata$hiv_neg_active, 484500)
})