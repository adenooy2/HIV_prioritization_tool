# ============================================================================
# test_04_prevention.R
# ----------------------------------------------------------------------------
# Tests for the prevention cost loop and prevention-stratum interactions that
# weren't fully covered in test_02_strata_foi.R:
#
#   - PEP allocation: each general sub-stratum receives `pep × 0.5 / n_substratum`.
#     With three sub-strata that adds up to 150% of supply allocated — noted in
#     test 4.5 but locking current behaviour.
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
# 4.1 PEP-only protection (general female)
# ---------------------------------------------------------------------------
# WHAT: With PEP = 10,000, n_general_female = 162,450 (from test 2.2 derivation
#       at SAFR = 0.60; here SAFR = 0.85 so it scales). Let me re-derive at SAFR=0.85.
#       hiv_negative = 950,000; sexually_active_negative = 950,000 × 0.6 × 0.85 = 484,500
#       n_high_risk = 484,500 × 0.05 = 24,225
#       n_general   = 484,500 × 0.95 = 460,275
#       n_general_female = 460,275 × 0.50 = 230,137.5
#       n_general_male_uncirc = 460,275 × 0.50 × 0.70 = 161,096.25
#       n_general_male_circ   = 460,275 × 0.50 × 0.30 =  69,041.25
#
#       pep_cov_gen_f = clip(10,000 × 0.5 / 230,137.5) = clip(0.021726) ≈ 0.02173
#       eff_pep = 0.80 (default).
#       Only PEP (no condoms):
#         residual_gen_female = (1 - 0 × 0.80) × (1 - 0.02173 × 0.80)
#                             = 1.0 × 0.98262
#                             = 0.98262
#         protection_gen_female = 1 - 0.98262 = 0.01738
# WHY: Locks the PEP allocation formula for general_female. Note the 0.5×
#      factor and that PEP supply is divided by the stratum size (so larger
#      strata get lower per-person coverage).
# ---------------------------------------------------------------------------
test_that("PEP-only allocation in general female matches formula", {
  s   <- prev_setup()
  interv <- make_fixture_interventions(pep = 10000)
  adj <- compute_prevention_adjustments(interv, s$strata, s$pops, s$sp)

  expected <- 1 - (1 - (10000 * 0.5 / 230137.5) * 0.80)
  expect_close(adj$protection_gen_female, expected, tolerance = 1e-6)
  # Sanity: ~1.7% protection
  expect_close(adj$protection_gen_female, 0.01738, tolerance = 1e-4)
})

# ---------------------------------------------------------------------------
# 4.2 PEP-only in high-risk stratum = 0 (high-risk gets no PEP allocation)
# ---------------------------------------------------------------------------
# WHAT: Lines 1037-1044: protection_high stack includes prep_oral, prep_len,
#       and condoms — but NOT PEP. So PEP scaling alone leaves the high-risk
#       stratum unchanged.
# WHY: PEP intervention is conceptually for occupational/sexual exposure in
#      the general population. The model's allocation rule explicitly excludes
#      the high-risk stratum from receiving PEP. Worth pinning down because
#      it's a non-obvious modelling choice.
# HOW: PEP = 100,000 (large value); protection_high should still be 0.
# ---------------------------------------------------------------------------
test_that("PEP-only does not affect high-risk stratum protection", {
  s   <- prev_setup()
  interv <- make_fixture_interventions(pep = 100000)
  adj <- compute_prevention_adjustments(interv, s$strata, s$pops, s$sp)

  expect_close(adj$protection_high, 0)
})

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
#       protection_* term. The condom/PEP terms for circ_male are independent
#       of vmmc_coverage_frac.
# WHY: Verifies the structural separation between coverage-frac (used for
#      stratum-shift in main FOI) and protection (used for residual within
#      stratum). A bug that conflates them would over-credit VMMC by both
#      shifting and protecting the same men.
# HOW: vmmc = 50,000; condoms = 0; pep = 0.
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
# 4.5 PEP supply allocation behaviour: sum across general sub-strata > supply
# ---------------------------------------------------------------------------
# WHAT: Each of the 3 general sub-strata (female, male_uncirc, male_circ)
#       receives `pep × 0.5 / n_substratum`. Multiplying through by stratum
#       sizes: total people protected by PEP across general strata =
#       n_gen_female × pep_cov_gen_f + n_gen_male_unc × pep_cov_gen_mu +
#       n_gen_male_circ × pep_cov_gen_mc = pep × 0.5 × 3 = 1.5 × pep.
#       i.e. ~150% of supply gets "allocated" across the three sub-strata.
# WHY: Lock current behaviour. This is worth a separate discussion — it may
#      be an unintended over-allocation, or intentional if the 0.5 factor
#      represents per-act probability of needing PEP rather than per-person.
#      Either way the test fails if the formula changes, which forces a
#      conversation.
# HOW: pep = 10,000. With clip() preventing >100% coverage, the actual
#      allocation is min(pep × 0.5 / n_substratum, 1) × n_substratum.
#      For 10,000 PEP against ~70k circ males:
#        pep_cov_gen_mc = clip(10,000 × 0.5 / 69,041.25) = 0.07242
#        people protected in circ stratum = 5,000
#      Similarly 5,000 in uncirc and 5,000 in female. Sum = 15,000.
#      Note: this is "people protected by PEP coverage frac", NOT "doses
#      distributed" — but verifies the structural 1.5× allocation effect.
# ---------------------------------------------------------------------------
test_that("PEP allocation across 3 general sub-strata sums to 1.5x supply", {
  s   <- prev_setup()
  pep_supply <- 10000
  interv <- make_fixture_interventions(pep = pep_supply, eff_pep = 1.0)  # eff=1 simplifies
  adj <- compute_prevention_adjustments(interv, s$strata, s$pops, s$sp)

  # Back out pep_cov per stratum from protection (with eff_pep = 1.0 and condom = 0):
  #   protection_gen_X = 1 - (1 - 0)(1 - pep_cov_X × 1.0) = pep_cov_X
  # So protection_X × n_X = people protected in stratum X.
  protected_female   <- adj$protection_gen_female    * s$strata$n_general_female
  protected_male_unc <- adj$protection_gen_male_unc  * s$strata$n_general_male_uncirc
  protected_male_circ<- adj$protection_gen_male_circ * s$strata$n_general_male_circ

  total_protected <- protected_female + protected_male_unc + protected_male_circ

  # Expected = 0.5 × pep × 3 = 1.5 × supply = 15,000
  expect_close(total_protected, 1.5 * pep_supply, tolerance = 1)
})

# ---------------------------------------------------------------------------
# 4.6 PEP coverage clips at 1.0 (cannot exceed 100%)
# ---------------------------------------------------------------------------
# WHAT: clip() caps pep_cov_X at 1. So if pep_supply × 0.5 > n_substratum,
#       coverage saturates and over-supply is wasted (in the model's
#       accounting; intervention cost still accrues).
# WHY: Edge-case test for very large PEP allocations relative to small strata.
# HOW: Build a small-population fixture so n_general_female × 2 < pep × 0.5.
#      Use the default fixture but pep = 10,000,000.
#      pep × 0.5 = 5M; n_general_female = 230,138. Cov saturates at 1.0.
#      With eff_pep = 1.0, protection_gen_female = 1.0.
# ---------------------------------------------------------------------------
test_that("PEP coverage clips at 1.0", {
  s <- prev_setup()
  interv <- make_fixture_interventions(pep = 1e7, eff_pep = 1.0)
  adj <- compute_prevention_adjustments(interv, s$strata, s$pops, s$sp)

  expect_close(adj$protection_gen_female,    1.0)
  expect_close(adj$protection_gen_male_unc,  1.0)
  expect_close(adj$protection_gen_male_circ, 1.0)
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
# 4.8 PrEP cost uses number_reached (capped at eligible_pop)
# ---------------------------------------------------------------------------
# WHAT: For prep_oral (eligible_pop = "high_risk_negative"), cost =
#       min(intervention_value, eligible) × unit_cost.
# WHY: Contrast with condoms — only condoms gets the raw-value special case.
# HOW: high_risk_negative (populations level, uses hiv_negative not sexually_active_negative):
#      = 950,000 × 0.05 = 47,500.
#      Set prep_oral = 100,000 (above eligible). Expected reached = 47,500.
#      Override unit_cost = 80. Expected cost = 47,500 × 80 = 3,800,000.
# ---------------------------------------------------------------------------
test_that("PrEP cost uses number_reached, capped at eligible_pop", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.59,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.9,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 0.93,
                       pmtct_cascade_supp_discount = 0.9))

  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_oral$unit_cost <- 80
  ig_new$prevention$interventions$prep_oral$efficacy  <- 0.99
  with_intervention_groups(list(prevention = ig_new$prevention))

  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$prep_oral <- 1e5   # well above high_risk_negative (47,500)

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # Expected: 47,500 × 80 = 3,800,000
  expect_close(result$total_intervention_cost, 3.8e6)
})

# ---------------------------------------------------------------------------
# 4.9 VMMC cost uses number_reached (capped at uncircumcised_males)
# ---------------------------------------------------------------------------
# WHAT: VMMC eligible_pop = "uncircumcised_males". Volume above this is
#       capped before costing.
# WHY: VMMC supply >> pool is realistic for late-stage VMMC programmes; cost
#      must not balloon past pool exhaustion.
# HOW: populations$uncircumcised_males = hiv_negative × 0.50 × (1 - 0.30) = 332,500
#      (note this includes KP males — uses full hiv_negative, not sex active).
#      Set vmmc = 1,000,000; unit_cost = 50. Expected: 332,500 × 50 = 16,625,000.
# ---------------------------------------------------------------------------
test_that("VMMC cost capped at uncircumcised_males pool size", {
  with_hiv_params(list(sexually_active_frac = SAFR,
                       prop_retest_default = 0.59,
                       testing_reengagement_cap_frac = 0.45,
                       testing_art_init_supp = 0.9,
                       new_diagnoses_cap_prop = 0.95,
                       average_linkage_cap = 0.93,
                       pmtct_cascade_supp_discount = 0.9))

  ig_new <- intervention_groups
  ig_new$prevention$interventions$vmmc$unit_cost <- 50
  ig_new$prevention$interventions$vmmc$efficacy  <- 0.60
  with_intervention_groups(list(prevention = ig_new$prevention))

  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$vmmc <- 1e6  # well above uncircumcised_males (332,500)

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # Expected: 332,500 × 50 = 16,625,000
  expect_close(result$total_intervention_cost, 1.6625e7)
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
