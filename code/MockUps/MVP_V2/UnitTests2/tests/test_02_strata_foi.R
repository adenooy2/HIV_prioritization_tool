# ============================================================================
# test_02_strata_foi.R
# ----------------------------------------------------------------------------
# Tests for the stratified Force-of-Infection module:
#
#   - define_strata_params()       : stratum-level fixed parameters
#   - partition_into_strata()      : split sexually_active_negative into 4 strata
#   - calibrate_beta()             : back-calculate β so model reproduces
#                                    observed new_infections_per_year
#   - compute_prevention_adjustments() : multiplicative protection by stratum
#   - estimate_new_infections_foi(): end-to-end FOI calculation
#
# The MOST IMPORTANT test in this file is the baseline roundtrip:
# given the same inputs used for calibration, the FOI model must reproduce
# context$new_infections_per_year to within a tight tolerance. If this
# breaks, every scenario comparison is meaningless because the baseline
# itself isn't reproducible.
#
# All tests override hiv_params$sexually_active_frac to a known value so
# the denominator is deterministic (the live value is unknown to the test
# author).
# ============================================================================

source("helpers.R")

# Shared override block: lock sexually_active_frac for the entire file so
# every fixture-based derivation is reproducible. Tests can layer additional
# overrides on top.
SAFR <- 0.60   # sexually_active_frac value used for derivations in this file

# ---------------------------------------------------------------------------
# 2.1 define_strata_params: uses context values when present
# ---------------------------------------------------------------------------
# WHAT: When context fields are non-NULL, define_strata_params returns them
#       directly (after % -> proportion conversion where applicable).
# WHY:  Mis-routing of context fields silently injects defaults from hiv_params,
#       breaking country-specific calibration.
# HOW:  Fixture: prop_high_risk = 0.05, rr_high = 4, prop_pop_male = 50 (%),
#       circ_prevalence = 30 (%).
#       Expected (after conversions): prop_male_general = 0.50,
#       circ_prevalence = 0.30, prop_high_risk = 0.05, rr_high = 4.
# ---------------------------------------------------------------------------
test_that("define_strata_params reflects context values with correct unit conversion", {
  ctx <- make_fixture_context()
  sp  <- define_strata_params(ctx)
  
  expect_close(sp$prop_high_risk,    0.05)
  expect_close(sp$prop_general,      0.95)
  expect_close(sp$rr_high,           4)
  expect_close(sp$prop_male_general, 0.50)
  expect_close(sp$circ_prevalence,   0.30)
})

# ---------------------------------------------------------------------------
# 2.2 partition_into_strata: splits sexually_active_negative into 4 strata
# ---------------------------------------------------------------------------
# WHAT: Strata-level partition. Note this is on sexually_active_negative,
#       NOT hiv_negative — that's tested separately in test_01.
# WHY:  These N's feed every β in calibrate_beta and every infection count
#       in estimate_new_infections_foi.
# HOW:  Override sexually_active_frac = 0.60 for determinism.
#         hiv_negative              = 950,000
#         sexually_active_negative  = 950,000 × (1 - 0.40) × 0.60 = 342,000
#       Strata params: prop_hr = 0.05, prop_male_general = 0.50, circ_prev = 0.30.
#         n_high_risk           = 342,000 × 0.05                 = 17,100
#         n_general             = 342,000 × 0.95                 = 324,900
#         n_general_male_uncirc = 324,900 × 0.50 × (1 - 0.30)    = 113,715
#         n_general_male_circ   = 324,900 × 0.50 × 0.30          =  48,735
#         n_general_female      = 324,900 × (1 - 0.50)           = 162,450
#       Sum of 4 strata = 17,100 + 113,715 + 48,735 + 162,450    = 342,000 ✓
#
#       n_unsuppressed = unsuppressed + diagnosed_not_on_art + undiagnosed
#                     = 3,600 + 9,000 + 5,000 = 17,600
# ---------------------------------------------------------------------------
test_that("partition_into_strata splits sexually_active_negative correctly", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  expect_close(strata$hiv_neg_active,        342000)
  expect_close(strata$n_high_risk,           17100)
  expect_close(strata$n_general,             324900)
  expect_close(strata$n_general_male_uncirc, 113715)
  expect_close(strata$n_general_male_circ,   48735)
  expect_close(strata$n_general_female,      162450)
  # Sanity: 4 strata cover all of sexually_active_negative
  expect_close(strata$n_high_risk +
                 strata$n_general_male_uncirc +
                 strata$n_general_male_circ +
                 strata$n_general_female,
               strata$hiv_neg_active)
  
  expect_close(strata$n_unsuppressed, 17600)
})

# ---------------------------------------------------------------------------
# 2.3 BASELINE ROUNDTRIP (no baseline_prev_adj passed)
# ---------------------------------------------------------------------------
# WHAT: When calibrate_beta is called WITHOUT baseline_prev_adj, β absorbs
#       prevention implicitly. Feeding the calibrated β back into the FOI
#       model with zero prevention should reproduce observed infections
#       exactly (modulo round()).
# WHY:  This is the core invariant — the model must reproduce its own input.
#       A 1% tolerance is allowed because estimate_new_infections_foi calls
#       round() on the total before returning.
# HOW:  Standard fixture (5,000 observed infections), zero interventions.
#       Expected: foi_result$new_infections ≈ 5,000 (within rounding).
# ---------------------------------------------------------------------------
test_that("FOI roundtrip (no baseline_prev_adj) reproduces observed infections", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx      <- make_fixture_context()
  pops     <- calculate_populations(ctx)
  interv   <- make_fixture_interventions()  # all zeros
  
  result <- estimate_new_infections_foi(
    context                = ctx,
    populations            = pops,
    scenario_interventions = interv,
    suppression_delta      = 0,
    baseline_interventions = NULL   # implicit-prevention β path
  )
  
  expect_within_pct(result$new_infections, 5000, pct = 1)
})

# ---------------------------------------------------------------------------
# 2.4 BASELINE ROUNDTRIP with baseline_prev_adj (biological-β path)
# ---------------------------------------------------------------------------
# WHAT: When baseline_interventions ARE passed AND scenario_interventions
#       match them, the result should still reproduce observed infections.
# WHY:  This tests the "biological β" path, where prevention is applied
#       symmetrically at calibration time and FOI eval time.
# HOW:  Use the same intervention set for both baseline and scenario.
#       Use modest non-zero values to exercise the prevention branches.
#       Expected: result ≈ 5,000.
# ---------------------------------------------------------------------------
test_that("FOI roundtrip with matching baseline & scenario reproduces observed", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx    <- make_fixture_context()
  pops   <- calculate_populations(ctx)
  
  # Modest baseline prevention coverage
  shared_interv <- make_fixture_interventions(
    prep_oral = 5000,
    condoms   = 100000,
    vmmc      = 1000
  )
  
  result <- estimate_new_infections_foi(
    context                = ctx,
    populations            = pops,
    scenario_interventions = shared_interv,
    suppression_delta      = 0,
    baseline_interventions = shared_interv   # symmetric
  )
  
  expect_within_pct(result$new_infections, 5000, pct = 1)
})

# ---------------------------------------------------------------------------
# 2.5 compute_prevention_adjustments: PrEP-only stack with k-fold allocation
# ---------------------------------------------------------------------------
# WHAT: PrEP is now allocated across high-risk AND general strata with a
#       k-fold per-capita advantage to high-risk (default k = 3, configurable
#       via hiv_params$prep_high_risk_fold). With only prep_oral > 0:
#         protection_high     = c_H × eff_prep_oral
#         protection_gen_X    = c_G × eff_prep_oral
#       where c_H = k * c_G and c_H * H + c_G * G = total_units (Case A,
#       no saturation).
# WHY:  Isolates the new allocation math to a single intervention so we can
#       pin down both high-risk AND general per-capita coverages.
# HOW:  SAFR = 0.60. From test 2.2: n_high_risk (H) = 17,100;
#         sexually_active_negative = 342,000; n_general (G) = 324,900.
#       k = 3 (default). Threshold for no-saturation: H + G/k
#         = 17,100 + 324,900/3 = 125,400.
#       prep_oral = 1,710. Since 1,710 < 125,400, Case A applies:
#         c_G = 1,710 / (3 * 17,100 + 324,900) = 1,710 / 376,200 = 0.004545454...
#         c_H = 3 * c_G                                          = 0.013636363...
#       eff_prep_oral = 0.99.
#         protection_high     = 0.013636363 * 0.99 = 0.013500
#         protection_gen_X    = 0.004545454 * 0.99 = 0.004500
#       All gen strata receive the same c_G uniformly (matches condom logic),
#       so protection_gen_female, _male_unc, _male_circ are equal.
#       condoms = 0 ⇒ condom term = 1 across all strata.
#       vmmc = 0 ⇒ vmmc_coverage_frac = 0.
# ---------------------------------------------------------------------------
test_that("compute_prevention_adjustments isolates PrEP-only correctly (k-fold)", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  interv  <- make_fixture_interventions(prep_oral = 1710)
  adj     <- compute_prevention_adjustments(interv, strata, pops, sp)
  
  expect_close(adj$protection_high,          0.013500, tolerance = 1e-6)
  expect_close(adj$protection_gen_female,    0.004500, tolerance = 1e-6)
  expect_close(adj$protection_gen_male_unc,  0.004500, tolerance = 1e-6)
  expect_close(adj$protection_gen_male_circ, 0.004500, tolerance = 1e-6)
  expect_close(adj$vmmc_coverage_frac,       0)
})

# ---------------------------------------------------------------------------
# 2.6 compute_prevention_adjustments: condom demand-weighting
# ---------------------------------------------------------------------------
# WHAT: Condoms are allocated by total sex acts per group, not headcount.
#       Per-person coverage = total_condoms × use_rate / total_acts. Since the
#       use rates differ (high = 0.75, gen = 0.55), per-person coverages
#       must differ even though both groups draw from the same total_acts
#       denominator.
# WHY:  This is the "demand-weighted" allocation rule per lines 1014-1036.
#       If incorrectly proportional to headcount, the high-risk stratum
#       gets too few condoms.
# HOW:  acts_high_total = 17,100 × 100 = 1,710,000
#       acts_gen_total  = 324,900 × 50  = 16,245,000
#       total_acts      = 17,955,000
#       total_condoms   = 1,795,500 -> chosen so total_condoms/total_acts = 0.1
#       condom_cov_high = 1,795,500 × 0.75 / 17,955,000 = 0.075
#       condom_cov_gen  = 1,795,500 × 0.55 / 17,955,000 = 0.055
#
#       With ONLY condoms scaled (PrEP = VMMC = 0):
#         protection_high       = condom_cov_high × eff_condom = 0.075 × 0.80 = 0.06
#         protection_gen_female = condom_cov_gen  × eff_condom = 0.055 × 0.80 = 0.044
# ---------------------------------------------------------------------------
test_that("condom allocation is demand-weighted (acts-based), not headcount-based", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  interv  <- make_fixture_interventions(condoms = 1795500)
  adj     <- compute_prevention_adjustments(interv, strata, pops, sp)
  
  # Verify the protection levels reflect different per-person coverages
  expect_close(adj$protection_high,       0.06,  tolerance = 1e-9)
  expect_close(adj$protection_gen_female, 0.044, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# 2.7 VMMC: shifts uncirc -> circ; coverage frac is bounded by uncirc pool
# ---------------------------------------------------------------------------
# WHAT: vmmc_coverage_frac = min(vmmc, n_general_male_uncirc) / n_general_male_uncirc.
#       Note this denominator is the GENERAL uncirc male pool, NOT the
#       overall populations$uncircumcised_males which includes KP men.
# WHY:  VMMC coverage formula must use the right denominator. If it used
#       populations$uncircumcised_males instead, the same VMMC volume
#       would understate coverage and the strata shift would be too small.
# HOW:  n_general_male_uncirc = 113,715 (from test 2.2).
#       Give vmmc = 11,371.5 (= 10% of pool).
#       Expected vmmc_coverage_frac ≈ 0.10.
#       Cap test: vmmc = 200,000 should be capped at 113,715 -> frac = 1.0.
# ---------------------------------------------------------------------------
test_that("VMMC coverage fraction uses the general uncirc-male pool as denominator", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  # 10% coverage
  interv  <- make_fixture_interventions(vmmc = 11371.5)
  adj     <- compute_prevention_adjustments(interv, strata, pops, sp)
  expect_close(adj$vmmc_coverage_frac, 0.10, tolerance = 1e-6)
  
  # Cap test: scale beyond pool size
  interv2 <- make_fixture_interventions(vmmc = 200000)
  adj2    <- compute_prevention_adjustments(interv2, strata, pops, sp)
  expect_close(adj2$vmmc_coverage_frac, 1.0)
})

# ---------------------------------------------------------------------------
# 2.8 Scaling PrEP up reduces infections (monotonicity)
# ---------------------------------------------------------------------------
# WHAT: Holding everything else fixed, more PrEP -> fewer infections.
# WHY:  This is a soft direction check that catches sign-flip bugs in
#       protection_high or in the FOI evaluation. Stronger than the test
#       suite's other "approximately X" assertions because it's a strict
#       inequality.
# HOW:  Run FOI at PrEP = 0, 5000, 20000 with everything else zero.
#       Expected: infections strictly decreasing.
# ---------------------------------------------------------------------------
test_that("scaling PrEP up monotonically reduces infections", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx    <- make_fixture_context()
  pops   <- calculate_populations(ctx)
  
  # Same baseline so calibration is constant across runs
  baseline <- make_fixture_interventions()
  
  inf_low <- estimate_new_infections_foi(
    ctx, pops,
    scenario_interventions = make_fixture_interventions(prep_oral = 0),
    baseline_interventions = baseline
  )$new_infections
  
  inf_mid <- estimate_new_infections_foi(
    ctx, pops,
    scenario_interventions = make_fixture_interventions(prep_oral = 5000),
    baseline_interventions = baseline
  )$new_infections
  
  inf_high <- estimate_new_infections_foi(
    ctx, pops,
    scenario_interventions = make_fixture_interventions(prep_oral = 20000),
    baseline_interventions = baseline
  )$new_infections
  
  expect_gt(inf_low,  inf_mid)
  expect_gt(inf_mid,  inf_high)
})

# ---------------------------------------------------------------------------
# 2.9 Positive suppression_delta reduces infections
# ---------------------------------------------------------------------------
# WHAT: suppression_delta reduces n_unsuppressed_scenario, which reduces
#       infectious_pressure_scenario, which lowers infections proportionally
#       across all strata.
# WHY:  This is the model's treatment-as-prevention channel. Sign error here
#       means scenarios that boost suppression would appear to INCREASE
#       infections — a serious model defect.
# HOW:  Two FOI runs identical except suppression_delta = 0 vs 1000.
#       n_unsuppressed = 17,600 (from test 2.2).
#       infectious_pressure_baseline = 17,600 / 1,000,000 = 0.0176
#       infectious_pressure_with_delta = (17,600 - 1,000) / 1,000,000 = 0.0166
#       Ratio = 0.0166 / 0.0176 ≈ 0.9432
#       So infections should scale down by ~5.7%.
#       Expected: result_with_delta / result_no_delta ≈ 0.94 (1% tolerance).
# ---------------------------------------------------------------------------
test_that("positive suppression_delta reduces infections proportionally", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  interv  <- make_fixture_interventions()
  
  inf_base <- estimate_new_infections_foi(
    ctx, pops, interv, suppression_delta = 0
  )$new_infections
  
  inf_supp <- estimate_new_infections_foi(
    ctx, pops, interv, suppression_delta = 1000
  )$new_infections
  
  expect_lt(inf_supp, inf_base)
  
  # Derived ratio:
  expected_ratio <- (17600 - 1000) / 17600   # ≈ 0.9432
  observed_ratio <- inf_supp / inf_base
  expect_within_pct(observed_ratio, expected_ratio, pct = 1)
})

# ---------------------------------------------------------------------------
# 2.10 Zero observed infections -> all β = 0
# ---------------------------------------------------------------------------
# WHAT: When new_infections_per_year = 0, every β should be 0 (no transmission
#       to back-calculate). Result: estimate_new_infections_foi returns 0.
# WHY:  Edge case for low-prevalence settings or test fixtures. Division-by-
#       zero protection in safe_beta() is supposed to handle this.
# HOW:  Override new_infections_per_year = 0.
# ---------------------------------------------------------------------------
test_that("zero observed infections produces zero modelled infections", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx <- make_fixture_context(new_infections_per_year = 0)
  pops <- calculate_populations(ctx)
  
  result <- estimate_new_infections_foi(
    ctx, pops,
    scenario_interventions = make_fixture_interventions(),
    suppression_delta = 0
  )
  
  expect_close(result$new_infections, 0)
})

# ---------------------------------------------------------------------------
# 2.11 by_stratum infections sum to total
# ---------------------------------------------------------------------------
# WHAT: The four by_stratum values should sum to new_infections (modulo rounding
#       differences from individual round() calls).
# WHY:  By-stratum decomposition is used for diagnostic / display purposes.
#       If it drifts from the total, displayed shares are misleading.
# HOW:  Standard fixture. Tolerance = 4 (one unit of round() error per stratum).
# ---------------------------------------------------------------------------
test_that("by_stratum infections sum to total (within rounding)", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  interv  <- make_fixture_interventions()
  
  res <- estimate_new_infections_foi(ctx, pops, interv,
                                     baseline_interventions = interv)
  
  sum_strata <- with(res$by_stratum,
                     high_risk + gen_female + gen_male_uncirc + gen_male_circ)
  expect_lte(abs(sum_strata - res$new_infections), 4)
})