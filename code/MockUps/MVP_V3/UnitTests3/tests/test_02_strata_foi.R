# ============================================================================
# test_02_strata_foi.R
# ----------------------------------------------------------------------------
# Tests for the stratified Force-of-Infection module:
#
#   - define_strata_params()       : stratum-level fixed parameters
#   - partition_into_strata()      : split sexually_active_negative into 4 strata
#   - calibrate_beta()             : back-calculate β so model reproduces
#                                    observed new_infections_per_year
#   - compute_prevention_adjustments() : protection by stratum (PrEP products
#                                    additive/disjoint; condoms multiplicative)
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
  
  expect_close(sp$prop_fsw,          0.025)
  expect_close(sp$prop_msm,          0.025)
  expect_close(sp$prop_agyw,         0)
  expect_close(sp$prop_general,      0.95)
  expect_close(sp$rr_fsw,            4)
  expect_close(sp$rr_msm,            4)
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
# ---- 2.2: NEW ----
# HOW: prop_fsw = prop_msm = 0.025 (helpers.R default), prop_agyw = 0.
#      n_fsw = n_msm = 342,000 × 0.025               = 8,550 each
#      n_general             = 342,000 × 0.95         = 324,900  (unchanged)
#      n_general_male_uncirc = 324,900 × 0.50 × 0.70   = 113,715  (unchanged)
#      n_general_male_circ   = 324,900 × 0.50 × 0.30   =  48,735  (unchanged)
#      n_general_female_all  = 324,900 × 0.50           = 162,450
#      n_agyw = 162,450 × 0 = 0; n_general_female = 162,450 (unchanged)
#      Sum of 6 strata = 8550+8550+113715+48735+162450+0 = 342,000 ✓
test_that("partition_into_strata splits sexually_active_negative correctly", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  expect_close(strata$hiv_neg_active,        342000)
  expect_close(strata$n_fsw,                 8550)
  expect_close(strata$n_msm,                 8550)
  expect_close(strata$n_agyw,                0)
  expect_close(strata$n_general_male_uncirc, 113715)
  expect_close(strata$n_general_male_circ,   48735)
  expect_close(strata$n_general_female,      162450)
  expect_close(strata$n_fsw + strata$n_msm + strata$n_agyw +
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
  with_hiv_params(list(sexually_active_frac = SAFR, prep_high_risk_fold = 3))
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

# ---- 2.5: NEW — direct group targeting, no allocation heuristic ----
#   cov_fsw_oral  = 855/8,550   = 0.10 -> protection_fsw  = 1-(1-0.10*0.3) = 0.03
#   cov_msm_oral  = 1,710/8,550 = 0.20 -> protection_msm  = 1-(1-0.20*0.6) = 0.12
#   cov_agyw_oral = 3,249/32,490= 0.10 -> protection_agyw = 1-(1-0.10*0.4) = 0.04

#       protection_gen_female should remain 0 here: this test allocates no
#       general PrEP (prep_oral_general defaults to 0) and no condoms, so the
#       general strata get no protection. General PrEP is exercised in 2.5b.
test_that("compute_prevention_adjustments targets FSW/MSM/AGYW independently (direct coverage)", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context(prop_agyw = 0.20)
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  interv  <- make_fixture_interventions(
    prep_oral_fsw = 855, prep_oral_msm = 1710, prep_oral_agyw = 3249
  )
  adj <- compute_prevention_adjustments(interv, strata, pops, sp)
  
  expect_close(adj$protection_fsw,        0.03, tolerance = 1e-6)
  expect_close(adj$protection_msm,        0.12, tolerance = 1e-6)
  expect_close(adj$protection_agyw,       0.04, tolerance = 1e-6)
  expect_close(adj$protection_gen_female, 0,    tolerance = 1e-9)
  expect_close(adj$vmmc_coverage_frac,    0)
})

# ---- 2.5c: NEW — oral + lenacapavir are ADDITIVE within a group ----
# WHAT: A group split across BOTH PrEP products protects additively (disjoint
#       regimens), not multiplicatively. This is the case no other test
#       exercises — all others set exactly one product per group.
# WHY:  Oral and lenacapavir are mutually exclusive (a person is on one or the
#       other), so coverages sum and protection adds. A multiplicative form
#       understates protection by the cross term.
# HOW:  Fixture n_fsw = 8,550 (prop_agyw = 0.20 context, as in 2.5).
#       prep_oral_fsw        = 855   -> cov_fsw_oral = 855 /8550 = 0.10
#       prep_lenacapavir_fsw = 1,710 -> cov_fsw_len  = 1710/8550 = 0.20
#       cov_oral + cov_len = 0.30 (<= 1, within cap).
#       eff_prep_oral_fsw = 0.3, eff_prep_len_fsw = 0.50 (fixture).
#       protection_fsw = clip(0.10*0.3 + 0.20*0.50) = 0.03 + 0.10 = 0.13
#       (Old multiplicative form gave 1-(1-0.03)(1-0.10) = 0.127; additive is
#        higher by the 0.003 cross term.) No condoms -> PrEP-only.
test_that("oral + lenacapavir protect additively within a group (disjoint regimens)", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context(prop_agyw = 0.20)
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  interv  <- make_fixture_interventions(
    prep_oral_fsw = 855, prep_lenacapavir_fsw = 1710
  )
  adj <- compute_prevention_adjustments(interv, strata, pops, sp)
  
  expect_close(adj$protection_fsw, 0.13, tolerance = 1e-6)
})

# ---------------------------------------------------------------------------
# 2.5b compute_prevention_adjustments: GENERAL PrEP split across general strata
# ---------------------------------------------------------------------------
# WHAT: A single general PrEP total is split female/male by
#       prep_general_prop_female (fixture default 0.506), then the male share
#       is sub-split uncirc/circ by circ_prevalence. Each general stratum's
#       protection then stacks that PrEP coverage with condoms (0 here).
#       KP strata (FSW/MSM/AGYW) must be UNTOUCHED by general PrEP.
# HOW:  prop_agyw = 0 (fixture default) so general_female holds all general
#       women. From test 2.2 partition (SAFR):
#         n_general_female      = 162,450
#         n_general_male_uncirc = 113,715   (circ_prevalence = 0.30)
#         n_general_male_circ   =  48,735
#       eff_prep_oral_general = 0.7 (fixture). Enter prep_oral_general = 16,245:
#         female slice = 16,245 * 0.506 = 8,219.97
#           cov_genf = 8,219.97 / 162,450 = 0.050600
#           protection_gen_female = 1-(1-0.050600*0.7) = 0.035420
#         male slice   = 16,245 * 0.494 = 8,025.03
#           uncirc = 8,025.03 * (1-0.30) = 5,617.52
#             cov = 5,617.52 / 113,715 = 0.049400
#             protection_gen_male_unc = 1-(1-0.049400*0.7) = 0.034580
#           circ   = 8,025.03 * 0.30 = 2,407.51
#             cov = 2,407.51 / 48,735 = 0.049400
#             protection_gen_male_circ = 1-(1-0.049400*0.7) = 0.034580
#       (uncirc and circ coverages are equal: each male sub-slice is scaled by
#        circ_prevalence and divided by a pool also scaled by it.)
# ---------------------------------------------------------------------------
test_that("general PrEP splits across general strata by female share and circ_prevalence", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()   # prop_agyw = 0 default
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  interv  <- make_fixture_interventions(prep_oral_general = 16245)
  adj     <- compute_prevention_adjustments(interv, strata, pops, sp)
  
  expect_close(adj$protection_gen_female,    0.035420, tolerance = 1e-5)
  expect_close(adj$protection_gen_male_unc,  0.034580, tolerance = 1e-5)
  expect_close(adj$protection_gen_male_circ, 0.034580, tolerance = 1e-5)
  # KP strata untouched by general PrEP
  expect_close(adj$protection_fsw, 0, tolerance = 1e-9)
  expect_close(adj$protection_msm, 0, tolerance = 1e-9)
  expect_close(adj$protection_agyw, 0, tolerance = 1e-9)
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
# ---- 2.6: NEW ----
# HOW: n_fsw + n_msm = 17,100 (test 2.2), same combined "high" pool as before
#      -- acts_high_total/acts_gen_total/condom_cov_high/condom_cov_gen are
#      numerically UNCHANGED from the old derivation (325). condom_cov_high
#      applies to BOTH fsw and msm (they share the same condom pool), so this
#      version additionally checks that protection_msm equals protection_fsw
#      -- a bug that only wired condoms into one of the two KP strata would
#      not have been catchable under the old single high_risk stratum.
test_that("condom allocation is demand-weighted (acts-based), not headcount-based", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  sp      <- define_strata_params(ctx)
  strata  <- partition_into_strata(pops, sp)
  
  interv  <- make_fixture_interventions(condoms = 1795500)
  adj     <- compute_prevention_adjustments(interv, strata, pops, sp)
  
  expect_close(adj$protection_fsw,        0.06,  tolerance = 1e-9)
  expect_close(adj$protection_msm,        0.06,  tolerance = 1e-9)
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
  baseline <- make_fixture_interventions()
  
  inf_low <- estimate_new_infections_foi(
    ctx, pops,
    scenario_interventions = make_fixture_interventions(prep_oral_fsw = 0),
    baseline_interventions = baseline
  )$new_infections
  
  inf_mid <- estimate_new_infections_foi(
    ctx, pops,
    scenario_interventions = make_fixture_interventions(prep_oral_fsw = 2000),
    baseline_interventions = baseline
  )$new_infections
  
  inf_high <- estimate_new_infections_foi(
    ctx, pops,
    scenario_interventions = make_fixture_interventions(prep_oral_fsw = 8000),
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
# HOW: # Tolerance widened 4 -> 6: "one unit of round() error per stratum" (per the
# original rationale), now 6 strata instead of 4.
test_that("by_stratum infections sum to total (within rounding)", {
  with_hiv_params(list(sexually_active_frac = SAFR))
  ctx     <- make_fixture_context()
  pops    <- calculate_populations(ctx)
  interv  <- make_fixture_interventions()
  
  res <- estimate_new_infections_foi(ctx, pops, interv,
                                     baseline_interventions = interv)
  
  sum_strata <- with(res$by_stratum,
                     fsw + msm + agyw + gen_female + gen_male_uncirc + gen_male_circ)
  expect_lte(abs(sum_strata - res$new_infections), 6)
})