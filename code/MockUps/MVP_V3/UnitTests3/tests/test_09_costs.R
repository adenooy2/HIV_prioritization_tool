# ============================================================================
# test_09_costs.R
# ----------------------------------------------------------------------------
# Tests for cost calculation branches in calculate_scenario_outcomes() that
# weren't covered explicitly in earlier files.
#
# Already covered elsewhere:
#   - Testing unit + linkage cost split        : test_03 (3.7)
#   - Condom raw-value vs number_reached cost  : test_04 (4.7)
#   - PrEP cost capped at eligible_pop         : test_04 (4.8)
#   - VMMC cost capped at uncircumcised_males  : test_04 (4.9)
#   - DSD cost = coverage × eligible × unit    : test_05 (5.6)
#   - Tracking cost = pool × coverage × unit   : test_05 (5.8)
#   - EID test + linkage cost split            : test_06 (6.8)
#
# Filled here:
#   9.1  EAC cost = eac_reach × unit_cost (layered VL/EAC coverage)
#   9.2  PMTCT linkage cost (line 1699): pmtct_cascade_linked_art × linkage_cost
#   9.3  CD4 testing cost = n_cd4_tested × unit_cost
#   9.4  AHD package cost = n_ahd_pkg_reached × unit_cost
#   9.5  ANC HIV testing unit cost separate from linkage cost
#   9.6  Sum-across-branches: multi-intervention cost = sum of individual costs
#   9.7  art_provision_cost = end_on_art × 200
#   9.7b art_provision_cost uses context$art_cost_standard when supplied
#   9.7c art_provision_cost falls back to ART_COST_STANDARD when context absent
#   9.8  total_cost = total_intervention_cost + art_provision_cost
#   9.9  Zero-intervention scenario: total_intervention_cost = 0
#   9.10 PNC VL testing unit cost
# ============================================================================

source("helpers.R")

LIVE_PARAMS_COSTS <- list(
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

LIVE_MTCT_RATES_C <- list(on_art_suppressed = 0.0033,
                          on_art_unsuppressed = 0.037,
                          not_on_art = 0.2)
LIVE_INFANT_MORT_C <- list(untreated = 0.3, on_art = 0.06, suppressed = 0.03)
LIVE_MORT_C <- list(
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

# Override all top-level constants set at source-time. Restores on exit.
override_cost_globals <- function(envir = parent.frame()) {
  snap <- list(s = ANNUAL_LTFU_RATE_STABLE, u = ANNUAL_LTFU_RATE_UNSTABLE,
               sp = ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE,
               rs = RETENTION_SUPPRESSION_RATE,
               cd4 = CD4_AHD_TARGETING_YIELD, use = USE_MORTALITY_CALIBRATION,
               mort = MORTALITY_RATES, mtct = MTCT_RATES,
               infant = INFANT_MORTALITY_RATES,
               art = ART_COST_STANDARD)
  assign("ANNUAL_LTFU_RATE_STABLE",              0.044, envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE",            0.14,  envir = .GlobalEnv)
  assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", 0,     envir = .GlobalEnv)
  assign("RETENTION_SUPPRESSION_RATE",           0.41,  envir = .GlobalEnv)
  assign("CD4_AHD_TARGETING_YIELD",              0.4,   envir = .GlobalEnv)
  assign("USE_MORTALITY_CALIBRATION",            FALSE, envir = .GlobalEnv)
  assign("MORTALITY_RATES",                      LIVE_MORT_C,         envir = .GlobalEnv)
  assign("MTCT_RATES",                           LIVE_MTCT_RATES_C,   envir = .GlobalEnv)
  assign("INFANT_MORTALITY_RATES",               LIVE_INFANT_MORT_C,  envir = .GlobalEnv)
  assign("ART_COST_STANDARD",                    200,   envir = .GlobalEnv)
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
            assign("ART_COST_STANDARD",                    SNAP_ART,  envir = .GlobalEnv)
          }, list(SNAP_S = snap$s, SNAP_U = snap$u, SNAP_SP = snap$sp,
                  SNAP_RS = snap$rs, SNAP_CD4 = snap$cd4, SNAP_USE = snap$use,
                  SNAP_MORT = snap$mort, SNAP_MTCT = snap$mtct,
                  SNAP_INF = snap$infant, SNAP_ART = snap$art)),
          add = TRUE), envir = envir)
  invisible(NULL)
}

base_ctx <- function() {
  make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                       yield_multipliers = list())
}

# ---------------------------------------------------------------------------
# 9.1 EAC cost = eac_reach × unit_cost
# ---------------------------------------------------------------------------
# WHAT: From lines 1585, 1591:
#   eac_reach <- populations$on_art × vl_cov_frac × unsuppressed_rate × eac_cov_frac
#   total_intervention_cost += eac_reach × unit_cost
# EAC is the only intervention that uses the layered VL/EAC coverage product —
# it operates on patients already identified as unsuppressed via routine VL.
#
# WHY: The dependency chain (VL coverage gates EAC reach) is easy to get wrong.
#      A bug that doesn't gate on VL would over-credit EAC reach and cost.
#
# HOW: Set vl_monitoring_routine = 100, adherence_counseling = 100.
#      Override unit costs: vl_monitoring$unit_cost = 8, eac$unit_cost = 25.
#      Set vl_monitoring efficacy = 1.0 (so vl_cov_frac = on_art × 1.0 / on_art = 1.0).
#      eac efficacy = 0.5 (so additional_suppressed isn't 100%, doesn't affect cost).
#
#      Derivation:
#        on_art = 36,000.
#        VL: number_reached = 36,000 × 1.0 = 36,000.
#        vl_cov_frac      = 36,000 / 36,000 = 1.0
#        unsuppressed_rate = 1 - 0.9 = 0.1
#        eac_cov_frac     = 1.0 (100% coverage)
#        eac_reach        = 36,000 × 1.0 × 0.1 × 1.0 = 3,600
#        EAC cost         = 3,600 × 25 = 90,000
#        VL cost          = 36,000 × 8 = 288,000
#        Total            = 378,000
# ---------------------------------------------------------------------------
test_that("EAC cost uses eac_reach (layered VL × unsuppressed × EAC coverage)", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  # vl_monitoring_routine lives in treatment_monitoring group;
  # adherence_counseling lives in retention_support group.
  ig_new <- intervention_groups
  ig_new$treatment_monitoring$interventions$vl_monitoring_routine$efficacy  <- 1.0
  ig_new$treatment_monitoring$interventions$vl_monitoring_routine$unit_cost <- 8
  ig_new$retention_support$interventions$adherence_counseling$efficacy      <- 0.5
  ig_new$retention_support$interventions$adherence_counseling$unit_cost     <- 25
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring,
                                retention_support    = ig_new$retention_support))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$vl_monitoring_routine <- 100
  interv$adherence_counseling  <- 100
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # 36,000 × 8 (VL) + 3,600 × 25 (EAC) = 288,000 + 90,000 = 378,000
  expect_close(result$total_intervention_cost, 378000)
  
  tol <- 1
  expect_lte(abs(result$treatment_monitoring_cost - 288000), tol)  # 36,000 × 8
  expect_lte(abs(result$retention_cost            -  90000), tol)  #  3,600 × 25
  expect_lte(abs(result$prevention_cost),        tol)
  expect_lte(abs(result$advanced_disease_cost),  tol)
})

# ---------------------------------------------------------------------------
# 9.2 PMTCT linkage cost (post-cascade, line 1699)
# ---------------------------------------------------------------------------
# WHAT: After the PMTCT cascade resolves,   newly-diagnosed pregnant
#       women are atempted to be linked to ART (pmtct_cascade_linked_art), a separate cost line
#       at 1699 charges:
#         total_intervention_cost += pmtct_pos × anc_hiv_testing$linkage_cost
#       This is in ADDITION to the per-test unit cost charged in the testing loop.
#
# WHY: Earlier ANC HIV tests (3.8 / 6.4) zeroed linkage_cost so this branch was
#      uncovered. PMTCT linkage cost is non-trivial in real budgets.
#
# HOW: anc_hiv_testing: efficacy=1.0, unit_cost=0, linkage_rate=0.8, linkage_cost=50.
#      ANC coverage 100%, all 125 pregnant_undiagnosed found and linked.
#      Expected PMTCT linkage cost = 125 × 50 = 6,250.
#      Testing unit cost = 0, so total_intervention_cost ≈ 6,250.
# ---------------------------------------------------------------------------
test_that("PMTCT linkage cost = pmtct_pos × anc_hiv_testing$linkage_cost", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$anc_hiv_testing$efficacy     <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$unit_cost    <- 0
  ig_new$testing$interventions$anc_hiv_testing$linkage_rate <- 0.8
  ig_new$testing$interventions$anc_hiv_testing$linkage_cost <- 50
  with_intervention_groups(list(testing = ig_new$testing))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$anc_hiv_testing <- 100
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # 125 × 50 = 6,250
  expect_close(result$total_intervention_cost, 6250)
})

# ---------------------------------------------------------------------------
# 9.3 CD4 testing cost = n_cd4_tested × unit_cost (gated by art_initiations)
# ---------------------------------------------------------------------------
# WHAT: From line 1862-1866. cd4_cost = n_cd4_tested × cd4_testing$unit_cost,
#       where n_cd4_tested = min(art_init × cd4_pct/100, art_init).
#       Only runs when art_initiations > 0.
#
# WHY: Inactive (zero) art_initiations should leave cd4 cost at 0 even with
#      cd4 coverage > 0.
#
# HOW: Force art_initiations: 10,000 tests at 5% yield with 100% linkage.
#      Fixture context sets prop_retesting = 0.30, so prop_new_dx = 0.70.
#        positive    = 10,000 × 0.05 × 1.0 = 500
#        new_dx      = 500 × 0.70 = 350
#        retest_pos  = 500 × 0.30 = 150
#      Linkage: under post-cap weighted-average approach (no average_linkage_cap),
#      L_avg = 1.0 (single intervention at linkage_rate = 1.0).
#        new_diagnoses           = 350 (new_diagnoses_cap_prop = 0.95 of
#                                       undiagnosed ≈ 0.95 × 5,000 = 4,750; cap
#                                       does not bind at 350)
#        re_engagement_testing   = 150 (testing_reengagement_cap = 0.45 ×
#                                       total_ltfu_pool ≈ 0.45 × 9,000 ≈ 4,050;
#                                       cap does not bind at 150)
#        art_init_testing        = 1.0 × (350 + 150) = 500
#      Then cascade ceiling line ~1858: min(500, max(0, 45000 + 350 - 34070 + 150))
#                                     = min(500, ~11,430) = 500.
#        art_initiations = 500.
#      With cd4_testing = 100, unit_cost = 12:
#        n_cd4_tested = 500 × 1.0 = 500
#        cd4_cost     = 500 × 12 = 6,000.
#      Other costs: test unit/linkage = 0 (set below). Expected total = 6,000.
# ---------------------------------------------------------------------------
test_that("CD4 testing cost = n_cd4_tested × cd4 unit_cost", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  ig_new$advanced_disease$interventions$cd4_testing$unit_cost     <- 12
  with_intervention_groups(list(testing = ig_new$testing,
                                advanced_disease = ig_new$advanced_disease))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$test_facility_general <- 10000
  interv$cd4_testing           <- 100
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # 500 × 12 = 6,000
  expect_lte(abs(result$total_intervention_cost - 6000), 1)
})

# ---------------------------------------------------------------------------
# 9.4 AHD package cost = n_ahd_pkg_reached × unit_cost
# ---------------------------------------------------------------------------
# WHAT: From line 1887. Charged separately from CD4 cost; gated by both
#       art_initiations > 0 AND ahd_pkg_value > 0.
#
# HOW: Same setup as 9.3. Add ahd_package = 100, ahd_package$unit_cost = 80.
#        art_initiations    = 500    (see 9.3 derivation)
#        prop_ahd_new_init  = 0.209
#        n_ahd_pool         = 500 × 0.209 = 104.5
#        n_cd4_tested       = 500 (100% CD4 coverage)
#        n_ahd_diagnosed    = min(500 × 0.4, 104.5) = min(200, 104.5) = 104.5
#        n_ahd_pkg_reached  = min(104.5 × 1.0, 104.5) = 104.5
#        ahd_pkg_cost       = 104.5 × 80 = 8,360
#      Plus CD4 cost from 9.3 = 6,000.
#      Total = 6,000 + 8,360 = 14,360.
# ---------------------------------------------------------------------------
test_that("AHD package cost = n_ahd_pkg_reached × unit_cost", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  ig_new$advanced_disease$interventions$cd4_testing$unit_cost     <- 12
  ig_new$advanced_disease$interventions$ahd_package$unit_cost     <- 80
  ig_new$advanced_disease$interventions$ahd_package$efficacy      <- 0.50
  with_intervention_groups(list(testing = ig_new$testing,
                                advanced_disease = ig_new$advanced_disease))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$test_facility_general <- 10000
  interv$cd4_testing           <- 100
  interv$ahd_package           <- 100
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: CD4 (6,000) + AHD pkg (104.5 × 80 = 8,360) = 14,360
  # Tolerance ±10 for cascade arithmetic rounding
  expect_lte(abs(result$total_intervention_cost - 14360), 10)
})

# ---------------------------------------------------------------------------
# 9.5 ANC HIV testing unit cost separate from linkage cost
# ---------------------------------------------------------------------------
# WHAT: ANC HIV testing has BOTH unit_cost (per test, all reached) and
#       linkage_cost (per pos pregnant women diagnosed). Both must accrue independently.
#
# HOW: Combine 6.4 setup with cost overrides.
#      ANC HIV: efficacy = 1.0, unit_cost = 3, linkage_rate = 1.0, linkage_cost = 50.
#      ANC coverage 100%:
#        number_reached = 23,875 (pregnant_hiv_testable)
#        unit cost     = 23,875 × 3 = 71,625
#        linkage cost  = 125 × 50 = 6,250
#        total         = 77,875
# ---------------------------------------------------------------------------
test_that("ANC HIV testing cost = unit × reached + linkage × pos", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$anc_hiv_testing$efficacy     <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$unit_cost    <- 3
  ig_new$testing$interventions$anc_hiv_testing$linkage_rate <- 0.8
  ig_new$testing$interventions$anc_hiv_testing$linkage_cost <- 50
  with_intervention_groups(list(testing = ig_new$testing))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$anc_hiv_testing <- 100
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # 23,875 × 3 + 125 × 50 = 71,625 + 6,250 = 77,875
  expect_close(result$total_intervention_cost, 77875)
})

# ---------------------------------------------------------------------------
# 9.6 Multi-intervention cost = sum of individual costs
# ---------------------------------------------------------------------------
# WHAT: Running two independent interventions together should produce the
#       SAME cost as the sum of running each individually (no cross-talk in
#       the cost accumulator).
#
# WHY: Cost loop bugs that share state between interventions would break this.
#
# HOW: Run (a) PrEP only, (b) condoms only, (c) both. Verify cost(both) ==
#      cost(PrEP) + cost(condoms).
# ---------------------------------------------------------------------------
test_that("multi-intervention total cost = sum of single-intervention costs", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_oral_fsw$unit_cost <- 80
  ig_new$prevention$interventions$prep_oral_fsw$efficacy  <- 0.99
  ig_new$prevention$interventions$condoms$unit_cost   <- 0.10
  ig_new$prevention$interventions$condoms$efficacy    <- 0.80
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  pops <- calculate_populations(base_ctx())
  
  interv_prep <- zero_interventions(); interv_prep$prep_oral_fsw <- 5000
  interv_cond <- zero_interventions(); interv_cond$condoms   <- 1e6
  interv_both <- zero_interventions()
  interv_both$prep_oral_fsw <- 5000; interv_both$condoms <- 1e6
  
  r_prep <- calculate_scenario_outcomes(base_ctx(), interv_prep, pops,
                                        is_baseline = TRUE,
                                        baseline_interventions = interv_prep)
  r_cond <- calculate_scenario_outcomes(base_ctx(), interv_cond, pops,
                                        is_baseline = TRUE,
                                        baseline_interventions = interv_cond)
  r_both <- calculate_scenario_outcomes(base_ctx(), interv_both, pops,
                                        is_baseline = TRUE,
                                        baseline_interventions = interv_both)
  
  expect_close(r_both$total_intervention_cost,
               r_prep$total_intervention_cost + r_cond$total_intervention_cost,
               tolerance = 2)
})

# ---------------------------------------------------------------------------
# 9.7 art_provision_cost = end_on_art × ART_COST_STANDARD + dsd_cost_adjustment
# ---------------------------------------------------------------------------
# WHAT: art_provision_cost <- end_on_art * ART_COST_STANDARD + dsd_cost_adjustment,
#       floored at 0. With no DSD interventions active, dsd_cost_adjustment = 0,
#       so art_provision_cost = end_on_art × ART_COST_STANDARD (= 200 in test).
#
# WHY: This is the single largest cost component in most scenarios. A typo
#      changing 200 to e.g. 20 would silently understate cost by 10×.
#      DSD-active cases are covered separately in tests 5.6, 5.6b, 5.6c.
#
# HOW: Standard fixture, NO interventions. end_on_art ≈ 33,843 (from test 8.2).
#      Expected art_provision_cost ≈ 33,843 × 200 = 6,768,600.
#
# NOTE: The returned end_on_art and art_provision_cost are BOTH rounded
#       in the result list, but rounded SEPARATELY from their unrounded
#       internal values. So result$end_on_art × 200 can differ from
#       result$art_provision_cost by up to ±200.
# ---------------------------------------------------------------------------
test_that("art_provision_cost = end_on_art × ART_COST_STANDARD with no DSD (within rounding)", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Allow ±200 because end_on_art and art_provision_cost round independently
  expect_lte(abs(result$art_provision_cost - result$end_on_art * ART_COST_STANDARD), 200)
  # Specific value check: within ±200 of 33,843 × 200 = 6,768,600
  expect_lte(abs(result$art_provision_cost - 33843 * 200), 200)
})

# ---------------------------------------------------------------------------
# 9.7b art_provision_cost uses context$art_cost_standard when supplied
# ---------------------------------------------------------------------------
# WHAT: When context$art_cost_standard is set (country-specific override from
#       basic_hiv_data.csv), the cost calc uses that value rather than the
#       global ART_COST_STANDARD.
#
# WHY: Country-specific ART unit costs were introduced so the model reflects
#      real per-country ART provision costs (range ~$100-$220 across the
#      14-country CSV) instead of a single global figure. Silent regression
#      (e.g. a future refactor that forgets the context override) would
#      reintroduce uniform costing.
#
# HOW: Override base_ctx() with art_cost_standard = 150 (vs global = 200 set
#      in fixture). end_on_art ≈ 33,843 (from test 8.2), so:
#        - Expected art_provision_cost ≈ 33,843 × 150 = 5,076,450
#        - Expected gap vs global ≈ 33,843 × 50 = 1,692,150  (well > 1000)
# ---------------------------------------------------------------------------
test_that("art_provision_cost uses context$art_cost_standard when supplied", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ctx <- base_ctx()
  ctx$art_cost_standard <- 150  # country-specific override
  
  pops <- calculate_populations(ctx)
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Country-specific value is used (±150 for independent rounding of
  # end_on_art and art_provision_cost)
  expect_lte(abs(result$art_provision_cost - result$end_on_art * 150), 150)
  # And the global is NOT used: gap should be ~1.7M, well above 1000
  expect_gt(abs(result$art_provision_cost - result$end_on_art * 200), 1000)
})

# ---------------------------------------------------------------------------
# 9.7c art_provision_cost falls back to ART_COST_STANDARD when context absent
# ---------------------------------------------------------------------------
# WHAT: When context$art_cost_standard is NULL (legacy CSVs without the
#       column, or the column blank/NA), the global ART_COST_STANDARD is used.
#
# WHY: Backwards compatibility. CSVs predating the art_cost_standard column
#      must behave exactly as before. This is functionally similar to test
#      9.7 but kept separate so a future contributor changing the fallback
#      path triggers an explicit, named failure.
#
# HOW: Use base_ctx() (no art_cost_standard field), confirm precondition,
#      verify art_provision_cost matches end_on_art × 200.
# ---------------------------------------------------------------------------
test_that("art_provision_cost falls back to ART_COST_STANDARD when context value absent", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ctx <- base_ctx()
  expect_null(ctx$art_cost_standard)  # confirm precondition
  
  pops <- calculate_populations(ctx)
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_lte(abs(result$art_provision_cost - result$end_on_art * 200), 200)
})

# ---------------------------------------------------------------------------
# 9.8 total_cost = total_intervention_cost + art_provision_cost
# ---------------------------------------------------------------------------
# WHAT: Line 2363: total_cost <- total_intervention_cost + art_provision_cost.
#
# WHY: Pin the bookkeeping identity. Display layer relies on this.
# ---------------------------------------------------------------------------
test_that("total_cost = total_intervention_cost + art_provision_cost (within rounding)", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$prep_oral_fsw$unit_cost <- 80
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$prep_oral_fsw <- 5000
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # All three values are independently rounded in the return list, so the
  # identity holds only to within a few dollars.
  expect_lte(abs(result$total_cost -
                   (result$total_intervention_cost + result$art_provision_cost)),
             2)
})

# ---------------------------------------------------------------------------
# 9.9 Zero-intervention scenario: total_intervention_cost = 0
# ---------------------------------------------------------------------------
# WHAT: With every intervention value = 0, total_intervention_cost must = 0.
#       Only art_provision_cost (end_on_art × 200) should remain.
#
# WHY: Sanity floor. Any cost branch that fires regardless of intervention
#      value would break this.
# ---------------------------------------------------------------------------
test_that("zero interventions produces zero total_intervention_cost", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()  # all zeros
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$total_intervention_cost, 0)
  # And total_cost equals just art_provision_cost
  expect_close(result$total_cost, result$art_provision_cost)
})

# ---------------------------------------------------------------------------
# 9.10 PNC VL testing cost = number_reached × unit_cost
# ---------------------------------------------------------------------------
# WHAT: PNC VL testing follows the "all other viral_suppression interventions"
#       path at lines 1597-1601. Cost = number_reached × unit_cost.
#
# WHY: PNC VL is the only test_06-style intervention we haven't checked the
#      cost branch for (test 6.3 zeroed costs).
#
# HOW: PNC VL: type = "coverage", eligible_pop = "pregnant_on_art".
#      pregnant_on_art = 900.
#      coverage 100%, unit_cost = 7.
#        number_reached = 900
#        cost           = 900 × 7 = 6,300
# ---------------------------------------------------------------------------
test_that("PNC VL testing cost = number_reached × unit_cost", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$retention_support$interventions$pnc_vl_testing$unit_cost <- 7
  ig_new$retention_support$interventions$pnc_vl_testing$efficacy  <- 1.0
  with_intervention_groups(list(retention_support = ig_new$retention_support))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$pnc_vl_testing <- 100
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # 900 × 7 = 6,300
  expect_close(result$total_intervention_cost, 6300)
})


# ---------------------------------------------------------------------------
# 9.11 Full multi-intervention baseline scenario: derived total cost
# ---------------------------------------------------------------------------
# WHAT: Activates a deliberately-chosen "baseline" mix of 9 interventions
#       spanning every cost branch we can derive cleanly by hand:
#         - prep_oral_fsw (capped at n_fsw, the FSW-only HIV-negative pool)
#         - condoms   (raw intervention_value, not number_reached)
#         - vmmc      (capped at uncircumcised_males_all)
#         - test_facility_general + test_community
#             (unit cost on tests performed; linkage cost on positives)
#         - mmd_3month (DSD -> dsd_cost_adjustment, NOT total_intervention_cost)
#         - tracking_tracing (deferred -> applied to full LTFU pool)
#         - cd4_testing + ahd_package (cascade-gated mortality interventions)
#       Then pins:
#         (a) total_intervention_cost = 518,174 (sum of 8 lines, derived below)
#         (b) art_provision_cost - end_on_art * ART_COST_STANDARD ~ 162,000
#             (the DSD adjustment from mmd_3month, within +/- 200 for rounding)
#         (c) total_cost = total_intervention_cost + art_provision_cost
#             (the bookkeeping identity, within +/- 2 for rounding)
#
# WHY: Tests 9.1-9.10 pin each cost branch in isolation. Test 9.6 checks
#      PrEP+condom additivity. This test goes further: it activates 9 branches
#      simultaneously, of which several depend on each other (CD4 gates AHD;
#      DSD reduces LTFU which changes total_ltfu_pool which sets tracking
#      reach; testing volumes feed art_initiations which sets the CD4 base).
#      A bug that double-counts a line, mis-routes DSD into the wrong bucket,
#      or breaks the shrinkage scaling on testing linkage cost would not show
#      up in any individual-branch test but would break (a) here.
#
# WHY NOT use default_baseline_interventions verbatim: those values include
#      ANC HIV testing + EID + infant prophy (PMTCT cascade -- complex caps
#      and yield-based costing) and VL monitoring + EAC (layered coverage
#      product). Each adds derivable but multi-line arithmetic that obscures
#      the test and is already covered by tests 6.8, 9.1, 9.5. The chosen
#      subset omits those branches to keep the hand-derivation auditable here.
#
# HOW (full hand-derivation):
#   Fixture populations (from base_ctx, helpers.R):
#     adult_pop = 1e6 * 0.60                            = 600,000
#     on_art                                            =  36,000
#     on_art_stable = 36,000 * 0.90                     =  32,400
#     uncircumcised_males_all = 1e6 * 0.50 * 0.70       = 350,000
#     undiagnosed                                       =   5,000
#     ltfu                                              =   9,000
#     ltfu_new_stable   = 32,400 * 0.044                = 1,425.6
#     ltfu_new_unstable =  3,600 * 0.14                 =   504.0
#
#   Step 1 -- DSD prevention of stable LTFU (mmd_3month at 50%, eff 0.5):
#     ltfu_retained_frac = (16,200 / 32,400) * 0.5      = 0.25
#     ltfu_prevented = 1,425.6 * 0.25                   = 356.4
#     stable_ltfu remaining = 1,425.6 - 356.4           = 1,069.2
#     ltfu_new_effective = 1,069.2 + 504.0              = 1,573.2
#     total_ltfu_pool = 9,000 + 1,573.2                 = 10,573.2
#
#   Step 2 -- Testing arithmetic (base_test_yield = 0.05, dilution = 1.0,
#            prop_retesting = 0.30 -> prop_new_dx = 0.70):
#     facility:  pos = 10,000 * 0.05           = 500  (new_dx=350, retest=150)
#     community: pos =  5,000 * 0.05           = 250  (new_dx=175, retest= 75)
#     new_diagnoses  = 525 < 4,750 cap (0.95 * undiagnosed)   -> shrinkage = 1
#     re_eng_testing = 225 < 4,757.94 cap (0.45 * pool)       -> shrinkage = 1
#     new_dx_linked     = 350*0.85 + 175*0.70 = 297.5 + 122.5 = 420
#     retest_pos_linked = 150*0.85 +  75*0.70 = 127.5 +  52.5 = 180
#     art_initiations   = 600
#     max_art_init      = 45,000 + 525 - 34,426.8 + 225 = 11,323.2
#       -> art_initiations final = 600 (cap doesn't bind)
#
#   Step 3 -- CD4 + AHD cascade (LIVE_MORT_C$prop_ahd$new_initiations = 0.209):
#     n_cd4_tested  = min(600 * 0.92, 600)              = 552
#     n_ahd_pool    = 600 * 0.209                       = 125.4
#     n_ahd_diag    = min(552 * CD4_AHD_TARGETING_YIELD=0.4, 125.4)
#                   = min(220.8, 125.4)                 = 125.4
#     n_ahd_pkg_reached = min(125.4 * 0.88, 125.4)      = 110.352
#
#   Step 4 -- Cost lines charged to total_intervention_cost:
#     PrEP        =  1,000   * 80    [min(1k, n_fsw=12,112.5) = 1k] =  80,000.0
#     Condoms     =100,000   * 0.10  [raw value, not reach]  =  10,000.0
#     VMMC        =  5,000   * 50    [min(5k, 350k) = 5k]    = 250,000.0
#     test_fac unit  = 10,000 * 2                            =  20,000.0
#     test_com unit  =  5,000 * 1                            =   5,000.0
#     test_fac link  = 500 * 20   [linkage cost per positive]=  10,000.0
#     test_com link  = 250 * 10                              =   2,500.0
#     tracking    = (10,573.2 * 0.40) * 30 = 4,229.28 * 30   = 126,878.4
#     CD4         =   552    * 5                             =   2,760.0
#     AHD         =   110.352* 100                           =  11,035.2
#     TOTAL_INTERVENTION_COST                                = 518,173.6
#       -> rounded in result list                            = 518,174
#
#   Step 5 -- DSD adjustment (charged to art_provision_cost, NOT above):
#     dsd_cost_adjustment = 16,200 * 200 * 0.05            = 162,000
#     art_provision_cost = end_on_art * 200 + 162,000
#       -> (result$art_provision_cost - result$end_on_art * 200) ~ 162,000
#         (allow +/- 200 because both fields round independently; observed
#         live-run delta is ~36, well within tolerance)
#
#   Step 6 -- Identity:
#     total_cost = total_intervention_cost + art_provision_cost (within +/- 2;
#     observed live-run delta is 1)
#
# NOTE on group placement (verified against logic file lines 386, 438, 481):
#   - mmd_3month   lives in treatment_monitoring (NOT retention_support)
#   - tracking_tracing lives in retention_support
#   - cd4_testing & ahd_package live in advanced_disease (NOT mortality)
# ---------------------------------------------------------------------------
test_that("full multi-intervention baseline: total_intervention_cost = 518,174 and identities hold", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  # Override unit costs, efficacies, linkage params for each intervention used.
  # Makes the test independent of SharePoint-loaded intervention_params values.
  ig_new <- intervention_groups
  
  # Prevention
  ig_new$prevention$interventions$prep_oral_fsw$unit_cost      <- 80
  ig_new$prevention$interventions$prep_oral_fsw$efficacy       <- 0.99
  ig_new$prevention$interventions$condoms$unit_cost            <- 0.10
  ig_new$prevention$interventions$condoms$efficacy             <- 0.80
  ig_new$prevention$interventions$vmmc$unit_cost               <- 50
  ig_new$prevention$interventions$vmmc$efficacy                <- 0.60
  
  # Testing
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 2
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 20
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 0.85
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_community$unit_cost           <- 1
  ig_new$testing$interventions$test_community$linkage_cost        <- 10
  ig_new$testing$interventions$test_community$linkage_rate        <- 0.70
  ig_new$testing$interventions$test_community$efficacy            <- 1.0
  
  # Treatment monitoring (DSD) -- unit_cost is FRACTIONAL (0.05 = 5% premium
  # over standard ART cost per person-year, not absolute USD)
  ig_new$treatment_monitoring$interventions$mmd_3month$unit_cost <- 0.05
  ig_new$treatment_monitoring$interventions$mmd_3month$efficacy  <- 0.5
  
  # Retention support (tracking)
  ig_new$retention_support$interventions$tracking_tracing$unit_cost <- 30
  ig_new$retention_support$interventions$tracking_tracing$efficacy  <- 0.6
  
  # Advanced disease (CD4 + AHD)
  ig_new$advanced_disease$interventions$cd4_testing$unit_cost <- 5
  ig_new$advanced_disease$interventions$cd4_testing$efficacy  <- 1.0
  ig_new$advanced_disease$interventions$ahd_package$unit_cost <- 100
  ig_new$advanced_disease$interventions$ahd_package$efficacy  <- 0.5
  
  with_intervention_groups(list(
    prevention           = ig_new$prevention,
    testing              = ig_new$testing,
    treatment_monitoring = ig_new$treatment_monitoring,
    retention_support    = ig_new$retention_support,
    advanced_disease     = ig_new$advanced_disease
  ))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$prep_oral_fsw          <- 1000
  interv$condoms               <- 100000
  interv$vmmc                  <- 5000
  interv$test_facility_general <- 10000
  interv$test_community        <- 5000
  interv$mmd_3month            <- 50   # 50% of on_art_stable
  interv$tracking_tracing      <- 40   # 40% of LTFU pool
  interv$cd4_testing           <- 92   # 92% of art_initiations
  interv$ahd_package           <- 88   # 88% of AHD-diagnosed
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # (a) Total intervention cost = 518,174 (exact, observed delta = 0).
  #     Use abs() check instead of expect_close to avoid testthat's relative
  #     tolerance interpretation; matches the style of tests 9.3, 9.7, 9.8.
  expect_lte(abs(result$total_intervention_cost - 518174), 1)
  
  # (b) DSD adjustment = 162,000 (16,200 × 200 × 0.05; unit_cost is fractional).
  #     Observed delta ~ 36, within +/- 200 tolerance to absorb independent
  #     rounding of end_on_art and art_provision_cost in the result list.
  expect_lte(
    abs((result$art_provision_cost - result$end_on_art * ART_COST_STANDARD) - 162000),
    200
  )
  
  # (c) Bookkeeping identity (observed delta = 1, within +/- 2 for triple-rounded
  #     fields).
  expect_lte(
    abs(result$total_cost - (result$total_intervention_cost + result$art_provision_cost)),
    2
  )
})

# ---------------------------------------------------------------------------
# 9.13 Country-specific test unit cost override (cost_overrides_test)
# ---------------------------------------------------------------------------
# WHAT: context$cost_overrides_test is a named list (key = intervention_key,
#       value = override unit cost in USD). When a key matches the loop's
#       int_key, the override replaces intervention$unit_cost at the cost-
#       charge site. Absent keys (NULL %||%) fall back to intervention$unit_cost.
#
# WHY:  Latent-fragility guard. If a future refactor changes the lookup key
#       (e.g. from int_key to intervention$name), moves the override branch
#       past the cost line, or breaks the %||% fallback for partial overrides,
#       this test fails immediately rather than the override silently being
#       ignored or all costs being overwritten.
#
# HOW:  Three sub-checks:
#       (a) test_facility_general (general HTS path): exact delta in absolute
#           USD; override raises unit cost from 4 to 9, applied to 10,000 tests.
#           Expected delta = 10000 × (9 - 4) = 50,000.
#       (b) anc_hiv_testing (ANC/PNC else-branch): cost scales linearly with
#           the override; uses ratio test (override_cost / unit_cost) so we
#           don't have to hardcode number_reached.
#       (c) Partial override (only hivst_facility supplied) leaves
#           test_facility_general cost unchanged — confirms per-key %||% fallback.
# ---------------------------------------------------------------------------
test_that("cost_overrides_test replaces intervention$unit_cost at the testing cost site", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  # Set known unit costs and zero linkage cost to isolate unit-cost effect.
  # efficacy/linkage_rate kept at defaults — irrelevant to cost arithmetic.
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 4
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  ig_new$testing$interventions$anc_hiv_testing$unit_cost          <- 3
  ig_new$testing$interventions$anc_hiv_testing$linkage_cost       <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  pops <- calculate_populations(base_ctx())
  
  # --- (a) test_facility_general: general HTS path -------------------------
  interv_fac <- zero_interventions()
  interv_fac$test_facility_general <- 10000
  
  ctx_nofix    <- base_ctx()
  ctx_override <- modifyList(
    base_ctx(),
    list(cost_overrides_test = list(test_facility_general = 9))
  )
  
  r_fac_nofix    <- calculate_scenario_outcomes(
    ctx_nofix,    interv_fac, pops,
    is_baseline = TRUE, baseline_interventions = interv_fac
  )
  r_fac_override <- calculate_scenario_outcomes(
    ctx_override, interv_fac, pops,
    is_baseline = TRUE, baseline_interventions = interv_fac
  )
  
  # Override raises unit cost from 4 to 9; same number_reached in both runs.
  # Expected delta = 10000 × (9 - 4) = 50,000.
  expect_lte(
    abs((r_fac_override$total_intervention_cost -
           r_fac_nofix$total_intervention_cost) - 50000),
    5
  )
  
  # --- (b) anc_hiv_testing: ANC/PNC else-branch path -----------------------
  interv_anc <- zero_interventions()
  interv_anc$anc_hiv_testing <- 80   # 80% coverage of pregnant_hiv_testable
  
  ctx_anc_override <- modifyList(
    base_ctx(),
    list(cost_overrides_test = list(anc_hiv_testing = 7))
  )
  
  r_anc_nofix    <- calculate_scenario_outcomes(
    base_ctx(),       interv_anc, pops,
    is_baseline = TRUE, baseline_interventions = interv_anc
  )
  r_anc_override <- calculate_scenario_outcomes(
    ctx_anc_override, interv_anc, pops,
    is_baseline = TRUE, baseline_interventions = interv_anc
  )
  
  # We avoid hardcoding number_reached (= pregnant_hiv_testable × 0.80).
  # Instead, derive nominal cost at unit_cost = 1 from the unfixed run
  # (where unit_cost = 3) and scale up to the override (= 7).
  nominal_at_unit_1      <- r_anc_nofix$total_intervention_cost / 3
  expected_override_cost <- nominal_at_unit_1 * 7
  expect_lte(
    abs(r_anc_override$total_intervention_cost - expected_override_cost),
    5
  )
  
  # --- (c) Partial override leaves unrelated keys untouched ---------------
  # Supplying an override for hivst_facility must NOT change the cost for
  # test_facility_general (different int_key). Confirms per-key %||% lookup.
  ctx_partial <- modifyList(
    base_ctx(),
    list(cost_overrides_test = list(hivst_facility = 99))
  )
  r_fac_partial <- calculate_scenario_outcomes(
    ctx_partial, interv_fac, pops,
    is_baseline = TRUE, baseline_interventions = interv_fac
  )
  expect_lte(
    abs(r_fac_partial$total_intervention_cost -
          r_fac_nofix$total_intervention_cost),
    5
  )
})


###############
test_that("cost category split reconstitutes total_intervention_cost and total_cost", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  pops   <- calculate_populations(base_ctx())
  interv <- default_baseline_interventions          # a fully populated scenario
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv)
  
  tol <- 5   # six independently round()ed fields => ±0.5 each
  cat_sum <- with(result, prevention_cost + testing_cost + treatment_monitoring_cost +
                    retention_cost + advanced_disease_cost)
  expect_lte(abs(cat_sum - result$total_intervention_cost), tol)
  expect_lte(abs(cat_sum + result$art_provision_cost - result$total_cost), tol)
})