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
})

# ---------------------------------------------------------------------------
# 9.2 PMTCT linkage cost (post-cascade, line 1699)
# ---------------------------------------------------------------------------
# WHAT: After the PMTCT cascade resolves how many newly-diagnosed pregnant
#       women linked to ART (pmtct_cascade_linked_art), a separate cost line
#       at 1699 charges:
#         total_intervention_cost += pmtct_cascade_linked_art × anc_hiv_testing$linkage_cost
#       This is in ADDITION to the per-test unit cost charged in the testing loop.
#
# WHY: Earlier ANC HIV tests (3.8 / 6.4) zeroed linkage_cost so this branch was
#      uncovered. PMTCT linkage cost is non-trivial in real budgets.
#
# HOW: anc_hiv_testing: efficacy=1.0, unit_cost=0, linkage_rate=1.0, linkage_cost=50.
#      ANC coverage 100%, all 125 pregnant_undiagnosed found and linked.
#      pmtct_cascade_linked_art = min(125, 350) × 1.0 = 125
#      Expected PMTCT linkage cost = 125 × 50 = 6,250.
#      Testing unit cost = 0, so total_intervention_cost ≈ 6,250.
# ---------------------------------------------------------------------------
test_that("PMTCT linkage cost = pmtct_linked × anc_hiv_testing$linkage_cost", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$anc_hiv_testing$efficacy     <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$unit_cost    <- 0
  ig_new$testing$interventions$anc_hiv_testing$linkage_rate <- 1.0
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
  expect_lte(abs(result$total_intervention_cost - 6000), 5)
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
#       linkage_cost (per PMTCT-linked). Both must accrue independently.
#
# HOW: Combine 6.4 setup with cost overrides.
#      ANC HIV: efficacy = 1.0, unit_cost = 3, linkage_rate = 1.0, linkage_cost = 50.
#      ANC coverage 100%:
#        number_reached = 23,875 (pregnant_hiv_testable)
#        unit cost     = 23,875 × 3 = 71,625
#        pmtct_linked  = 125
#        linkage cost  = 125 × 50 = 6,250
#        total         = 77,875
# ---------------------------------------------------------------------------
test_that("ANC HIV testing cost = unit × reached + linkage × linked", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$anc_hiv_testing$efficacy     <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$unit_cost    <- 3
  ig_new$testing$interventions$anc_hiv_testing$linkage_rate <- 1.0
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
  ig_new$prevention$interventions$prep_oral$unit_cost <- 80
  ig_new$prevention$interventions$prep_oral$efficacy  <- 0.99
  ig_new$prevention$interventions$condoms$unit_cost   <- 0.10
  ig_new$prevention$interventions$condoms$efficacy    <- 0.80
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  pops <- calculate_populations(base_ctx())
  
  interv_prep <- zero_interventions(); interv_prep$prep_oral <- 5000
  interv_cond <- zero_interventions(); interv_cond$condoms   <- 1e6
  interv_both <- zero_interventions()
  interv_both$prep_oral <- 5000; interv_both$condoms <- 1e6
  
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
  ig_new$prevention$interventions$prep_oral$unit_cost <- 80
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  interv$prep_oral <- 5000
  
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