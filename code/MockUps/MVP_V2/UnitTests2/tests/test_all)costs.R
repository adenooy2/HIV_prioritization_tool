# ============================================================================
# test_09_costs.R  (CONSOLIDATED)
# ----------------------------------------------------------------------------
# All cost-related unit tests for calculate_scenario_outcomes(), consolidated
# from across the suite into a single file.
#
# Previously, cost tests lived next to their logic context (testing costs in
# test_03, prevention costs in test_04, etc.). Consolidating them here gives
# a single audit point for the cost model. Logic-side behaviours those tests
# also exercised (yield, capping, stratum mechanics) remain covered by the
# non-cost tests that stayed in their original files.
#
# Coverage map (17 tests total):
#
#   ## Testing-modality costs (moved from test_03)
#   3.7   Testing modality cost = unit × tests + linkage × linked
#
#   ## Prevention costs (moved from test_04)
#   4.7   Condoms cost uses raw intervention_value, not number_reached
#   4.8   PrEP cost uses number_reached, capped at eligible_pop
#   4.9   VMMC cost capped at uncircumcised_males pool size
#
#   ## Retention costs (moved from test_05)
#   5.6   DSD cost = coverage × eligible × unit_cost (full reach)
#   5.8   Tracking cost = (pool × coverage) × unit_cost (pre-efficacy)
#
#   ## MTCT / EID cost (moved from test_06)
#   6.8   EID cost = test × reached + linkage × diagnosed
#
#   ## Originally lived here (test_09)
#   9.1   EAC cost = eac_reach × unit_cost (layered VL/EAC coverage)
#   9.2   PMTCT linkage cost = pmtct_linked × anc_hiv_testing$linkage_cost
#   9.3   CD4 testing cost = n_cd4_tested × unit_cost
#   9.4   AHD package cost = n_ahd_pkg_reached × unit_cost
#   9.5   ANC HIV testing unit cost separate from linkage cost
#   9.6   Multi-intervention cost = sum of individual costs
#   9.7   art_provision_cost = end_on_art × 200
#   9.8   total_cost = total_intervention_cost + art_provision_cost
#   9.9   Zero-intervention scenario: total_intervention_cost = 0
#   9.10  PNC VL testing cost = number_reached × unit_cost
#
# Test numbering preserved (3.7, 4.7, 4.8, 4.9, 5.6, 5.8, 6.8, 9.1..9.10) so
# any cross-references in docs and code comments remain valid.
# ============================================================================

source("helpers.R")

# ============================================================================
# Shared constants & helpers
# ============================================================================
#
# SAFR: live sexually_active_frac value used by every test below. Originally
# defined in test_04 (prevention) and test_03 (testing).
# ----------------------------------------------------------------------------
SAFR <- 0.85

# Live hiv_params snapshot. Superset of LIVE_PARAMS_COSTS, LIVE_PARAMS_RETENTION,
# LIVE_PARAMS_MTCT — they were identical apart from which keys each file
# needed. Using one composite block keeps every cost test on the same params.
LIVE_PARAMS_COSTS <- list(
  sexually_active_frac            = SAFR,
  ltfu_rate_stable                = 0.044,
  ltfu_rate_unstable              = 0.14,
  spontaneous_reengagement_rate   = 0,
  retention_suppression_rate      = 0.41,
  tracking_reengagement_supp      = 0.9,
  prop_on_art_stable_diff         = 0,
  testing_reengagement_cap_frac   = 0.45,
  testing_art_init_supp           = 0.9,
  prop_retest_default             = 0.59,
  new_diagnoses_cap_prop          = 0.95,
  average_linkage_cap             = 0.93,
  pmtct_cascade_supp_discount     = 0.9,
  eid_supp_rate                   = 0.38,
  prop_cd4_ahd                    = 0.4
)

# Hiv params shape used by tests originally in test_03 / test_04. Differs
# from LIVE_PARAMS_COSTS in prop_retest_default and average_linkage_cap.
# Preserved verbatim so the test arithmetic still matches.
LIVE_PARAMS_TESTING_03 <- list(
  sexually_active_frac          = SAFR,
  prop_retest_default           = 0.30,
  testing_reengagement_cap_frac = 0.45,
  testing_art_init_supp         = 0.90,
  new_diagnoses_cap_prop        = 0.95,
  average_linkage_cap           = 1.0
)

LIVE_PARAMS_PREVENTION_04 <- list(
  sexually_active_frac          = SAFR,
  prop_retest_default           = 0.59,
  testing_reengagement_cap_frac = 0.45,
  testing_art_init_supp         = 0.9,
  new_diagnoses_cap_prop        = 0.95,
  average_linkage_cap           = 0.93,
  pmtct_cascade_supp_discount   = 0.9
)

# Used by override_cost_globals (for the 9.x tests).
LIVE_MTCT_RATES_C  <- list(on_art_suppressed = 0.0033,
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

# Helper: zero out every intervention.
zero_interventions <- function() {
  setNames(as.list(rep(0, length(default_baseline_interventions))),
           names(default_baseline_interventions))
}

# Helper: standard cost-test context (round numbers, no prior-year testing).
base_ctx <- function() {
  make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                       yield_multipliers = list())
}

# Override globals set at source-time (LTFU rates, mortality, MTCT, etc.).
# Restores on exit of the calling test_that block. Originally in test_09.
override_cost_globals <- function(envir = parent.frame()) {
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
  assign("MORTALITY_RATES",                      LIVE_MORT_C,        envir = .GlobalEnv)
  assign("MTCT_RATES",                           LIVE_MTCT_RATES_C,  envir = .GlobalEnv)
  assign("INFANT_MORTALITY_RATES",               LIVE_INFANT_MORT_C, envir = .GlobalEnv)
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

# Lighter version used by tests originally from test_05 (DSD, tracking).
# They only need the LTFU/spontaneous/retention rates restored; mortality
# and MTCT untouched. Kept as a separate helper because override_cost_globals
# also forces USE_MORTALITY_CALIBRATION = FALSE which would change retention
# test arithmetic in cases where calibration was originally on.
override_ltfu_rates <- function(stable = 0.044, unstable = 0.14,
                                spontaneous = 0,
                                retention_supp = 0.41,
                                envir = parent.frame()) {
  snap <- list(
    s   = ANNUAL_LTFU_RATE_STABLE,
    u   = ANNUAL_LTFU_RATE_UNSTABLE,
    sp  = ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE,
    rs  = RETENTION_SUPPRESSION_RATE
  )
  assign("ANNUAL_LTFU_RATE_STABLE",              stable,         envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE",            unstable,       envir = .GlobalEnv)
  assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", spontaneous,    envir = .GlobalEnv)
  assign("RETENTION_SUPPRESSION_RATE",           retention_supp, envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute({
            assign("ANNUAL_LTFU_RATE_STABLE",                SNAP_S,  envir = .GlobalEnv)
            assign("ANNUAL_LTFU_RATE_UNSTABLE",              SNAP_U,  envir = .GlobalEnv)
            assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE",   SNAP_SP, envir = .GlobalEnv)
            assign("RETENTION_SUPPRESSION_RATE",             SNAP_RS, envir = .GlobalEnv)
          }, list(SNAP_S = snap$s, SNAP_U = snap$u, SNAP_SP = snap$sp,
                  SNAP_RS = snap$rs)),
          add = TRUE),
          envir = envir)
  invisible(NULL)
}

# Helper: override one retention intervention's params (preserves the rest).
# Tracking lives in retention_support; DSD (mmd_3/6/12, community_pickup) in
# treatment_monitoring. From test_05.
override_retention <- function(int_key, efficacy, unit_cost) {
  ig_new <- intervention_groups
  if (int_key == "tracking_tracing") {
    ig_new$retention_support$interventions[[int_key]]$efficacy  <- efficacy
    ig_new$retention_support$interventions[[int_key]]$unit_cost <- unit_cost
  } else {
    ig_new$treatment_monitoring$interventions[[int_key]]$efficacy  <- efficacy
    ig_new$treatment_monitoring$interventions[[int_key]]$unit_cost <- unit_cost
  }
  ig_new
}


# ============================================================================
# Testing-modality cost split — moved from test_03 (3.7)
# ============================================================================
# 3.7 Linkage cost = linked × linkage_cost (unit cost charged separately)
# ---------------------------------------------------------------------------
# WHAT: For each testing modality, two cost components accrue:
#       (a) unit_cost × number_reached  (every test, regardless of result)
#       (b) linkage_cost × linked       (per ART linkage achieved)
# WHY:  Splitting these matters for cost-effectiveness analysis. Mis-attributing
#       linkage cost to all tests inflates per-test costs.
# HOW:  10,000 tests below threshold, yield = 0.05, prop_new_dx = 0.70,
#       prop_reeng = 0.30, linkage_rate = 0.8, efficacy = 1.0.
#         positive_tests = 500; new_dx = 350; retest_pos = 150
#         linked         = (350 + 150) × 0.8 = 400
#         unit cost      = 10,000 × 5      = 50,000
#         linkage cost   = 400    × 10     = 4,000
#         total testing-driven cost = 54,000
# ---------------------------------------------------------------------------
test_that("testing modality cost = unit × tests + linkage_cost × linked", {
  with_hiv_params(LIVE_PARAMS_TESTING_03)
  
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
  
  # Expected unit cost:    10,000 × 5  = 50,000
  # Expected linkage cost:    400 × 10 =  4,000
  # Total intervention cost:           = 54,000
  expect_close(result$total_intervention_cost, 54000)
})


# ============================================================================
# Prevention costs — moved from test_04 (4.7, 4.8, 4.9)
# ============================================================================
# 4.7 Cost loop: condoms uses `intervention_value` (raw count), not number_reached
# ---------------------------------------------------------------------------
# WHAT: Line 2242-2243: for condoms, `units_costed = intervention_value`.
#       For all other adult_infections interventions, `units_costed =
#       number_reached` (capped at eligible_pop).
# WHY:  Locks the special-case for condoms. The intent is that condom unit
#       cost is per CONDOM DISTRIBUTED, not per person reached — so the cost
#       should not be capped at the sexually_active_negative pool size.
# HOW:  Override condoms efficacy/unit_cost to known values. Set condoms =
#       10,000,000 (well above eligible_pop). Expected cost = 10M × unit_cost.
# ---------------------------------------------------------------------------
test_that("condoms cost uses raw intervention_value, not number_reached", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$condoms$unit_cost <- 0.10
  ig_new$prevention$interventions$condoms$efficacy  <- 0.80
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
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
# 4.9 VMMC cost uses number_reached (capped at uncircumcised_males)
# ---------------------------------------------------------------------------
# WHAT: VMMC eligible_pop = "uncircumcised_males". Volume above this is
#       capped before costing.
# WHY:  VMMC supply >> pool is realistic for late-stage VMMC programmes; cost
#       must not balloon past pool exhaustion.
# HOW:  populations$uncircumcised_males = hiv_negative × 0.50 × (1 - 0.30)
#       = 332,500 (includes KP males — uses full hiv_negative, not sex active).
#       Set vmmc = 1,000,000; unit_cost = 50. Expected: 332,500 × 50 = 16,625,000.
# ---------------------------------------------------------------------------
test_that("VMMC cost capped at uncircumcised_males pool size", {
  with_hiv_params(LIVE_PARAMS_PREVENTION_04)
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$vmmc$unit_cost <- 50
  ig_new$prevention$interventions$vmmc$efficacy  <- 0.60
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$vmmc <- 1e6  # well above uncircumcised_males (332,500)
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: 332,500 × 50 = 16,625,000
  expect_close(result$total_intervention_cost, 1.6625e7)
})


# ============================================================================
# Retention costs — moved from test_05 (5.6, 5.8)
# ============================================================================
# 5.6 DSD cost applies to ALL stable clients reached (not just retained)
# ---------------------------------------------------------------------------
# WHAT: total_intervention_cost gets number_reached × unit_cost (line 1667-68),
#       where number_reached is the full coverage × eligible (here 50% × 32,400
#       = 16,200), regardless of retained frac.
# WHY:  Captures that you pay for delivering DSD to all enrolled stable
#       clients, not just those who would otherwise have been LTFU.
# HOW:  Override mmd_3month$unit_cost = 12 (arbitrary). Coverage 50% of 32,400.
#         expected cost = 16,200 × 12 = 194,400
# ---------------------------------------------------------------------------
test_that("DSD cost = coverage × eligible × unit_cost (full reach)", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_ltfu_rates()
  
  ig_new <- override_retention("mmd_3month", efficacy = 0.10, unit_cost = 12)
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$mmd_3month <- 50
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # 32,400 × 0.50 = 16,200; × 12 = 194,400
  expect_close(result$total_intervention_cost, 194400)
})

# ---------------------------------------------------------------------------
# 5.8 Tracking cost applies to the full deferred pool
# ---------------------------------------------------------------------------
# WHAT: total_intervention_cost += tracking_reached × unit_cost (line 1751-52).
#       tracking_reached = total_ltfu_pool × coverage (NOT × efficacy).
# WHY:  You pay to attempt tracking, not to succeed.
# HOW:  No DSD, so ltfu_new_effective = ltfu_new = 1,929.6.
#       total_ltfu_pool = 9,000 + 1,929.6 = 10,929.6.
#       tracking_tracing: efficacy = 0.50, unit_cost = 15, coverage = 40%.
#         tracking_reached = 10,929.6 × 0.40 = 4,371.84
#         expected cost    = 4,371.84 × 15   = 65,577.6 -> 65,578
# ---------------------------------------------------------------------------
test_that("tracking cost = (pool × coverage) × unit_cost (pre-efficacy)", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_ltfu_rates()
  
  ig_new <- intervention_groups
  ig_new$retention_support$interventions$tracking_tracing$efficacy  <- 0.50
  ig_new$retention_support$interventions$tracking_tracing$unit_cost <- 15
  with_intervention_groups(list(retention_support = ig_new$retention_support))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$tracking_tracing <- 40
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # 10,929.6 × 0.40 × 15 = 65,577.6 -> 65,578 after round
  expect_lte(abs(result$total_intervention_cost - 65578), 2)
})


# ============================================================================
# MTCT / EID cost — moved from test_06 (6.8)
# ============================================================================
# 6.8 EID cost: testing × all reached + linkage × infected diagnosed only
# ---------------------------------------------------------------------------
# WHAT: total cost from EID = eid_infants_reached × unit_cost
#                           + eid_infants_diagnosed × linkage_cost
#       Testing cost applies to ALL infants tested.
#       Linkage cost only to actual infected diagnoses.
# WHY:  Splitting these matters for cost-effectiveness — most EID tests come
#       back negative, but you still pay for them. Mis-attributing linkage
#       cost to all tested would inflate per-test costs.
# HOW:  Set eid$unit_cost = 5, eid$linkage_cost = 20.
#         eid_infants_reached   = 1,000
#         eid_infants_diagnosed ≈ 54.72
#         testing cost = 1,000 × 5  = 5,000
#         linkage cost = 54.72 × 20 = 1,094.4
#         total       ≈ 6,094
# ---------------------------------------------------------------------------
test_that("EID cost = test×reached + linkage_cost×diagnosed", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$eid$efficacy     <- 0.90
  ig_new$testing$interventions$eid$linkage_rate <- 0.80
  ig_new$testing$interventions$eid$unit_cost    <- 5
  ig_new$testing$interventions$eid$linkage_cost <- 20
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx  <- base_ctx()
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$eid <- 80
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected ≈ 5,000 + 54.72 × 20 = 5,000 + 1,094.4 = 6,094.4
  # Tolerance ±10 because eid_infants_diagnosed uses round() in return
  expect_lte(abs(result$total_intervention_cost - 6094), 10)
})


# ============================================================================
# Originally in test_09 — EAC, PMTCT linkage, CD4, AHD, ANC HIV combined,
# multi-intervention sum, ART provision, total cost, zero scenario, PNC VL
# ============================================================================
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
#      Set vl_monitoring efficacy = 1.0, eac efficacy = 0.5.
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
# HOW: Force art_initiations: 10,000 tests at 5% yield with 100% linkage
#      and prop_new_dx ~ 0.41.
#        positive = 10,000 × 0.05 × 1.0 = 500
#        new_dx   = 500 × 0.41 = 205
#        retest_pos = 500 × 0.59 = 295
#        linked   = 500 × 1.0 = 500
#        art_init_testing = min(500, 0.93 × (205 + 295)) = min(500, 465) = 465
#        Then cascade cap line 1824: min(465, max(0, 45000 + 205 - 34070 + 295))
#                                  = min(465, ~11,430) = 465.
#        art_initiations = 465.
#      With cd4_testing = 100, unit_cost = 12:
#        n_cd4_tested = 465 × 1.0 = 465
#        cd4_cost     = 465 × 12 = 5,580.
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
  
  # 465 × 12 = 5,580
  expect_lte(abs(result$total_intervention_cost - 5580), 5)
})

# ---------------------------------------------------------------------------
# 9.4 AHD package cost = n_ahd_pkg_reached × unit_cost
# ---------------------------------------------------------------------------
# WHAT: From line 1887. Charged separately from CD4 cost; gated by both
#       art_initiations > 0 AND ahd_pkg_value > 0.
#
# HOW: Same setup as 9.3. Add ahd_package = 100, ahd_package$unit_cost = 80.
#        art_initiations    = 465
#        prop_ahd_new_init  = 0.209
#        n_ahd_pool         = 465 × 0.209 = 97.185
#        n_cd4_tested       = 465 (100% CD4 coverage)
#        n_ahd_diagnosed    = min(465 × 0.4, 97.185) = min(186, 97.185) = 97.185
#        n_ahd_pkg_reached  = min(97.185 × 1.0, 97.185) = 97.185
#        ahd_pkg_cost       = 97.185 × 80 = 7,774.8 -> 7,775
#      Plus CD4 cost from 9.3 = 5,580.
#      Total = 5,580 + 7,775 ≈ 13,355.
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
  
  # Expected: CD4 (5,580) + AHD pkg (97.185 × 80 = 7,774.8) ≈ 13,355
  expect_lte(abs(result$total_intervention_cost - 13355), 10)
})

# ---------------------------------------------------------------------------
# 9.5 ANC HIV testing unit cost separate from linkage cost
# ---------------------------------------------------------------------------
# WHAT: ANC HIV testing has BOTH unit_cost (per test, all reached) and
#       linkage_cost (per PMTCT-linked). Both must accrue independently.
#
# HOW: anc_hiv_testing: efficacy=1.0, unit_cost=3, linkage_rate=1.0, linkage_cost=50.
#      ANC coverage 100%:
#        number_reached = 23,875 (pregnant_hiv_testable)
#        unit cost      = 23,875 × 3 = 71,625
#        pmtct_linked   = 125
#        linkage cost   = 125 × 50 = 6,250
#        total          = 77,875
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
# 9.7 art_provision_cost = end_on_art × 200
# ---------------------------------------------------------------------------
# WHAT: Line 2360: art_provision_cost <- end_on_art * 200.
#       Flat $200 per person on ART at year-end.
#
# WHY: This is the single largest cost component in most scenarios. A typo
#      changing 200 to e.g. 20 would silently understate cost by 10×.
#
# HOW: Standard fixture. end_on_art ≈ 33,843 (from test 8.2).
#      Expected art_provision_cost ≈ 33,843 × 200 = 6,768,600.
#
# NOTE: The returned end_on_art and art_provision_cost are BOTH rounded
#       in the result list, but rounded SEPARATELY from their unrounded
#       internal values. So result$end_on_art × 200 can differ from
#       result$art_provision_cost by up to ±200.
# ---------------------------------------------------------------------------
test_that("art_provision_cost = end_on_art × 200 (within rounding)", {
  with_hiv_params(LIVE_PARAMS_COSTS)
  override_cost_globals()
  
  pops <- calculate_populations(base_ctx())
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    base_ctx(), interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Allow ±200 because end_on_art and art_provision_cost round independently
  expect_lte(abs(result$art_provision_cost - result$end_on_art * 200), 200)
  # Specific value check: within ±200 of 33,843 × 200 = 6,768,600
  expect_lte(abs(result$art_provision_cost - 33843 * 200), 200)
})

# ---------------------------------------------------------------------------
# 9.8 total_cost = total_intervention_cost + art_provision_cost
# ---------------------------------------------------------------------------
# WHAT: Line 2363: total_cost <- total_intervention_cost + art_provision_cost.
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