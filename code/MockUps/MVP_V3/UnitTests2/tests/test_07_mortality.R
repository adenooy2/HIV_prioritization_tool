# ============================================================================
# test_07_mortality.R
# ----------------------------------------------------------------------------
# Tests for the mortality calculation block inside calculate_scenario_outcomes():
#
#   - 5-group death calc: undiagnosed, diagnosed_not_art, new_initiations,
#     established_treated, established_supp
#   - calc_deaths(n, base, ahd, prop_ahd) = n × ((1-prop_ahd)×base + prop_ahd×ahd)
#   - AHD package efficacy reduces eff_ahd_rate_new_init AND eff_ahd_rate_established
#   - AHD package gated by CD4 testing coverage and AHD targeting yield
#   - mortality_calibration_factor: defaults to 1; scales when toggle ON
#   - deaths_averted = unadjusted_on_treatment - adjusted_on_treatment
#
# Parameter values used (LIVE from the CSV you provided):
#   mortality_untreated_undiagnosed = 0.012
#   mortality_new_art_initiations    = 0.006
#   mortality_treated                = 0.0051
#   mortality_suppressed             = 0.0051
#   mortality_ahd_new                = 0.144
#   mortality_ahd_established        = 0.028
#   mortality_ahd_untreated          = 0.254
#   prop_ahd_undiagnosed             = 0.154
#   prop_ahd_diagnosed_not_art       = 0.209
#   prop_ahd_new_initiations         = 0.209
#   prop_ahd_established_treated     = 0.295
#   prop_ahd_established_supp        = 0.043
#   prop_cd4_ahd (CD4_AHD_TARGETING_YIELD) = 0.4
#
# NOTE: USE_MORTALITY_CALIBRATION is set to FALSE in the live source. Tests
# that exercise the calibration toggle override it temporarily.
# ============================================================================

source("helpers.R")  # rename test_helpers.R -> helpers.R per earlier discussion

# Live mortality parameters from your CSV
LIVE_MORT <- list(
  untreated_undiagnosed = 0.012,
  new_art_initiations    = 0.006,
  treated                = 0.0051,
  suppressed             = 0.0051,
  ahd_new                = 0.144,
  ahd_established        = 0.028,
  ahd_untreated          = 0.254,
  prop_ahd = list(
    undiagnosed         = 0.154,
    diagnosed_not_art   = 0.209,
    new_initiations     = 0.209,
    established_treated = 0.295,
    established_supp    = 0.043
  )
)

LIVE_PARAMS_MORT <- list(
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
  prop_cd4_ahd                     = 0.4
)

zero_interventions <- function() {
  setNames(as.list(rep(0, length(default_baseline_interventions))),
           names(default_baseline_interventions))
}

# Override the top-level LTFU and CD4 constants set at source-time, plus
# the mortality structures. Restores on exit.
override_mortality_globals <- function(envir = parent.frame()) {
  snap <- list(
    s    = ANNUAL_LTFU_RATE_STABLE,
    u    = ANNUAL_LTFU_RATE_UNSTABLE,
    sp   = ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE,
    rs   = RETENTION_SUPPRESSION_RATE,
    cd4  = CD4_AHD_TARGETING_YIELD,
    use  = USE_MORTALITY_CALIBRATION,
    mort = MORTALITY_RATES
  )
  assign("ANNUAL_LTFU_RATE_STABLE",              0.044,  envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE",            0.14,   envir = .GlobalEnv)
  assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", 0,      envir = .GlobalEnv)
  assign("RETENTION_SUPPRESSION_RATE",           0.41,   envir = .GlobalEnv)
  assign("CD4_AHD_TARGETING_YIELD",              0.4,    envir = .GlobalEnv)
  assign("USE_MORTALITY_CALIBRATION",            FALSE,  envir = .GlobalEnv)
  assign("MORTALITY_RATES",                      LIVE_MORT, envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute({
            assign("ANNUAL_LTFU_RATE_STABLE",              SNAP_S,    envir = .GlobalEnv)
            assign("ANNUAL_LTFU_RATE_UNSTABLE",            SNAP_U,    envir = .GlobalEnv)
            assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", SNAP_SP,   envir = .GlobalEnv)
            assign("RETENTION_SUPPRESSION_RATE",           SNAP_RS,   envir = .GlobalEnv)
            assign("CD4_AHD_TARGETING_YIELD",              SNAP_CD4,  envir = .GlobalEnv)
            assign("USE_MORTALITY_CALIBRATION",            SNAP_USE,  envir = .GlobalEnv)
            assign("MORTALITY_RATES",                      SNAP_MORT, envir = .GlobalEnv)
          }, list(SNAP_S = snap$s, SNAP_U = snap$u, SNAP_SP = snap$sp,
                  SNAP_RS = snap$rs, SNAP_CD4 = snap$cd4,
                  SNAP_USE = snap$use, SNAP_MORT = snap$mort)),
          add = TRUE),
          envir = envir)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Derivation of expected baseline mortality (no interventions, no LTFU prevention):
# ---------------------------------------------------------------------------
# Fixture: plhiv = 50,000; diagnosed = 45,000; on_art = 36,000; suppressed = 32,400.
# With no interventions, ltfu_new = 1,929.6 (from test 5.1), no tracking, no testing
# -> art_initiations = 0, ltfu_reengaged = 0, no DSD prevention.
# Then in the cascade pre-mort allocation:
#   end_diagnosed_pre_mort = populations$diagnosed + 0 = 45,000   (capped at plhiv)
#   effective_on_art       = 36,000 - 1,929.6 = 34,070.4
#   end_on_art_pre_mort    = min(34,070.4 + 0 + 0, 45,000) = 34,070.4
#   n_undiagnosed       = 50,000 - 45,000 = 5,000
#   n_diagnosed_not_art = 45,000 - 34,070.4 = 10,929.6
#   n_new_initiations   = min(0, 34,070.4) = 0
#   n_established_on_art = 34,070.4
#   pct_supp_frac        = 0.9
#   n_est_supp_base    = 34,070.4 × 0.9 = 30,663.36
#   n_est_treated_base = 34,070.4 - 30,663.36 = 3,407.04
#   n_new_supp_base    = 0; n_new_treated_base = 0
#
# additional_suppressed: with no testing, no EAC, no re-engagement, this is 0.
# intervention_supp_shift = 0 - 0 - 0 = 0.
# reengagement_supp_delta = 0 × (0.9 - 0.9) = 0
# So n_established_supp = n_est_supp_base = 30,663.36
# n_established_treated = 3,407.04
# n_new_supp = 0; n_new_treated = 0
#
# DEATHS with NO ahd_package, NO AHD adjustment to eff rates:
# eff_ahd_rate_new_init     = LIVE_MORT$ahd_new × (1 - 0) = 0.144
# eff_ahd_rate_established  = LIVE_MORT$ahd_established × (1 - 0) = 0.028
#
# deaths_undiagnosed = 5,000 × ((1 - 0.154) × 0.012 + 0.154 × 0.254)
#                    = 5,000 × (0.846 × 0.012 + 0.154 × 0.254)
#                    = 5,000 × (0.010152 + 0.039116)
#                    = 5,000 × 0.049268
#                    = 246.34
#
# deaths_diagnosed_not_art = 10,929.6 × ((1 - 0.209) × 0.012 + 0.209 × 0.254)
#                          = 10,929.6 × (0.791 × 0.012 + 0.209 × 0.254)
#                          = 10,929.6 × (0.009492 + 0.053086)
#                          = 10,929.6 × 0.062578
#                          = 683.95
#
# deaths_new_initiations = 0 (n_new_initiations = 0)
#
# deaths_established_treated = 3,407.04 × ((1 - 0.295) × 0.0051 + 0.295 × 0.028)
#                            = 3,407.04 × (0.705 × 0.0051 + 0.295 × 0.028)
#                            = 3,407.04 × (0.0035955 + 0.008260)
#                            = 3,407.04 × 0.0118555
#                            = 40.39
#
# deaths_established_supp = 30,663.36 × ((1 - 0.043) × 0.0051 + 0.043 × 0.028)
#                         = 30,663.36 × (0.957 × 0.0051 + 0.043 × 0.028)
#                         = 30,663.36 × (0.004881 + 0.001204)
#                         = 30,663.36 × 0.006085
#                         = 186.59
#
# total_hiv_deaths (no infants, pre-add) = 246.34 + 683.95 + 0 + 40.39 + 186.59
#                                        = 1,157.27
#
# With USE_MORTALITY_CALIBRATION = FALSE -> factor = 1.0 -> no change.
# end_deaths gets infant deaths added later. With no MTCT interventions,
# infant infections > 0 -> infant_deaths > 0. To isolate adult mortality
# in this test, we use deaths_undiagnosed / etc. (these are EXPOSED in
# the return list).
# ---------------------------------------------------------------------------

# ---------------------------------------------------------------------------
# 7.1 Each death-group rate matches calc_deaths formula
# ---------------------------------------------------------------------------
# WHAT: With no interventions and no calibration, the 5 deaths_* outputs
#       should match the calc_deaths formula applied to the cascade groups
#       above. We test the 4 non-zero groups (n_new_initiations = 0 -> 0).
# WHY:  Lock in the deaths formula. Bugs here would mis-attribute deaths
#       across cascade stages and corrupt both total_hiv_deaths and the
#       deaths_averted calculation that depends on the per-group split.
# HOW:  See full derivation above. Tolerance ±1 for rounding inside return.
# ---------------------------------------------------------------------------
test_that("per-group deaths match calc_deaths formula at baseline", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_lte(abs(result$deaths_undiagnosed         - 246), 1)
  expect_lte(abs(result$deaths_diagnosed_not_art   - 684), 1)
  expect_close(result$deaths_new_initiations,       0)
  expect_lte(abs(result$deaths_established_treated - 40),  1)
  expect_lte(abs(result$deaths_established_suppressed - 187), 1)
})

# ---------------------------------------------------------------------------
# 7.2 total_hiv_deaths_before_interventions = sum of 5 components
# ---------------------------------------------------------------------------
# WHAT: total_hiv_deaths_before_interventions in the return list equals
#       total_hiv_deaths + total_deaths_averted. At baseline with no
#       interventions on treatment groups, deaths_averted = 0, so this
#       equals the sum of all 5 deaths_* components.
# WHY:  Cross-check the bookkeeping. If this drifts, the deaths_averted
#       attribution is also wrong.
# HOW:  Same fixture as 7.1. Expected ~1,157.
# ---------------------------------------------------------------------------
test_that("total_hiv_deaths sums the 5 components at baseline", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Sum of components = 246.34 + 683.95 + 0 + 40.39 + 186.59 = 1,157.27
  component_sum <- result$deaths_undiagnosed +
    result$deaths_diagnosed_not_art +
    result$deaths_new_initiations +
    result$deaths_established_treated +
    result$deaths_established_suppressed
  
  # total_hiv_deaths_before_interventions exposed in return
  expect_lte(abs(result$total_hiv_deaths_before_interventions - component_sum), 4)
  expect_lte(abs(component_sum - 1157), 4)
})

# ---------------------------------------------------------------------------
# 7.3 mortality_calibration_factor defaults to 1.0 (toggle off)
# ---------------------------------------------------------------------------
# WHAT: Live USE_MORTALITY_CALIBRATION = FALSE. Even with aids_deaths_per_year
#       set, factor must remain 1.0.
# WHY:  Confirms the toggle works as a kill-switch.
# HOW:  Standard fixture (aids_deaths_per_year = 2,500). Modelled deaths ≈
#       1,157 + infant contribution. With USE = FALSE, factor = 1.0 regardless.
# ---------------------------------------------------------------------------
test_that("mortality_calibration_factor = 1.0 when toggle is FALSE", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()   # sets USE_MORTALITY_CALIBRATION = FALSE
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list(),
                              aids_deaths_per_year = 2500)
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$mortality_calibration_factor, 1.0)
})

# ---------------------------------------------------------------------------
# 7.4 mortality_calibration_factor scales when toggle ON
# ---------------------------------------------------------------------------
# WHAT: When USE_MORTALITY_CALIBRATION = TRUE, factor = target / modelled.
# WHY:  Verifies the calibration math when activated.
# HOW:  Override USE = TRUE. With modelled adult deaths ~1,157 and target
#       2,500:
#         factor = 2,500 / total_hiv_deaths (which includes infant contribution)
#       Test relationship: factor × pre_factor_total = target. Since the
#       infant contribution complicates exact derivation, we check the
#       invariant: factor > 1 if target > pre_calibration_total.
#       (Test 7.5 below pins this more precisely.)
# ---------------------------------------------------------------------------
test_that("mortality_calibration_factor responds to USE_MORTALITY_CALIBRATION = TRUE", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  assign("USE_MORTALITY_CALIBRATION", TRUE, envir = .GlobalEnv)
  # Note: override_mortality_globals's on.exit will restore USE to its original
  # FALSE state at end of test.
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list(),
                              aids_deaths_per_year = 2500)
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Adult-only pre-calibration deaths ≈ 1,157. Target 2,500. Factor should be > 1.
  # Total modelled deaths (incl. infants) is the actual denominator.
  expect_gt(result$mortality_calibration_factor, 1)
})

# ---------------------------------------------------------------------------
# 7.5 Calibration anchors total adult deaths to target (no infant pathway)
# ---------------------------------------------------------------------------
# WHAT: When calibration is ON and aids_deaths_per_year = T, the SUM of the
#       5 deaths_* components AFTER calibration should equal T (modulo the
#       infant contribution).
#
#       Working: factor = T / total_hiv_deaths_PRE.
#       After factor applied to each component: sum = total_hiv_deaths_PRE × factor = T.
#       The 5 deaths_* outputs in the return list are POST-factor.
# WHY:  This is the whole point of calibration. If sum != T, the anchor isn't
#       holding.
# HOW:  Strip infant contributions by setting birth_rate = 0 -> no births,
#       no MTCT, no infant deaths -> total_hiv_deaths reflects adults only.
#       Set aids_deaths_per_year = 2,500.
#         pre-calibration total ≈ 1,157
#         factor               ≈ 2,500 / 1,157 ≈ 2.160
#         post-calibration sum ≈ 2,500
# ---------------------------------------------------------------------------
test_that("calibration anchors adult deaths to aids_deaths_per_year target", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  assign("USE_MORTALITY_CALIBRATION", TRUE, envir = .GlobalEnv)
  
  # birth_rate = 0 -> hiv_exposed_births = 0 -> no infant infections / deaths
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list(),
                              aids_deaths_per_year = 2500,
                              birth_rate = 0)
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  adult_deaths_sum <- result$deaths_undiagnosed +
    result$deaths_diagnosed_not_art +
    result$deaths_new_initiations +
    result$deaths_established_treated +
    result$deaths_established_suppressed
  
  expect_within_pct(adult_deaths_sum, 2500, pct = 1)
  # Factor should be ~ 2,500 / 1,157.27 ≈ 2.160
  expect_within_pct(result$mortality_calibration_factor, 2500 / 1157.27, pct = 2)
})

# ---------------------------------------------------------------------------
# 7.6 With no AHD package, eff_ahd_rate_new_init = ahd_new (no reduction)
# ---------------------------------------------------------------------------
# WHAT: When ahd_pkg_value = 0, ahd_pkg_eff_reduction stays at 0 init value.
#       Then eff_ahd_rate_new_init = ahd_new × (1 - 0) = ahd_new = 0.144.
#       (Same logic for eff_ahd_rate_established.)
# WHY:  Cross-check that without the package, baseline mortality reflects
#       the full untouched AHD rate.
# HOW:  Force art_initiations > 0 via testing volume. Without CD4 or AHD
#       package, deaths_new_initiations should follow the unreduced formula.
#
#       This test requires actually generating new initiations. Set
#       test_facility_general with a tiny known volume so we can derive
#       n_new_initiations cleanly.
#       Override test_facility_general: efficacy=1, unit_cost=0, linkage_rate=1, linkage_cost=0
#       Volume 1,000 tests, yield 0.05 -> 50 positives.
#         new_dx     = 50 × (1 - prop_retest_default) = 50 × 0.41 = 20.5
#         retest_pos = 50 × 0.59 = 29.5
#         linked     = (20.5 + 29.5) × 1.0 = 50 -> art_initiations += 50
#
#       At line 1817 onwards (post-cap-removal), art_inititations_testing is
#       computed as L_avg × (new_diagnoses + re_engagement_testing), where
#       L_avg = 1.0 (single intervention at linkage_rate = 1.0). Caps on
#       new_diagnoses (cap = 0.95 × undiagnosed) and re_engagement_testing
#       (cap = 0.45 × total_ltfu_pool) do not bind at these small volumes.
#         art_inititations_testing = 1.0 × (20.5 + 29.5) = 50.
#
#       Then at line ~1858, art_initiations <- min(art_initiations,
#         max(0, diagnosed + new_diagnoses - effective_on_art + re_engagement))
#       = min(50, max(0, 45000 + 20.5 - 34,070.4 + 29.5)) = min(50, 10,979.6) = 50
#
#       n_new_initiations = 50
#       deaths_new_initiations = 50 × ((1 - 0.209) × 0.006 + 0.209 × 0.144)
#                              = 50 × (0.791 × 0.006 + 0.209 × 0.144)
#                              = 50 × (0.004746 + 0.030096)
#                              = 50 × 0.034842
#                              = 1.74 -> round to 2
# ---------------------------------------------------------------------------
test_that("eff_ahd_rate_new_init = ahd_new when no AHD package", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 1000
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected art_initiations = 50 (no cap binds; linkage_rate = 1.0)
  # Expected deaths_new_initiations ≈ 1.74 -> rounds to 2
  # Tolerance ±1 because of round() in return list
  expect_lte(abs(result$deaths_new_initiations - 2), 1)
})

# ---------------------------------------------------------------------------
# 7.7 AHD package reduces new-init AHD deaths
# ---------------------------------------------------------------------------
# WHAT: With CD4 testing 100% and AHD package 100%:
#         cd4_coverage_frac    = 1.0
#         n_cd4_tested         = art_initiations × 1.0 = art_initiations
#         n_ahd_pool           = art_initiations × prop_ahd_new_init = × 0.209
#         n_ahd_diagnosed      = min(n_cd4_tested × 0.4, n_ahd_pool)
#                              = min(art_init × 0.4, art_init × 0.209)
#                              = art_init × 0.209   (cap binds: CD4 yield > pool)
#         cd4_ahd_detection_frac = n_ahd_diagnosed / n_ahd_pool = 1.0
#         n_ahd_pkg_reached    = n_ahd_diagnosed × 1.0 = n_ahd_diagnosed
#         ahd_pkg_cov_frac_of_ahd = 1.0
#         ahd_pkg_eff_reduction = 1.0 × 1.0 × ahd_package$efficacy
#       Suppose ahd_package$efficacy = 0.50:
#         ahd_pkg_eff_reduction = 0.50
#       Then eff_ahd_rate_new_init = 0.144 × (1 - 0.50) = 0.072
#       AND eff_ahd_rate_established = 0.028 × 0.50 = 0.014
#
# WHY:  Verify the AHD package effect is correctly gated and applied to BOTH
#       new and established AHD groups.
# HOW:  Same testing fixture as 7.6 to get art_initiations = 50.
#       Add cd4_testing = 100, ahd_package = 100, ahd_package$efficacy = 0.50.
#       deaths_new_initiations = 50 × ((1 - 0.209) × 0.006 + 0.209 × 0.072)
#                              = 50 × (0.004746 + 0.015048)
#                              = 50 × 0.019794
#                              = 0.99 -> 1 (round)
# ---------------------------------------------------------------------------
test_that("AHD package halves AHD mortality contribution when fully applied", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  ig_new$advanced_disease$interventions$cd4_testing$unit_cost     <- 0
  ig_new$advanced_disease$interventions$ahd_package$efficacy      <- 0.50
  ig_new$advanced_disease$interventions$ahd_package$unit_cost     <- 0
  with_intervention_groups(list(testing = ig_new$testing,
                                advanced_disease = ig_new$advanced_disease))
  
  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 1000
  interv$cd4_testing  <- 100
  interv$ahd_package  <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected ≈ 1 (down from ≈2 without AHD package, see test 7.6)
  expect_lte(abs(result$deaths_new_initiations - 1), 1)
})

# ---------------------------------------------------------------------------
# 7.8 AHD package reduces established-AHD deaths too
# ---------------------------------------------------------------------------
# WHAT: ahd_pkg_eff_reduction is applied to eff_ahd_rate_established at line
#       2003-2004. With full coverage and efficacy 0.50:
#         eff_ahd_rate_established = 0.028 × (1 - 0.50) = 0.014
#       For n_established_treated = 3,407.04:
#         deaths_est_treated = 3,407.04 × (0.705 × 0.0051 + 0.295 × 0.014)
#                            = 3,407.04 × (0.0035955 + 0.004130)
#                            = 3,407.04 × 0.0077255
#                            = 26.32 -> round to 26
#       (vs ~40 without the package)
# WHY:  The AHD package conceptually targets new initiates with low CD4, but
#       the code reduction is applied to ESTABLISHED AHD rate too — locked
#       per source line 2003-04. Worth flagging in the doc block whether this
#       is intentional (continued AHD management on long-term care) or
#       a mis-attribution.
# HOW:  Same setup as 7.7.
# ---------------------------------------------------------------------------
test_that("AHD package also reduces established AHD mortality", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  ig_new$advanced_disease$interventions$cd4_testing$unit_cost     <- 0
  ig_new$advanced_disease$interventions$ahd_package$efficacy      <- 0.50
  ig_new$advanced_disease$interventions$ahd_package$unit_cost     <- 0
  with_intervention_groups(list(testing = ig_new$testing,
                                advanced_disease = ig_new$advanced_disease))
  
  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 1000
  interv$cd4_testing  <- 100
  interv$ahd_package  <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected deaths_established_treated ≈ 26 (down from ~40 with no package)
  expect_lte(abs(result$deaths_established_treated - 26), 2)
})

# ---------------------------------------------------------------------------
# 7.9 deaths_averted = unadjusted_on_treatment - adjusted_on_treatment
# ---------------------------------------------------------------------------
# WHAT: deaths_averted ONLY counts on-treatment groups (new_inits,
#       established_treated, established_supp). Untreated groups don't
#       contribute because no current intervention reduces their mortality
#       rate (line 2062-67).
# WHY:  Locks the deaths_averted formula. Sign-flip bugs here would invert
#       cost-effectiveness ratios.
# HOW:  Run the AHD package scenario (test 7.8). Without the package,
#       on-treatment deaths ≈ 0 + 40 + 187 = 227. With the package and
#       full coverage:
#         eff_ahd_rate_new_init  = 0.072 (halved)
#         eff_ahd_rate_est       = 0.014 (halved)
#       deaths_new_init  ≈ 1
#       deaths_est_treat ≈ 26  (from test 7.8)
#       deaths_est_supp  ≈ 30,663 × (0.957 × 0.0051 + 0.043 × 0.014)
#                       ≈ 30,663 × (0.004881 + 0.000602)
#                       ≈ 30,663 × 0.005483
#                       ≈ 168
#       Adjusted on-treatment = 1 + 26 + 168 = 195
#       Unadjusted on-treatment (without AHD package effect):
#         deaths_new_init (full ahd_new rate) ≈ 2
#         deaths_est_treat (full ahd_est rate) ≈ 40
#         deaths_est_supp  (full ahd_est rate) ≈ 187
#         Total = ~229
#       deaths_averted ≈ 229 - 195 = 34 (within rounding)
# ---------------------------------------------------------------------------
test_that("deaths_averted reflects on-treatment delta from AHD package", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  ig_new$advanced_disease$interventions$cd4_testing$unit_cost     <- 0
  ig_new$advanced_disease$interventions$ahd_package$efficacy      <- 0.50
  ig_new$advanced_disease$interventions$ahd_package$unit_cost     <- 0
  with_intervention_groups(list(testing = ig_new$testing,
                                advanced_disease = ig_new$advanced_disease))
  
  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 1000
  interv$cd4_testing  <- 100
  interv$ahd_package  <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # On-treatment delta should be roughly 34 (derivation above).
  # Loose tolerance ±10 because of rounding through several layered terms,
  # plus a small infant_deaths_averted contribution that may also accrue.
  expect_gt(result$deaths_averted, 20)
  expect_lt(result$deaths_averted, 60)
})

# ---------------------------------------------------------------------------
# 7.10 Partial CD4 coverage gates AHD package effect proportionally
# ---------------------------------------------------------------------------
# WHAT: ahd_pkg_eff_reduction = cd4_ahd_detection_frac × ahd_pkg_cov_frac × pkg_efficacy.
#       cd4_ahd_detection_frac is gated by CD4 coverage (since n_ahd_diagnosed
#       depends on n_cd4_tested = art_init × cd4_coverage_frac).
# WHY:  Without CD4 testing the AHD package cannot identify cases. So 0% CD4
#       coverage should reduce AHD effect to 0 even with 100% AHD package
#       coverage.
# HOW:  Run with ahd_package = 100 but cd4_testing = 0. ahd_pkg_eff_reduction
#       should resolve to 0. deaths_new_initiations and deaths_est_*
#       should match the no-package baseline (test 7.6 level).
# ---------------------------------------------------------------------------
test_that("AHD package has no effect when CD4 coverage = 0", {
  with_hiv_params(LIVE_PARAMS_MORT)
  override_mortality_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$test_facility_general$efficacy     <- 1.0
  ig_new$testing$interventions$test_facility_general$unit_cost    <- 0
  ig_new$testing$interventions$test_facility_general$linkage_rate <- 1.0
  ig_new$testing$interventions$test_facility_general$linkage_cost <- 0
  ig_new$advanced_disease$interventions$ahd_package$efficacy      <- 0.50
  ig_new$advanced_disease$interventions$ahd_package$unit_cost     <- 0
  with_intervention_groups(list(testing = ig_new$testing,
                                advanced_disease = ig_new$advanced_disease))
  
  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$test_facility_general <- 1000
  interv$cd4_testing  <- 0     # NO CD4 coverage
  interv$ahd_package  <- 100   # but full AHD package coverage
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Without CD4 detection, AHD package contributes 0 reduction.
  # deaths_new_initiations should match the no-package case (~2).
  expect_lte(abs(result$deaths_new_initiations - 2), 1)
})


# ---------------------------------------------------------------------------
# 7.11 Per-country mortality calibration flag (context$use_mortality_calibration)
# ---------------------------------------------------------------------------
# WHAT: The country-flag path triggers calibration even when the global toggle
#       USE_MORTALITY_CALIBRATION is FALSE. This is the path used in production
#       for South Africa via the basic_hiv_data.csv column.
# WHY:  Verifies that the new conditional path works and that existing tests'
#       assumption (global toggle off => no calibration) still holds when the
#       country flag is also off or NULL.
# ---------------------------------------------------------------------------

test_that("country flag triggers calibration when global toggle is off", {
  override_mortality_globals()   # sets USE_MORTALITY_CALIBRATION = FALSE
  
  ctx_calibrate <- make_fixture_context(aids_deaths_per_year = 2500,
                                        use_mortality_calibration = TRUE)
  interv <- make_fixture_interventions()
  pops   <- calculate_populations(ctx_calibrate)
  
  result <- calculate_scenario_outcomes(
    ctx_calibrate, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Calibration ON via country flag -> factor scales modelled deaths to target
  expect_gt(result$mortality_calibration_factor, 1.5)   # modelled ~1,157 vs target 2,500
  expect_within_pct(result$mortality_calibration_factor, 2500 / 1157.27, pct = 2)
})

test_that("country flag FALSE leaves global toggle as sole determinant", {
  override_mortality_globals()   # sets USE_MORTALITY_CALIBRATION = FALSE
  
  ctx_no_calib <- make_fixture_context(aids_deaths_per_year = 2500,
                                       use_mortality_calibration = FALSE)
  interv <- make_fixture_interventions()
  pops   <- calculate_populations(ctx_no_calib)
  
  result <- calculate_scenario_outcomes(
    ctx_no_calib, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$mortality_calibration_factor, 1.0)
})

test_that("country flag NULL behaves identically to FALSE", {
  override_mortality_globals()
  
  # Fixture without use_mortality_calibration field at all (the production
  # default for countries other than South Africa).
  ctx_null <- make_fixture_context(aids_deaths_per_year = 2500)
  # Confirm the field is genuinely absent
  expect_null(ctx_null$use_mortality_calibration)
  
  interv <- make_fixture_interventions()
  pops   <- calculate_populations(ctx_null)
  
  result <- calculate_scenario_outcomes(
    ctx_null, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$mortality_calibration_factor, 1.0)
})