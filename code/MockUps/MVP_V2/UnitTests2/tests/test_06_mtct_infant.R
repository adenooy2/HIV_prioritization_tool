# ============================================================================
# test_06_mtct_infant.R
# ----------------------------------------------------------------------------
# Tests for the MTCT cascade and infant mortality logic inside
# calculate_scenario_outcomes():
#
#   - baseline_infant_infections from 3-route MTCT formula (supp / unsupp /
#     no-ART maternal risk × MTCT_RATES)
#   - ANC VL testing shifts unsuppressed pregnant women on ART → suppressed
#   - PNC VL testing is applied AFTER ANC VL (no double-count, capped at
#     remaining unsuppressed)
#   - PMTCT linkage: newly-diagnosed HIV+ pregnant women routed into ART at
#     pmtct_linkage_rate, suppressed at country supp × pmtct_supp_discount
#   - Infant prophylaxis (NVP) reduces baseline_infant_infections multiplicatively
#   - EID testing cost applies to ALL HIV-exposed infants reached; linkage
#     cost only to actually diagnosed (yield-based)
#   - eid_infants_diagnosed = eid_infants_reached × actual_eid_yield × eid_efficacy
#   - Infant mortality cascade: untreated / on-ART unsuppressed / suppressed
#
# LIVE PARAMETER VALUES (from your CSV):
#   mtct_on_art_suppressed       = 0.0033
#   mtct_on_art_unsuppressed     = 0.037
#   mtct_not_on_art              = 0.2
#   infant_mort_untreated        = 0.3
#   infant_mort_on_art           = 0.06
#   infant_mort_suppressed       = 0.03
#   pmtct_cascade_supp_discount  = 0.9
#   eid_supp_rate                = 0.38
#
# Fixture-derived MTCT baseline (default fixture, no interventions):
#   pregnant_on_art_suppressed   = 810
#   pregnant_on_art_unsuppressed = 90
#   pregnant_not_on_art          = 350
#   pregnant_undiagnosed         = 125
#   pregnant_hiv_testable        = 23,875
#   baseline_infant_infections   = 810 × 0.0033 + 90 × 0.037 + 350 × 0.2
#                                = 2.673 + 3.33 + 70 = 76.003
#
# With no EID, no infant_prophylaxis:
#   end_infant_infections = round(76.003) = 76
#   infant_untreated      = 76 (all untreated)
#   total_infant_deaths   = 76 × 0.3 = 22.8 -> round to 23
# ============================================================================

source("helpers.R")

LIVE_MTCT_RATES <- list(
  on_art_suppressed   = 0.0033,
  on_art_unsuppressed = 0.037,
  not_on_art          = 0.2
)
LIVE_INFANT_MORT <- list(
  untreated  = 0.3,
  on_art     = 0.06,
  suppressed = 0.03
)

LIVE_PARAMS_MTCT <- list(
  sexually_active_frac             = 0.85,
  ltfu_rate_stable                 = 0.044,
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
  prop_cd4_ahd                     = 0.4,
  # Duration params for NVP efficacy adjustment (Step 4 of MTCT cascade).
  # Tests 6.5/6.6 use full-duration NVP (ratio=1.0) to keep arithmetic
  # unchanged. Test 6.13 explicitly tests partial-duration scaling.
  bf_duration_months               = 18,
  nvp_prophylaxis_duration_months  = 18
)

zero_interventions <- function() {
  setNames(as.list(rep(0, length(default_baseline_interventions))),
           names(default_baseline_interventions))
}

# Override MTCT and INFANT_MORTALITY constants set at source-time.
override_mtct_globals <- function(envir = parent.frame()) {
  snap <- list(
    mtct = MTCT_RATES, infant = INFANT_MORTALITY_RATES,
    s = ANNUAL_LTFU_RATE_STABLE, u = ANNUAL_LTFU_RATE_UNSTABLE,
    sp = ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE,
    rs = RETENTION_SUPPRESSION_RATE,
    cd4 = CD4_AHD_TARGETING_YIELD
  )
  assign("MTCT_RATES",                            LIVE_MTCT_RATES,    envir = .GlobalEnv)
  assign("INFANT_MORTALITY_RATES",                LIVE_INFANT_MORT,   envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_STABLE",               0.044,              envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE",             0.14,               envir = .GlobalEnv)
  assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE",  0,                  envir = .GlobalEnv)
  assign("RETENTION_SUPPRESSION_RATE",            0.41,               envir = .GlobalEnv)
  assign("CD4_AHD_TARGETING_YIELD",               0.4,                envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute({
            assign("MTCT_RATES",                            SNAP_M,    envir = .GlobalEnv)
            assign("INFANT_MORTALITY_RATES",                SNAP_I,    envir = .GlobalEnv)
            assign("ANNUAL_LTFU_RATE_STABLE",               SNAP_S,    envir = .GlobalEnv)
            assign("ANNUAL_LTFU_RATE_UNSTABLE",             SNAP_U,    envir = .GlobalEnv)
            assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE",  SNAP_SP,   envir = .GlobalEnv)
            assign("RETENTION_SUPPRESSION_RATE",            SNAP_RS,   envir = .GlobalEnv)
            assign("CD4_AHD_TARGETING_YIELD",               SNAP_CD4,  envir = .GlobalEnv)
          }, list(SNAP_M = snap$mtct, SNAP_I = snap$infant, SNAP_S = snap$s,
                  SNAP_U = snap$u, SNAP_SP = snap$sp, SNAP_RS = snap$rs,
                  SNAP_CD4 = snap$cd4)),
          add = TRUE),
          envir = envir)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# 6.1 Baseline infant infections (no MTCT interventions)
# ---------------------------------------------------------------------------
# WHAT: With NO ANC/PNC VL, NO ANC HIV testing, NO infant prophylaxis,
#       baseline_infant_infections = pregnant_on_art_suppressed × mtct_supp_rate
#                                  + pregnant_on_art_unsuppressed × mtct_unsupp_rate
#                                  + pregnant_not_on_art × mtct_no_art_rate
# WHY: This formula is the core MTCT calculation. Any change to the 3-route
#      structure breaks here.
# HOW: Fixture:
#        mtct_supp_pool   = 810 × 0.0033 = 2.673
#        mtct_unsupp_pool = 90  × 0.037  = 3.33
#        mtct_no_art_pool = 350 × 0.2    = 70
#        total            = 76.003 -> 76 (round)
#      end_infant_infections (no NVP) = baseline = 76.
# ---------------------------------------------------------------------------
test_that("baseline infant infections sum to 76 (3-route MTCT formula)", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$end_infant_infections, 76)
  # Also check the MTCT pool breakdown
  expect_close(result$mtct_pregnant_suppressed,   810)
  expect_close(result$mtct_pregnant_unsuppressed, 90)
  expect_close(result$mtct_pregnant_no_art,       350)
})

# ---------------------------------------------------------------------------
# 6.2 ANC VL testing shifts unsuppressed -> suppressed
# ---------------------------------------------------------------------------
# WHAT: ANC VL at 100% coverage with efficacy 1.0 shifts ALL pregnant_on_art_unsuppressed
#       women to suppressed.
# WHY: Pins the shift formula and the cap at populations$pregnant_on_art_unsuppressed.
# HOW: pregnant_on_art = 900 -> 100% coverage = 900 reached.
#      anc_vl_shift = min(900 × (1 - 0.9) × 1.0, 90) = min(90, 90) = 90
#      mtct_supp   = 810 + 90 = 900
#      mtct_unsupp = 0
#      baseline_infant_inf = 900 × 0.0033 + 0 × 0.037 + 350 × 0.2
#                          = 2.97 + 0 + 70 = 72.97 -> 73
# ---------------------------------------------------------------------------
test_that("ANC VL at 100% × eff=1.0 fully suppresses on-ART unsuppressed mothers", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$retention_support$interventions$anc_vl_testing$efficacy  <- 1.0
  ig_new$retention_support$interventions$anc_vl_testing$unit_cost <- 0
  with_intervention_groups(list(retention_support = ig_new$retention_support))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$anc_vl_testing <- 100   # 100% coverage of pregnant_on_art
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected: 900 × 0.0033 + 0 × 0.037 + 350 × 0.2 = 72.97 -> 73
  expect_close(result$end_infant_infections, 73)
  expect_close(result$mtct_pregnant_suppressed,   900)
  expect_close(result$mtct_pregnant_unsuppressed, 0)
})

# ---------------------------------------------------------------------------
# 6.3 PNC VL applied AFTER ANC VL (no double-count)
# ---------------------------------------------------------------------------
# WHAT: ANC VL at 50% coverage shifts SOME unsuppressed; PNC VL then acts on
#       remaining unsuppressed, not the original pool.
# WHY: Avoids double-counting the same women across the two interventions.
# HOW: ANC VL 50%, eff = 1.0:
#        anc_vl_reached = 900 × 0.5 = 450
#        anc_vl_shift   = min(450 × 0.1 × 1.0, 90) = min(45, 90) = 45
#        mtct_unsupp_after_ANC = 90 - 45 = 45
#      PNC VL 100%, eff = 1.0:
#        pnc_vl_reached = 900 × 1.0 = 900
#        pnc_vl_shift   = min(900 × 0.1 × 1.0, 45) = min(90, 45) = 45
#        mtct_unsupp_after_PNC = 0
#      mtct_supp = 810 + 45 + 45 = 900
#      infant_inf = 900 × 0.0033 + 0 × 0.037 + 350 × 0.2 = 72.97 -> 73
# ---------------------------------------------------------------------------
test_that("PNC VL applied after ANC VL, no double-count", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$retention_support$interventions$anc_vl_testing$efficacy  <- 1.0
  ig_new$retention_support$interventions$anc_vl_testing$unit_cost <- 0
  ig_new$retention_support$interventions$pnc_vl_testing$efficacy  <- 1.0
  ig_new$retention_support$interventions$pnc_vl_testing$unit_cost <- 0
  with_intervention_groups(list(retention_support = ig_new$retention_support))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$anc_vl_testing <- 50
  interv$pnc_vl_testing <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$mtct_pregnant_suppressed,   900)
  expect_close(result$mtct_pregnant_unsuppressed, 0)
  expect_close(result$end_infant_infections, 73)
})

# ---------------------------------------------------------------------------
# 6.4 PMTCT linkage: newly-diagnosed HIV+ pregnant women → ART
# ---------------------------------------------------------------------------
# WHAT: ANC HIV testing at 100% × eff=1.0 identifies all pregnant_undiagnosed
#       (125 women). Of these, anc_hiv_testing$linkage_rate fraction link to ART;
#       of those, country supp × pmtct_supp_discount fraction suppress.
# WHY: Locks the PMTCT routing math. Errors here understate or overstate
#      PMTCT impact on infant infections.
# HOW: Set anc_hiv_testing$linkage_rate = 1.0, efficacy = 1.0, costs = 0.
#       pmtct_new_diagnoses = 125 (all undiagnosed found at ANC)
#       pmtct_linked_total  = min(125, 350) = 125
#       pmtct_linked_art    = 125 × 1.0 = 125
#       pmtct_supp_rate     = (90/100) × 0.9 = 0.81
#       pmtct_linked_supp   = 125 × 0.81 = 101.25
#       pmtct_linked_unsupp = 125 - 101.25 = 23.75
#       pmtct_not_linked    = 125 - 125 = 0
#       mtct_supp   = 810 + 0 (no ANC VL shift) + 101.25 = 911.25
#       mtct_unsupp = 90 + 23.75 = 113.75
#       mtct_no_art = max(0, 350 - 125) + 0 = 225
#       baseline_infant_inf = 911.25 × 0.0033 + 113.75 × 0.037 + 225 × 0.2
#                           = 3.007 + 4.209 + 45 = 52.22 -> 52
# ---------------------------------------------------------------------------
test_that("ANC HIV testing routes undiagnosed pregnant women through PMTCT cascade", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$anc_hiv_testing$efficacy     <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$linkage_rate <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$unit_cost    <- 0
  ig_new$testing$interventions$anc_hiv_testing$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$anc_hiv_testing <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$pmtct_newly_diagnosed,  125)
  expect_close(result$pmtct_newly_linked,     125)
  # pmtct_newly_suppressed = 125 × 0.81 = 101.25 -> 101
  expect_lte(abs(result$pmtct_newly_suppressed - 101), 1)
  # end_infant_infections ≈ 52
  expect_lte(abs(result$end_infant_infections - 52), 1)
})

# ---------------------------------------------------------------------------
# 6.5 Infant prophylaxis reduces infant infections multiplicatively (full duration)
# ---------------------------------------------------------------------------
# WHAT: NVP efficacy is duration-adjusted in the logic:
#         nvp_eff_adjusted = raw_eff × min(1, nvp_prophy_months / bf_months)
#       LIVE_PARAMS_MTCT sets bf_duration_months = nvp_prophylaxis_duration_months = 18,
#       so ratio = 1.0 and nvp_eff_adjusted = raw_eff. This pins the full-duration
#       case where adjusted efficacy equals raw efficacy.
#       infant_prophy_reduction = baseline × cov_frac × nvp_eff_adjusted
# WHY: Isolates the coverage/efficacy multiplication from duration scaling.
#      Test 6.13 explicitly tests partial-duration scaling.
# HOW: eff=1.0, cov=100%, ratio=1.0:
#        cov_frac         = 1.0 × 1.0 = 1.0
#        nvp_eff_adjusted = 1.0 × 1.0 = 1.0
#        reduction        = 76 × 1.0 × 1.0 = 76
#        end_infant_inf   = max(0, 76 - 76) = 0
# ---------------------------------------------------------------------------
test_that("infant prophylaxis at 100% × eff=1 reduces infections to 0", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$infant_prophylaxis$efficacy  <- 1.0
  ig_new$prevention$interventions$infant_prophylaxis$unit_cost <- 0
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$infant_prophylaxis <- 100   # 100% coverage of HIV-exposed infants
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$end_infant_infections,     0)
  expect_close(result$infant_infections_averted, 76)
})

# ---------------------------------------------------------------------------
# 6.6 Infant prophylaxis: duration-adjusted efficacy scales reduction linearly
# ---------------------------------------------------------------------------
# WHAT: With raw efficacy = 0.50, full-duration NVP (ratio=1.0), coverage = 100%:
#         nvp_eff_adjusted = 0.50 × 1.0 = 0.50
#         cov_frac         = 1.0 × 0.50 = 0.50
#         reduction        = 76 × 0.50  = 38
#       LIVE_PARAMS_MTCT sets ratio=1.0 so this is equivalent to the pre-change
#       single-application formula.
# WHY: Verifies efficacy scales the reduction linearly at full NVP duration.
#      If duration adjustment is misapplied (e.g. ratio applied twice),
#      reduction would be 76 × 0.50 × (1.5/18) = ~3 instead of 38.
# HOW: eff = 0.50, cov = 100%, ratio = 1.0:
#        nvp_eff_adjusted = 0.50
#        reduction        = 76 × 0.50 = 38
#        end_infant_inf   = 76 - 38 = 38
# ---------------------------------------------------------------------------
test_that("infant prophylaxis applies efficacy linearly (single-application)", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$infant_prophylaxis$efficacy  <- 0.50
  ig_new$prevention$interventions$infant_prophylaxis$unit_cost <- 0
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$infant_prophylaxis <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Single-efficacy: reduction = 76 × 0.50 = 38
  expect_lte(abs(result$end_infant_infections     - 38), 1)
  expect_lte(abs(result$infant_infections_averted - 38), 1)
})

# ---------------------------------------------------------------------------
# 6.7 EID diagnosed = reached × actual_yield × efficacy
# ---------------------------------------------------------------------------
# WHAT: eid_infants_diagnosed = eid_infants_reached × actual_eid_yield × efficacy
#       where actual_eid_yield = end_infant_infections / hiv_exposed_infants.
# WHY: EID yield depends on the INFECTION rate among HIV-exposed infants, not
#      on a fixed test sensitivity. If MTCT interventions reduce infections,
#      EID yield drops too. Tests this linkage.
# HOW: At baseline (no MTCT shift), end_infant_inf = 76. hiv_exposed_infants = 1,250.
#      actual_eid_yield = 76 / 1,250 = 0.0608
#      Set EID coverage = 80, eid efficacy = 0.90 (live default).
#        eid_infants_reached = 1,250 × 0.80 = 1,000
#        eid_infants_diagnosed = 1,000 × 0.0608 × 0.90 = 54.72 -> 55
# ---------------------------------------------------------------------------
test_that("EID diagnosed = reached × actual yield × efficacy", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$eid$efficacy     <- 0.90
  ig_new$testing$interventions$eid$linkage_rate <- 0.80
  ig_new$testing$interventions$eid$unit_cost    <- 0
  ig_new$testing$interventions$eid$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$eid <- 80   # 80% coverage of HIV-exposed infants
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # 1,000 × (76 / 1,250) × 0.90 = 1,000 × 0.0608 × 0.90 = 54.72 -> 55
  expect_lte(abs(result$eid_infants_diagnosed - 55), 1)
})

# ---------------------------------------------------------------------------
# 6.8 EID cost: testing × all reached + linkage × infected diagnosed only
# ---------------------------------------------------------------------------
# WHAT: total cost from EID = eid_infants_reached × unit_cost
#                           + eid_infants_diagnosed × linkage_cost
#       Testing cost applies to ALL infants tested (per Step 5 comment).
#       Linkage cost only to actual infected diagnoses.
# WHY: Splitting these matters for cost-effectiveness — most EID tests come
#      back negative, but you still pay for them. Mis-attributing linkage
#      cost to all tested would inflate per-test costs.
# HOW: Set eid$unit_cost = 5, eid$linkage_cost = 20.
#        eid_infants_reached   = 1,000
#        eid_infants_diagnosed ≈ 54.72
#        testing cost = 1,000 × 5  = 5,000
#        linkage cost = 54.72 × 20 = 1,094.4
#        total       ≈ 6,094
# ---------------------------------------------------------------------------
test_that("EID cost = test×reached + linkage_cost×diagnosed", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$eid$efficacy     <- 0.90
  ig_new$testing$interventions$eid$linkage_rate <- 0.80
  ig_new$testing$interventions$eid$unit_cost    <- 5
  ig_new$testing$interventions$eid$linkage_cost <- 20
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
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

# ---------------------------------------------------------------------------
# 6.9 Infant mortality cascade at baseline (no EID)
# ---------------------------------------------------------------------------
# WHAT: With no EID interventions, eid_infants_diagnosed = 0 -> infant_on_art = 0,
#       infant_suppressed = 0, infant_untreated = end_infant_infections.
#       total_infant_deaths = end_infant_inf × INFANT_MORTALITY_RATES$untreated
# WHY: Default-untreated mortality is the worst-case scenario. Locks the
#      ceiling.
# HOW: end_infant_infections = 76; INFANT_MORTALITY_RATES$untreated = 0.3.
#      total_infant_deaths = 76 × 0.3 = 22.8 -> 23
# ---------------------------------------------------------------------------
test_that("infant mortality at baseline equals all-untreated count", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$infant_on_art,         0)
  expect_close(result$infant_suppressed,     0)
  # 76 × 0.3 = 22.8 -> 23
  expect_lte(abs(result$total_infant_deaths - 23), 1)
})

# ---------------------------------------------------------------------------
# 6.10 EID + linkage reduces infant mortality
# ---------------------------------------------------------------------------
# WHAT: EID-diagnosed infants split by linkage and suppression:
#         infant_on_art        = eid_infants_diagnosed × eid_linkage_rate
#         infant_suppressed    = infant_on_art × eid_supp_rate
#         infant_on_art_unsupp = infant_on_art - infant_suppressed
#         infant_untreated     = max(0, end_inf - infant_on_art)
#       Deaths weighted by each group's mortality rate.
# WHY: Verifies the cascade-based infant mortality structure.
# HOW: EID 100% coverage, efficacy = 1.0, linkage_rate = 1.0.
#        eid_infants_diagnosed = 1,250 × 1.0 × (76/1250) × 1.0 = 76
#        infant_on_art        = 76 × 1.0 = 76
#        infant_suppressed    = 76 × 0.38 = 28.88 (eid_supp_rate live)
#        infant_on_art_unsupp = 76 - 28.88 = 47.12
#        infant_untreated     = max(0, 76 - 76) = 0
#      Deaths:
#        suppressed:  28.88 × 0.03 = 0.866
#        on_art:      47.12 × 0.06 = 2.827
#        untreated:   0     × 0.30 = 0
#        total ≈ 3.69 -> 4
# ---------------------------------------------------------------------------
test_that("full EID coverage reduces infant deaths via cascade mortality", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$eid$efficacy     <- 1.0
  ig_new$testing$interventions$eid$linkage_rate <- 1.0
  ig_new$testing$interventions$eid$unit_cost    <- 0
  ig_new$testing$interventions$eid$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$eid <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Expected deaths ≈ 4 (down from 23 untreated baseline)
  expect_lt(result$total_infant_deaths, 6)
  # Substantial deaths averted vs untreated counterfactual (76 × 0.3 = 22.8)
  expect_gt(result$infant_deaths_averted, 15)
})

# ---------------------------------------------------------------------------
# 6.11 pregnant_undiagnosed cap on PMTCT new diagnoses
# ---------------------------------------------------------------------------
# WHAT: pmtct_new_diagnoses += min(pmtct_candidates, pregnant_undiagnosed).
#       So even if ANC HIV testing efficacy > 1.0 or yield is over-estimated,
#       you cannot diagnose more women than are actually undiagnosed.
# WHY: Hard cap on the cascade entry point. Critical sanity check.
# HOW: Set ANC HIV testing efficacy = 2.0 (impossibly high) at 100% coverage.
#      With pregnant_undiagnosed = 125, pmtct_new_diagnoses should still cap
#      at 125 (not 250 or higher).
# ---------------------------------------------------------------------------
test_that("PMTCT new diagnoses capped at pregnant_undiagnosed even with eff>1", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$testing$interventions$anc_hiv_testing$efficacy     <- 2.0   # impossibly high
  ig_new$testing$interventions$anc_hiv_testing$linkage_rate <- 1.0
  ig_new$testing$interventions$anc_hiv_testing$unit_cost    <- 0
  ig_new$testing$interventions$anc_hiv_testing$linkage_cost <- 0
  with_intervention_groups(list(testing = ig_new$testing))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$anc_hiv_testing <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # Capped at 125 (pregnant_undiagnosed = 1,250 × 0.10)
  expect_close(result$pmtct_newly_diagnosed, 125)
})

# ---------------------------------------------------------------------------
# 6.12 ANC VL shift capped at pregnant_on_art_unsuppressed
# ---------------------------------------------------------------------------
# WHAT: anc_vl_shift = min(reached × unsupp_rate × eff, pregnant_on_art_unsuppressed).
#       Cap ensures you cannot shift more women than are actually unsuppressed.
# WHY: Hard cap analogous to 6.11 — guards against double-counting and
#      over-attribution.
# HOW: Override anc_vl_testing$efficacy = 5.0 at 100% coverage.
#        Reach = 900; raw shift = 900 × 0.1 × 5.0 = 450.
#        Cap at populations$pregnant_on_art_unsuppressed = 90.
#        Final shift = 90. So mtct_unsupp should be 0, not negative.
# ---------------------------------------------------------------------------
test_that("ANC VL shift capped at pregnant_on_art_unsuppressed even with eff>1", {
  with_hiv_params(LIVE_PARAMS_MTCT)
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$retention_support$interventions$anc_vl_testing$efficacy  <- 5.0   # impossibly high
  ig_new$retention_support$interventions$anc_vl_testing$unit_cost <- 0
  with_intervention_groups(list(retention_support = ig_new$retention_support))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$anc_vl_testing <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  expect_close(result$mtct_pregnant_unsuppressed, 0)
  expect_close(result$mtct_pregnant_suppressed,   900)
})
# ---------------------------------------------------------------------------
# 6.13 NVP duration scaling: partial coverage window reduces effective efficacy
# ---------------------------------------------------------------------------
# WHAT: With 6-week standard NVP (1.5 months) over 18-month breastfeeding:
#         nvp_eff_adjusted = raw_eff × (1.5 / 18) = raw_eff × 0.0833
#       At raw_eff = 1.0, cov = 100%:
#         reduction = 76 × 1.0 × 0.0833 = 6.33 -> 6
#         end_infant_inf = 76 - 6 = 70
# WHY: Directly tests the duration-fraction formula. If the formula ignores
#      duration (ratio stays 1.0), end_infant_inf would wrongly be 0.
# HOW: Override nvp_prophylaxis_duration_months = 1.5 (6-week NVP),
#      bf_duration_months = 18. eff = 1.0, cov = 100%.
# ---------------------------------------------------------------------------
test_that("6-week NVP (1.5m / 18m) scales efficacy to 8.3% of raw", {
  with_hiv_params(modifyList(LIVE_PARAMS_MTCT, list(
    nvp_prophylaxis_duration_months = 1.5,
    bf_duration_months              = 18
  )))
  override_mtct_globals()
  
  ig_new <- intervention_groups
  ig_new$prevention$interventions$infant_prophylaxis$efficacy  <- 1.0
  ig_new$prevention$interventions$infant_prophylaxis$unit_cost <- 0
  with_intervention_groups(list(prevention = ig_new$prevention))
  
  ctx <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                              yield_multipliers = list())
  pops <- calculate_populations(ctx)
  
  interv <- zero_interventions()
  interv$infant_prophylaxis <- 100
  
  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )
  
  # nvp_eff_adjusted = 1.0 × (1.5/18) = 0.0833
  # reduction = 76 × 1.0 × 0.0833 = 6.33 -> 6
  # end_infant_inf = 76 - 6 = 70  (not 0 as with full-duration NVP)
  expect_lte(abs(result$end_infant_infections - 70), 1)
  expect_lte(abs(result$infant_infections_averted - 6), 1)
  # Must be substantially higher than full-duration case (0)
  expect_gt(result$end_infant_infections, 60)
})