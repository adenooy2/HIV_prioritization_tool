# ============================================================================
# UNIT TESTS FOR MORTALITY MODEL
# ============================================================================
# Tests the mortality logic including:
#   - Stage-specific mortality rates
#   - Per-group AHD proportions
#   - Cotrimoxazole reduces base rate for new initiations only
#   - OI management reduces base rate for new initiations only
#   - Cotrimoxazole + OI combined (multiplicative)
#   - AHD package effect gated by CD4 testing
#   - AHD package with zero CD4 → zero effect
#   - Costs: CD4 testing, cotrimoxazole, OI management, AHD package
#   - Deaths averted calculation
#   - Established patients unaffected by cotrimoxazole/OI
# ============================================================================

library(testthat)

source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/Mock-Up_logic_V2.R")

###Do costing tests 15-19
# ============================================================================
# HELPERS
# ============================================================================

# Standard test context (1M population, 5% prevalence)
make_context <- function() {
  list(
    total_population        = 1000000,
    hiv_prevalence          = 0.05,
    new_infections_per_year = 2500,
    percent_diagnosed       = 80,
    percent_on_art          = 75,
    percent_suppressed      = 85,
    aids_deaths_per_year    = 1000,
    birth_rate              = 25,
    prop_pop_male           = 49,
    prop_pop_under_14       = 40
  )
}

# All interventions set to zero
zero_interventions <- function() {
  ints <- list()
  for (g in names(intervention_groups))
    for (k in names(intervention_groups[[g]]$interventions))
      ints[[k]] <- 0
  ints
}

# Helper: calc_deaths formula (mirrors logic file)
calc_deaths_expected <- function(n, base_rate, ahd_rate, prop_ahd) {
  n * ((1 - prop_ahd) * base_rate + prop_ahd * ahd_rate)
}

# Set all mortality rates to zero for isolation
zero_mortality <- function() {
  MORTALITY_RATES <<- list(
    untreated_undiagnosed = 0, new_art_initiations = 0,
    treated = 0, suppressed = 0, ahd = 0,
    prop_ahd = list(
      undiagnosed         = 0.20,
      diagnosed_not_art   = 0.20,
      new_initiations     = 0.20,
      established_treated = 0.00,
      established_supp    = 0.00
    )
  )
}

# Restore production mortality rates
restore_mortality <- function() {
  MORTALITY_RATES <<- list(
    untreated_undiagnosed = 0.10,
    new_art_initiations   = 0.06,
    treated               = 0.008,
    suppressed            = 0.003,
    ahd                   = 0.20,
    prop_ahd = list(
      undiagnosed         = 0.20,
      diagnosed_not_art   = 0.20,
      new_initiations     = 0.20,
      established_treated = 0.00,
      established_supp    = 0.00
    )
  )
}

# Override specific mortality rates for isolation
set_mortality <- function(untreated=0, new_art=0, treated=0,
                          suppressed=0, ahd=0,
                          prop_ahd_undiag=0.20, prop_ahd_diag_not_art=0.20,
                          prop_ahd_new_init=0.20, prop_ahd_est_treated=0,
                          prop_ahd_est_supp=0) {
  MORTALITY_RATES <<- list(
    untreated_undiagnosed = untreated,
    new_art_initiations   = new_art,
    treated               = treated,
    suppressed            = suppressed,
    ahd                   = ahd,
    prop_ahd = list(
      undiagnosed         = prop_ahd_undiag,
      diagnosed_not_art   = prop_ahd_diag_not_art,
      new_initiations     = prop_ahd_new_init,
      established_treated = prop_ahd_est_treated,
      established_supp    = prop_ahd_est_supp
    )
  )
}

# Fixed intervention efficacies for mortality interventions (override loaded params)
set_mortality_intervention_params <- function(cotrix_eff=0.5, oi_eff=0.5,
                                              cd4_eff=1.0, ahd_pkg_eff=0.6,
                                              cotrix_cost=2, oi_cost=3,
                                              cd4_cost=5, ahd_pkg_cost=20) {
  intervention_groups$treatment_monitoring$interventions$cotrimoxazole$efficacy <<- cotrix_eff
  intervention_groups$treatment_monitoring$interventions$cotrimoxazole$unit_cost <<- cotrix_cost
  intervention_groups$treatment_monitoring$interventions$oi_management$efficacy  <<- oi_eff
  intervention_groups$treatment_monitoring$interventions$oi_management$unit_cost <<- oi_cost
  intervention_groups$advanced_disease$interventions$cd4_testing$efficacy        <<- cd4_eff
  intervention_groups$advanced_disease$interventions$cd4_testing$unit_cost       <<- cd4_cost
  intervention_groups$advanced_disease$interventions$ahd_package$efficacy        <<- ahd_pkg_eff
  intervention_groups$advanced_disease$interventions$ahd_package$unit_cost       <<- ahd_pkg_cost
}

ctx  <- make_context()
pops <- calculate_populations(ctx)

# Known cascade groups with zero testing (art_initiations = 0)
# n_undiagnosed       = plhiv - diagnosed       = 50000 - 40000 = 10000
# n_diagnosed_not_art = diagnosed - on_art      = 40000 - 30000 = 10000
# n_new_initiations   = art_initiations         = 0
# n_established_supp  = suppressed              = 25500
# n_established_treated = on_art - suppressed   = 4500
n_undiag        <- pops$plhiv    - pops$diagnosed
n_diag_not_art  <- pops$diagnosed - pops$on_art
n_established_supp     <- pops$suppressed
n_established_treated  <- pops$on_art - pops$suppressed

cat(sprintf("Cascade groups (zero testing):\n"))
cat(sprintf("  n_undiagnosed:        %g\n", n_undiag))
cat(sprintf("  n_diagnosed_not_art:  %g\n", n_diag_not_art))
cat(sprintf("  n_established_treated:%g\n", n_established_treated))
cat(sprintf("  n_established_supp:   %g\n", n_established_supp))

# ============================================================================
# TEST 1: ZERO MORTALITY RATES → ZERO DEATHS
# ============================================================================

test_that("Zero mortality rates produce zero deaths", {
  cat("\n========================================\n")
  cat("TEST 1: Zero Mortality Rates\n")
  cat("========================================\n")

  zero_mortality()
  ints <- zero_interventions()
  out  <- calculate_scenario_outcomes(ctx, ints, pops)

  cat(sprintf("  end_deaths:     %g\n", out$end_deaths))
  cat(sprintf("  deaths_averted: %g\n", out$deaths_averted))

  expect_equal(out$end_deaths,     0, info = "Zero rates → zero deaths")
  expect_equal(out$deaths_averted, 0, info = "Zero rates → zero deaths averted")

  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 2: UNDIAGNOSED  and diagnosed no on ART  MORTALITY RATE
# ============================================================================

test_that("UNtreatedgroup deaths use untreated_undiagnosed rate with AHD blend", {
  cat("\n========================================\n")
  cat("TEST 2: Untreated Groups Mortality\n")
  cat("========================================\n")

  set_mortality(untreated = 0.10, ahd = 0.20,
                prop_ahd_undiag = 0.20, prop_ahd_diag_not_art = 0.2)
  ints <- zero_interventions()
  out  <- calculate_scenario_outcomes(ctx, ints, pops)

  expected_deaths_undiag     <- calc_deaths_expected(n_undiag,       0.10, 0.20, 0.20)
  expected_deaths_diag_not_art <- calc_deaths_expected(n_diag_not_art, 0.10, 0.20, 0.20)
  expected_total <- expected_deaths_undiag + expected_deaths_diag_not_art
  
  cat(sprintf("  Expected deaths (undiagnosed):     %.1f\n", expected_deaths_undiag))
  cat(sprintf("  Expected deaths (diag, not ART):   %.1f\n", expected_deaths_diag_not_art))
  cat(sprintf("  Expected total:                    %.1f\n", expected_total))
  cat(sprintf("  end_deaths: %g\n", out$end_deaths))
  
  expect_equal(out$end_deaths, round(expected_total),
               info = "Deaths = undiagnosed group + diagnosed-not-ART group (same base rate)")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 4: ESTABLISHED TREATED GROUP MORTALITY
# ============================================================================

test_that("Established treated group uses treated rate (prop_ahd = 0)", {
  cat("\n========================================\n")
  cat("TEST 4: Established Treated Group Mortality\n")
  cat("========================================\n")
  
  set_mortality(untreated = 0, new_art = 0, treated = 0.008, suppressed = 0, ahd = 0,
                prop_ahd_est_treated = 0.00)
  ints <- zero_interventions()
  out  <- calculate_scenario_outcomes(ctx, ints, pops)
  
  # prop_ahd = 0 → deaths = n * treated rate only
  expected <- n_established_treated * 0.008
  cat(sprintf("  Expected deaths (established treated): %.1f\n", expected))
  cat(sprintf("  end_deaths: %g\n", out$end_deaths))
  
  expect_equal(out$end_deaths, round(expected),
               info = "Established treated deaths = n * treated rate (no AHD)")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 5: ESTABLISHED SUPPRESSED GROUP MORTALITY
# ============================================================================

test_that("Established suppressed group uses suppressed rate (prop_ahd = 0)", {
  cat("\n========================================\n")
  cat("TEST 5: Established Suppressed Group Mortality\n")
  cat("========================================\n")
  
  set_mortality(untreated = 0, new_art = 0, treated = 0, suppressed = 0.003, ahd = 0,
                prop_ahd_est_supp = 0.00)
  ints <- zero_interventions()
  out  <- calculate_scenario_outcomes(ctx, ints, pops)
  
  expected <- n_established_supp * 0.003
  cat(sprintf("  Expected deaths (established suppressed): %.1f\n", expected))
  cat(sprintf("  end_deaths: %g\n", out$end_deaths))
  
  expect_equal(out$end_deaths, round(expected),
               info = "Established suppressed deaths = n * suppressed rate (no AHD)")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 6: PROP_AHD = 0 FOR ESTABLISHED GROUPS → AHD RATE HAS NO EFFECT
# ============================================================================

test_that("prop_ahd=0 for established groups: AHD rate changes have no effect", {
  cat("\n========================================\n")
  cat("TEST 6: prop_ahd=0 → AHD Rate Irrelevant for Established Groups\n")
  cat("========================================\n")
  
  set_mortality(untreated=0, new_art=0, treated=0.008, suppressed=0.003, ahd=0.20,
                prop_ahd_undiag=0, prop_ahd_diag_not_art=0,
                prop_ahd_est_treated=0, prop_ahd_est_supp=0)
  ints <- zero_interventions()
  out_low_ahd <- calculate_scenario_outcomes(ctx, ints, pops)
  
  set_mortality(untreated=0, new_art=0, treated=0.008, suppressed=0.003, ahd=0.99,
                prop_ahd_undiag=0, prop_ahd_diag_not_art=0,
                prop_ahd_est_treated=0, prop_ahd_est_supp=0)
  out_high_ahd <- calculate_scenario_outcomes(ctx, ints, pops)
  
  cat(sprintf("  Deaths (ahd=0.20): %g\n", out_low_ahd$end_deaths))
  cat(sprintf("  Deaths (ahd=0.99): %g\n", out_high_ahd$end_deaths))
  
  expect_equal(out_low_ahd$end_deaths, out_high_ahd$end_deaths,
               info = "AHD rate irrelevant when prop_ahd=0")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 7: COTRIMOXAZOLE REDUCES BASE RATE FOR NEW INITIATIONS
# ============================================================================

test_that("Cotrimoxazole reduces base mortality rate for new initiations", {
  cat("\n========================================\n")
  cat("TEST 7: Cotrimoxazole Effect on New Initiations\n")
  cat("========================================\n")
  
  set_mortality_intervention_params(cotrix_eff = 0.50, cotrix_cost = 2)
  
  # Only new_art_initiations rate, no AHD (to isolate cotrimoxazole effect on base rate)
  set_mortality(new_art = 0.10, ahd = 0,
                prop_ahd_new_init = 0.20)
  
  # Use fixed testing to get predictable art_initiations
  ints_no_cotrix <- zero_interventions()
  ints_no_cotrix$test_facility_general <- 50000
  
  ints_with_cotrix <- ints_no_cotrix
  ints_with_cotrix$cotrimoxazole <- 100  # 100% coverage
  
  out_no   <- calculate_scenario_outcomes(ctx, ints_no_cotrix,   pops)
  out_with <- calculate_scenario_outcomes(ctx, ints_with_cotrix, pops)
  
  art_init <- out_no$art_initiations  # same in both (mortality doesn't affect ART)
  
  # With ahd=0: deaths_new = art_init * (1 - 0.20) * eff_base_rate
  # Without cotrix: deaths_new = art_init * 0.80 * 0.10
  # With cotrix 100% coverage, efficacy 0.50: eff_base = 0.10 * 0.50 = 0.05
  #   deaths_new = art_init * 0.80 * 0.05
  expected_no   <- round(art_init * 0.80 * 0.10)
  expected_with <- round(art_init * 0.80 * 0.05)
  
  cat(sprintf("  art_initiations: %g\n", art_init))
  cat(sprintf("  Expected deaths (no cotrix):   %g\n", expected_no))
  cat(sprintf("  Actual  deaths (no cotrix):    %g\n", out_no$end_deaths))
  cat(sprintf("  Expected deaths (with cotrix): %g\n", expected_with))
  cat(sprintf("  Actual  deaths (with cotrix):  %g\n", out_with$end_deaths))
  
  expect_equal(out_no$end_deaths,   expected_no,   info = "Deaths without cotrimoxazole")
  expect_equal(out_with$end_deaths, expected_with, info = "Deaths with cotrimoxazole")
  expect_lt(out_with$end_deaths, out_no$end_deaths ) #"Cotrimoxazole should reduce deaths"
  expect_equal(out_with$deaths_averted,
               round(art_init * 0.80 * 0.10 * 0.50))#"Deaths averted = coverage × efficacy × unadjusted deaths")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 8: OI MANAGEMENT REDUCES BASE RATE FOR NEW INITIATIONS
# ============================================================================

test_that("OI management reduces base mortality rate for new initiations", {
  cat("\n========================================\n")
  cat("TEST 8: OI Management Effect on New Initiations\n")
  cat("========================================\n")
  
  set_mortality_intervention_params(oi_eff = 0.40, oi_cost = 3)
  set_mortality(new_art = 0.10, ahd = 0, prop_ahd_new_init = 0.20)
  
  ints_no_oi <- zero_interventions()
  ints_no_oi$test_facility_general <- 50000
  
  ints_with_oi <- ints_no_oi
  ints_with_oi$oi_management <- 100  # 100% coverage
  
  out_no   <- calculate_scenario_outcomes(ctx, ints_no_oi,   pops)
  out_with <- calculate_scenario_outcomes(ctx, ints_with_oi, pops)
  
  art_init <- out_no$art_initiations
  
  # eff_base = 0.10 * (1 - 0.40) = 0.06
  expected_no   <- round(art_init * 0.80 * 0.10)
  expected_with <- round(art_init * 0.80 * 0.06)
  
  cat(sprintf("  art_initiations: %g\n", art_init))
  cat(sprintf("  Expected deaths (no OI):   %g\n", expected_no))
  cat(sprintf("  Actual  deaths (no OI):    %g\n", out_no$end_deaths))
  cat(sprintf("  Expected deaths (with OI): %g\n", expected_with))
  cat(sprintf("  Actual  deaths (with OI):  %g\n", out_with$end_deaths))
  
  expect_equal(out_no$end_deaths,   expected_no,   info = "Deaths without OI management")
  expect_equal(out_with$end_deaths, expected_with, info = "Deaths with OI management")
  expect_lt(out_with$end_deaths, out_no$end_deaths)#"OI management should reduce deaths")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 9: COTRIMOXAZOLE + OI MANAGEMENT COMBINED (MULTIPLICATIVE)
# ============================================================================

test_that("Cotrimoxazole and OI management effects are multiplicative", {
  cat("\n========================================\n")
  cat("TEST 9: Cotrimoxazole + OI Combined (Multiplicative)\n")
  cat("========================================\n")
  
  cotrix_eff <- 0.50
  oi_eff     <- 0.40
  set_mortality_intervention_params(cotrix_eff = cotrix_eff, oi_eff = oi_eff)
  set_mortality(new_art = 0.10, ahd = 0, prop_ahd_new_init = 0.20)
  
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 50000
  out_none <- calculate_scenario_outcomes(ctx, ints_base, pops)
  art_init <- out_none$art_initiations
  
  # ── 100% coverage both ───────────────────────────────────────────────────
  ints_both_100 <- ints_base
  ints_both_100$cotrimoxazole <- 100
  ints_both_100$oi_management <- 100
  out_both_100 <- calculate_scenario_outcomes(ctx, ints_both_100, pops)
  
  # eff_base = 0.10 * (1-0.50) * (1-0.40) = 0.03
  expected_100 <- round(art_init * 0.80 * 0.03)
  cat(sprintf("  art_initiations: %g\n", art_init))
  cat(sprintf("  Expected deaths (100%% both): %g\n", expected_100))
  cat(sprintf("  Actual  deaths (100%% both):  %g\n", out_both_100$end_deaths))
  
  expect_equal(out_both_100$end_deaths, expected_100,
               info = "100% coverage: eff_base = base * (1-cotrix_eff) * (1-oi_eff)")
  
  # ── Partial coverage: 60% cotrix, 80% OI ────────────────────────────────
  cotrix_cov <- 0.60
  oi_cov     <- 0.80
  
  ints_partial <- ints_base
  ints_partial$cotrimoxazole <- cotrix_cov * 100
  ints_partial$oi_management <- oi_cov     * 100
  out_partial <- calculate_scenario_outcomes(ctx, ints_partial, pops)
  
  # eff_base = 0.10 * (1 - 0.60*0.50) * (1 - 0.80*0.40)
  #          = 0.10 * (1 - 0.30) * (1 - 0.32)
  #          = 0.10 * 0.70 * 0.68 = 0.0476
  eff_base_partial <- 0.10 * (1 - cotrix_cov * cotrix_eff) * (1 - oi_cov * oi_eff)
  expected_partial <- round(art_init * 0.80 * eff_base_partial)
  
  cat(sprintf("  Expected deaths (60%% cotrix, 80%% OI): %g\n", expected_partial))
  cat(sprintf("  Actual  deaths (60%% cotrix, 80%% OI):  %g\n", out_partial$end_deaths))
  
  expect_equal(out_partial$end_deaths, expected_partial,
               info = "Partial coverage: eff_base = base * (1 - cov*eff) for each intervention")
  
  # ── Partial should be between no-intervention and full coverage ──────────
  expect_lt(out_partial$end_deaths, out_none$end_deaths)#"Partial coverage reduces deaths vs no intervention")
  expect_gt(out_partial$end_deaths, out_both_100$end_deaths)#"Partial coverage reduces deaths less than full coverage")
  
  # ── Scaling: doubling cotrix coverage from 30% to 60% ───────────────────
  ints_cotrix_30 <- ints_base
  ints_cotrix_30$cotrimoxazole <- 30
  ints_cotrix_30$oi_management <- 0
  out_cotrix_30 <- calculate_scenario_outcomes(ctx, ints_cotrix_30, pops)
  
  ints_cotrix_60 <- ints_base
  ints_cotrix_60$cotrimoxazole <- 60
  ints_cotrix_60$oi_management <- 0
  out_cotrix_60 <- calculate_scenario_outcomes(ctx, ints_cotrix_60, pops)
  
  eff_base_30 <- 0.10 * (1 - 0.30 * cotrix_eff)
  eff_base_60 <- 0.10 * (1 - 0.60 * cotrix_eff)
  expected_30 <- round(art_init * 0.80 * eff_base_30)
  expected_60 <- round(art_init * 0.80 * eff_base_60)
  
  cat(sprintf("  Expected deaths (30%% cotrix): %g\n", expected_30))
  cat(sprintf("  Actual  deaths (30%% cotrix):  %g\n", out_cotrix_30$end_deaths))
  cat(sprintf("  Expected deaths (60%% cotrix): %g\n", expected_60))
  cat(sprintf("  Actual  deaths (60%% cotrix):  %g\n", out_cotrix_60$end_deaths))
  
  expect_equal(out_cotrix_30$end_deaths, expected_30,
               info = "30% cotrix coverage: eff_base = base * (1 - 0.30*efficacy)")
  expect_equal(out_cotrix_60$end_deaths, expected_60,
               info = "60% cotrix coverage: eff_base = base * (1 - 0.60*efficacy)")
  expect_lt(out_cotrix_60$end_deaths, out_cotrix_30$end_deaths)# "Higher cotrix coverage → fewer deaths")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 10: COTRIMOXAZOLE AND OI DO NOT AFFECT ESTABLISHED PATIENTS
# ============================================================================

test_that("Cotrimoxazole and OI management do not affect established patient deaths", {
  cat("\n========================================\n")
  cat("TEST 10: Cotrimoxazole/OI Only Affect New Initiations\n")
  cat("========================================\n")
  
  set_mortality_intervention_params(cotrix_eff=0.99, oi_eff=0.99)
  
  # Only established group rates, zero new_art rate
  set_mortality(treated=0.008, suppressed=0.003,
                prop_ahd_est_treated=0, prop_ahd_est_supp=0)
  
  ints_no_int <- zero_interventions()  # zero testing → zero art_initiations
  ints_with   <- zero_interventions()
  ints_with$cotrimoxazole <- 100
  ints_with$oi_management <- 100
  
  out_no   <- calculate_scenario_outcomes(ctx, ints_no_int, pops)
  out_with <- calculate_scenario_outcomes(ctx, ints_with,   pops)
  
  cat(sprintf("  Deaths without cotrix/OI: %g\n", out_no$end_deaths))
  cat(sprintf("  Deaths with cotrix/OI:    %g\n", out_with$end_deaths))
  
  expect_equal(out_no$end_deaths, out_with$end_deaths,
               info = "Cotrimoxazole/OI have no effect on established patients")
  expect_equal(out_with$deaths_averted, 0,
               info = "Zero deaths averted when only established groups present")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 11: AHD PACKAGE WITH ZERO CD4 TESTING → ZERO EFFECT
# ============================================================================

test_that("AHD package with zero CD4 testing has no mortality effect", {
  cat("\n========================================\n")
  cat("TEST 11: AHD Package Gated by CD4 Testing\n")
  cat("========================================\n")
  
  set_mortality_intervention_params(ahd_pkg_eff=0.80, cd4_cost=5, ahd_pkg_cost=20)
  set_mortality(new_art=0.10, ahd=0.20, prop_ahd_new_init=0.20)
  
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 50000
  
  # AHD package at 100% but NO CD4 testing
  ints_ahd_no_cd4 <- ints_base
  ints_ahd_no_cd4$ahd_package  <- 100
  ints_ahd_no_cd4$cd4_testing  <- 0
  
  out_base       <- calculate_scenario_outcomes(ctx, ints_base,       pops)
  out_ahd_no_cd4 <- calculate_scenario_outcomes(ctx, ints_ahd_no_cd4, pops)
  
  cat(sprintf("  Deaths (base):          %g\n", out_base$end_deaths))
  cat(sprintf("  Deaths (AHD, no CD4):   %g\n", out_ahd_no_cd4$end_deaths))
  cat(sprintf("  Deaths averted (no CD4):%g\n", out_ahd_no_cd4$deaths_averted))
  
  expect_equal(out_base$end_deaths, out_ahd_no_cd4$end_deaths,
               info = "AHD package without CD4 testing has zero mortality effect")
  expect_equal(out_ahd_no_cd4$deaths_averted, 0,
               info = "No deaths averted without CD4 testing")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 12: AHD PACKAGE WITH CD4 TESTING REDUCES AHD MORTALITY
# ============================================================================

test_that("AHD package with CD4 testing reduces AHD mortality for new initiations", {
  cat("\n========================================\n")
  cat("TEST 12: AHD Package + CD4 Testing Effect\n")
  cat("========================================\n")
  
  ahd_pkg_eff <- 0.60
  set_mortality_intervention_params(ahd_pkg_eff = ahd_pkg_eff, cd4_cost=5, ahd_pkg_cost=20)
  
  # Only AHD rate matters for new initiations; zero all other groups to isolate
  set_mortality(new_art=0, ahd=0.20, prop_ahd_new_init=0.20,
                prop_ahd_undiag=0, prop_ahd_diag_not_art=0)
  
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 50000
  
  ints_with_cd4_ahd <- ints_base
  ints_with_cd4_ahd$cd4_testing <- 100  # 100% CD4 coverage
  ints_with_cd4_ahd$ahd_package <- 100  # 100% AHD package coverage
  
  out_base     <- calculate_scenario_outcomes(ctx, ints_base,        pops)
  out_with     <- calculate_scenario_outcomes(ctx, ints_with_cd4_ahd, pops)
  
  art_init <- out_base$art_initiations
  
  # With 100% CD4, 100% AHD pkg, efficacy 0.60:
  # ahd_pkg_eff_reduction = cd4_frac(1.0) * ahd_cov_frac(1.0) * efficacy(0.60) = 0.60
  # eff_ahd_rate = 0.20 * (1 - 0.60) = 0.08
  # deaths_new = art_init * (0.80*0 + 0.20*0.08) = art_init * 0.016
  expected_with <- round(art_init * 0.20 * 0.08)
  
  # Without interventions:
  # deaths_new = art_init * 0.20 * 0.20 = art_init * 0.04
  expected_base <- round(art_init * 0.20 * 0.20)
  
  cat(sprintf("  art_initiations: %g\n", art_init))
  cat(sprintf("  Expected deaths (base):        %g\n", expected_base))
  cat(sprintf("  Actual  deaths (base):         %g\n", out_base$end_deaths))
  cat(sprintf("  Expected deaths (CD4+AHD pkg): %g\n", expected_with))
  cat(sprintf("  Actual  deaths (CD4+AHD pkg):  %g\n", out_with$end_deaths))
  
  expect_equal(out_base$end_deaths, expected_base,
               info = "Baseline AHD deaths correct")
  expect_equal(out_with$end_deaths, expected_with,
               info = "AHD deaths reduced by package when CD4 tested")
  expect_lt(out_with$end_deaths, out_base$end_deaths) #"AHD package + CD4 testing reduces deaths")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 13: AHD PACKAGE EFFECT SCALES WITH CD4 COVERAGE
# ============================================================================

test_that("AHD package effect scales proportionally with CD4 coverage", {
  cat("\n========================================\n")
  cat("TEST 13: AHD Effect Scales with CD4 Coverage\n")
  cat("========================================\n")
  
  set_mortality_intervention_params(ahd_pkg_eff=0.60)
  set_mortality(new_art=0, ahd=0.20, prop_ahd_new_init=0.20)
  
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 50000
  
  ints_cd4_50 <- ints_base
  ints_cd4_50$cd4_testing  <- 50   # 50% CD4 coverage
  ints_cd4_50$ahd_package  <- 100
  
  ints_cd4_100 <- ints_base
  ints_cd4_100$cd4_testing  <- 100  # 100% CD4 coverage
  ints_cd4_100$ahd_package  <- 100
  
  out_cd4_50  <- calculate_scenario_outcomes(ctx, ints_cd4_50,  pops)
  out_cd4_100 <- calculate_scenario_outcomes(ctx, ints_cd4_100, pops)
  
  cat(sprintf("  deaths_averted (50%% CD4):  %g\n", out_cd4_50$deaths_averted))
  cat(sprintf("  deaths_averted (100%% CD4): %g\n", out_cd4_100$deaths_averted))
  cat(sprintf("  Ratio: %.2f (expected ~0.50)\n",
              out_cd4_50$deaths_averted / out_cd4_100$deaths_averted))
  
  expect_lt(out_cd4_50$deaths_averted, out_cd4_100$deaths_averted)# "Higher CD4 coverage → more deaths averted")
  # 50% CD4 should give ~50% of the deaths averted vs 100% CD4
  ratio <- out_cd4_50$deaths_averted / out_cd4_100$deaths_averted
  expect_true(abs(ratio - 0.50) < 0.05,
              info = "Deaths averted scales linearly with CD4 coverage")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 14: DEATHS AVERTED = 0 WITH ZERO MORTALITY INTERVENTIONS
# ============================================================================

test_that("Deaths averted is zero when no mortality interventions applied", {
  cat("\n========================================\n")
  cat("TEST 14: Zero Deaths Averted Without Interventions\n")
  cat("========================================\n")
  
  restore_mortality()
  
  ints <- zero_interventions()
  ints$test_facility_general <- 50000  # testing only, no mortality interventions
  
  out <- calculate_scenario_outcomes(ctx, ints, pops)
  
  cat(sprintf("  deaths_averted: %g\n", out$deaths_averted))
  
  expect_equal(out$deaths_averted, 0,
               info = "No mortality interventions → zero deaths averted")
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 15: COST OF CD4 TESTING
# ============================================================================

test_that("CD4 testing cost = art_initiations × coverage × unit_cost", {
  cat("\n========================================\n")
  cat("TEST 15: CD4 Testing Cost\n")
  cat("========================================\n")
  
  cd4_unit_cost <- 7
  set_mortality_intervention_params(cd4_cost = cd4_unit_cost)
  zero_mortality()
  
  ints_testing_only <- zero_interventions()
  ints_testing_only$test_facility_general <- 50000
  
  out_no_cd4 <- calculate_scenario_outcomes(ctx, ints_testing_only, pops)
  art_init   <- out_no_cd4$art_initiations
  base_cost  <- out_no_cd4$total_intervention_cost
  
  ints_with_cd4 <- ints_testing_only
  ints_with_cd4$cd4_testing <- 80  # 80% coverage
  
  out_with_cd4 <- calculate_scenario_outcomes(ctx, ints_with_cd4, pops)
  
  expected_cd4_cost <- round(art_init * 0.80 * cd4_unit_cost)
  actual_cd4_cost   <- out_with_cd4$total_intervention_cost - base_cost
  
  cat(sprintf("  art_initiations: %g\n", art_init))
  cat(sprintf("  Expected CD4 cost: %g\n", expected_cd4_cost))
  cat(sprintf("  Actual CD4 cost:   %g\n", actual_cd4_cost))
  
  expect_equal(actual_cd4_cost, expected_cd4_cost,
               info = "CD4 cost = art_init × coverage × unit_cost")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 16: COST OF AHD PACKAGE (GATED BY CD4 DIAGNOSIS)
# ============================================================================

test_that("AHD package cost applies only to those diagnosed with AHD via CD4", {
  cat("\n========================================\n")
  cat("TEST 16: AHD Package Cost Gated by CD4\n")
  cat("========================================\n")
  
  cd4_cost    <- 5
  ahd_cost    <- 25
  prop_ahd_ni <- MORTALITY_RATES$prop_ahd$new_initiations  # 0.20
  set_mortality_intervention_params(cd4_cost = cd4_cost, ahd_pkg_cost = ahd_cost)
  zero_mortality()
  
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 50000
  
  out_base <- calculate_scenario_outcomes(ctx, ints_base, pops)
  art_init <- out_base$art_initiations
  base_cost <- out_base$total_intervention_cost
  
  # 100% CD4, 100% AHD package
  ints_full <- ints_base
  ints_full$cd4_testing <- 100
  ints_full$ahd_package <- 100
  
  out_full <- calculate_scenario_outcomes(ctx, ints_full, pops)
  
  n_cd4_tested   <- art_init * 1.00
  n_ahd_diagnosed <- n_cd4_tested * prop_ahd_ni
  n_ahd_pkg      <- n_ahd_diagnosed * 1.00
  
  expected_cd4_cost <- round(n_cd4_tested * cd4_cost)
  expected_ahd_cost <- round(n_ahd_pkg * ahd_cost)
  expected_add_cost <- expected_cd4_cost + expected_ahd_cost
  
  actual_add_cost <- out_full$total_intervention_cost - base_cost
  
  cat(sprintf("  art_initiations:    %g\n", art_init))
  cat(sprintf("  n_ahd_diagnosed:    %.1f\n", n_ahd_diagnosed))
  cat(sprintf("  Expected add cost:  %g\n", expected_add_cost))
  cat(sprintf("  Actual   add cost:  %g\n", actual_add_cost))
  
  expect_equal(actual_add_cost, expected_add_cost,
               info = "AHD cost = (cd4_tested × cd4_cost) + (ahd_diagnosed × ahd_cost)")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 17: AHD PACKAGE COST SCALES WITH CD4 COVERAGE
# ============================================================================

test_that("AHD package cost scales with CD4 coverage (fewer diagnosed → lower cost)", {
  cat("\n========================================\n")
  cat("TEST 17: AHD Package Cost Scales with CD4 Coverage\n")
  cat("========================================\n")
  
  set_mortality_intervention_params(cd4_cost=5, ahd_pkg_cost=25)
  zero_mortality()
  
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 50000
  
  ints_cd4_50 <- ints_base
  ints_cd4_50$cd4_testing <- 50
  ints_cd4_50$ahd_package <- 100
  
  ints_cd4_100 <- ints_base
  ints_cd4_100$cd4_testing <- 100
  ints_cd4_100$ahd_package <- 100
  
  out_cd4_50  <- calculate_scenario_outcomes(ctx, ints_cd4_50,  pops)
  out_cd4_100 <- calculate_scenario_outcomes(ctx, ints_cd4_100, pops)
  
  cat(sprintf("  Cost (50%% CD4):  %g\n", out_cd4_50$total_intervention_cost))
  cat(sprintf("  Cost (100%% CD4): %g\n", out_cd4_100$total_intervention_cost))
  
  expect_lt(out_cd4_50$total_intervention_cost, out_cd4_100$total_intervention_cost,
            info = "Lower CD4 coverage → fewer AHD diagnosed → lower AHD package cost")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 18: COTRIMOXAZOLE COST
# ============================================================================

test_that("Cotrimoxazole cost = art_initiations × coverage × unit_cost", {
  cat("\n========================================\n")
  cat("TEST 18: Cotrimoxazole Cost\n")
  cat("========================================\n")
  
  cotrix_cost <- 2
  set_mortality_intervention_params(cotrix_cost = cotrix_cost)
  zero_mortality()
  
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 50000
  out_base  <- calculate_scenario_outcomes(ctx, ints_base, pops)
  art_init  <- out_base$art_initiations
  base_cost <- out_base$total_intervention_cost
  
  ints_cotrix <- ints_base
  ints_cotrix$cotrimoxazole <- 60  # 60% coverage
  out_cotrix  <- calculate_scenario_outcomes(ctx, ints_cotrix, pops)
  
  expected_add_cost <- round(art_init * 0.60 * cotrix_cost)
  actual_add_cost   <- out_cotrix$total_intervention_cost - base_cost
  
  cat(sprintf("  art_initiations:   %g\n", art_init))
  cat(sprintf("  Expected add cost: %g\n", expected_add_cost))
  cat(sprintf("  Actual   add cost: %g\n", actual_add_cost))
  
  expect_equal(actual_add_cost, expected_add_cost,
               info = "Cotrimoxazole cost = art_init × coverage × unit_cost")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 19: OI MANAGEMENT COST
# ============================================================================

test_that("OI management cost = art_initiations × coverage × unit_cost", {
  cat("\n========================================\n")
  cat("TEST 19: OI Management Cost\n")
  cat("========================================\n")
  
  oi_cost <- 4
  set_mortality_intervention_params(oi_cost = oi_cost)
  zero_mortality()
  
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 50000
  out_base  <- calculate_scenario_outcomes(ctx, ints_base, pops)
  art_init  <- out_base$art_initiations
  base_cost <- out_base$total_intervention_cost
  
  ints_oi <- ints_base
  ints_oi$oi_management <- 75  # 75% coverage
  out_oi  <- calculate_scenario_outcomes(ctx, ints_oi, pops)
  
  expected_add_cost <- round(art_init * 0.75 * oi_cost)
  actual_add_cost   <- out_oi$total_intervention_cost - base_cost
  
  cat(sprintf("  art_initiations:   %g\n", art_init))
  cat(sprintf("  Expected add cost: %g\n", expected_add_cost))
  cat(sprintf("  Actual   add cost: %g\n", actual_add_cost))
  
  expect_equal(actual_add_cost, expected_add_cost,
               info = "OI cost = art_init × coverage × unit_cost")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 20: MORE TESTING → MORE ART INITIATIONS → MORE DEATHS AVERTED
# ============================================================================

test_that("More testing produces more ART initiations and more deaths averted", {
  cat("\n========================================\n")
  cat("TEST 20: Scale-up Testing → More Deaths Averted\n")
  cat("========================================\n")
  
  restore_mortality()
  set_mortality_intervention_params(cotrix_eff=0.50, oi_eff=0.40,
                                    cd4_cost=5, ahd_pkg_eff=0.60)
  
  ints_low <- zero_interventions()
  ints_low$test_facility_general <- 10000
  ints_low$cotrimoxazole         <- 80
  ints_low$oi_management         <- 80
  ints_low$cd4_testing           <- 80
  ints_low$ahd_package           <- 80
  
  ints_high <- ints_low
  ints_high$test_facility_general <- 50000
  
  out_low  <- calculate_scenario_outcomes(ctx, ints_low,  pops)
  out_high <- calculate_scenario_outcomes(ctx, ints_high, pops)
  
  cat(sprintf("  art_initiations (low):  %g\n", out_low$art_initiations))
  cat(sprintf("  art_initiations (high): %g\n", out_high$art_initiations))
  cat(sprintf("  deaths_averted (low):   %g\n", out_low$deaths_averted))
  cat(sprintf("  deaths_averted (high):  %g\n", out_high$deaths_averted))
  
  expect_gt(out_high$art_initiations, out_low$art_initiations) #"More testing → more ART initiations")
  expect_gt(out_high$deaths_averted, out_low$deaths_averted)# "More ART initiations → more deaths averted from mortality interventions")
  
  restore_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("ALL MORTALITY TESTS COMPLETED\n")
cat("========================================\n")
cat("✓ TEST 1:  Zero mortality rates → zero deaths\n")
cat("✓ TEST 2:  untreated group mortality rate\n")
cat("✓ TEST 4:  Established treated group mortality\n")
cat("✓ TEST 5:  Established suppressed group mortality\n")
cat("✓ TEST 6:  prop_ahd=0 → AHD rate irrelevant\n")
cat("✓ TEST 7:  Cotrimoxazole reduces base rate for new initiations\n")
cat("✓ TEST 8:  OI management reduces base rate for new initiations\n")
cat("✓ TEST 9:  Cotrimoxazole + OI combined (multiplicative)\n")
cat("✓ TEST 10: Cotrimoxazole/OI do not affect established patients\n")
cat("✓ TEST 11: AHD package with zero CD4 → zero effect\n")
cat("✓ TEST 12: AHD package + CD4 reduces AHD mortality\n")
cat("✓ TEST 13: AHD effect scales with CD4 coverage\n")
cat("✓ TEST 14: Zero deaths averted without mortality interventions\n")
cat("✓ TEST 15: CD4 testing cost\n")
cat("✓ TEST 16: AHD package cost gated by CD4 diagnosis\n")
cat("✓ TEST 17: AHD package cost scales with CD4 coverage\n")
cat("✓ TEST 18: Cotrimoxazole cost\n")
cat("✓ TEST 19: OI management cost\n")
cat("✓ TEST 20: More testing → more deaths averted\n")






