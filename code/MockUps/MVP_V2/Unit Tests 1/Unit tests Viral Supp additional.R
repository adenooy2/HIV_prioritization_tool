# ============================================================================
# UNIT TESTS FOR VIRAL SUPPRESSION (VL MONITORING) LOGIC
# ============================================================================
# Tests that changes in viral load testing coverage and modalities are correctly
# handled by the model. Covers:
#   - Zero VL testing: no change in suppressed count
#   - VL testing increases suppression among unsuppressed patients on ART
#   - Doubling coverage doubles additional suppressed
#   - Scaling down coverage scales down additional suppressed proportionally
#   - Higher VL monitoring efficacy produces proportionally more suppression
#   - Cascade consistency: suppressed <= on_art <= diagnosed <= PLHIV
#   - VL monitoring does not affect testing, diagnoses, or ART initiations
# ============================================================================

library(testthat)

source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/Mock-Up_logic_V2.R")

# ============================================================================
# HELPERS
# ============================================================================

# Standard test context matching Test_Framework_Description.docx:
#   1,000,000 population | 5% prevalence | 80% diagnosed | 75% on ART
#   85% suppressed | 5,000 new infections | 1,000 AIDS deaths
#   birth rate 24 | 49% male | 40% under 15
make_context <- function() {
  list(
    total_population        = 1000000,
    hiv_prevalence          = 0.05,
    new_infections_per_year = 5000,
    percent_diagnosed       = 80,
    percent_on_art          = 75,
    percent_suppressed      = 85,
    aids_deaths_per_year    = 1000,
    birth_rate              = 24,
    prop_pop_male           = 49,
    prop_pop_under_14       = 40
  )
}

# All interventions zeroed
zero_interventions <- function() {
  ints <- list()
  for (g in names(intervention_groups))
    for (k in names(intervention_groups[[g]]$interventions))
      ints[[k]] <- 0
  ints
}

# Zero out mortality so cascade arithmetic is not confounded
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

# Zero LTFU so suppressed_ltfu does not obscure the VL effect
zero_ltfu_rates <- function() {
  ANNUAL_LTFU_RATE_SUPPRESSED   <<- 0
  ANNUAL_LTFU_RATE_UNSUPPRESSED <<- 0
}

restore_ltfu_rates <- function() {
  ANNUAL_LTFU_RATE_SUPPRESSED   <<- 0.05
  ANNUAL_LTFU_RATE_UNSUPPRESSED <<- 0.15
}

# Override VL monitoring efficacy and cost
set_vl_params <- function(efficacy = 0.20, unit_cost = 15) {
  intervention_groups$treatment_monitoring$interventions$vl_monitoring_routine$efficacy  <<- efficacy
  intervention_groups$treatment_monitoring$interventions$vl_monitoring_routine$unit_cost <<- unit_cost
}

# ============================================================================
# SETUP
# ============================================================================

ctx  <- make_context()
zero_mortality()
zero_ltfu_rates()         # ← must precede calculate_populations() so pops$ltfu_new_suppressed = 0
set_vl_params(efficacy = 0.20)
pops <- calculate_populations(ctx)  # ← built AFTER rates are zeroed

# Pre-computed baseline values (zero interventions, zero LTFU, zero mortality):
#   on_art            = 50,000 * 0.80 * 0.75 = 30,000
#   suppressed        = 30,000 * 0.85         = 25,500
#   unsuppressed      = 30,000 - 25,500        = 4,500
#   unsuppressed_rate = 1 - 0.85               = 0.15
#
# With 60% VL coverage and 20% efficacy:
#   number_reached            = 30,000 * 0.60       = 18,000
#   expected_additional_supp  = 18,000 * 0.15 * 0.20 = 540
#   expected_end_suppressed   = 25,500 + 540         = 26,040

cat(sprintf("\nPopulation baseline values (zero interventions):\n"))
cat(sprintf("  on_art:          %g\n", pops$on_art))
cat(sprintf("  suppressed:      %g\n", pops$suppressed))
cat(sprintf("  unsuppressed:    %g\n", pops$unsuppressed))
cat(sprintf("  PLHIV:           %g\n", pops$plhiv))


# ============================================================================
# TEST 1: ZERO VL TESTING → NO CHANGE IN SUPPRESSION
# ============================================================================

test_that("Zero VL testing produces no additional suppressed and end_suppressed equals starting suppressed", {
  cat("\n========================================\n")
  cat("TEST 1: Zero VL Testing → No Change in Suppression\n")
  cat("========================================\n")
  
  ints_zero <- zero_interventions()   # vl_monitoring_routine = 0
  
  out <- calculate_scenario_outcomes(ctx, ints_zero, pops)
  
  cat(sprintf("  additional_suppressed: %g\n", out$additional_suppressed))
  cat(sprintf("  end_suppressed:        %g\n", out$end_suppressed))
  cat(sprintf("  start_suppressed:      %g\n", pops$suppressed))
  
  expect_equal(out$additional_suppressed, 0,
               info = "Zero VL coverage must produce zero additional suppressed")
  
  # No tolerance — with zero LTFU and zero mortality, end_suppressed must equal
  # start_suppressed exactly.
  expect_equal(out$end_suppressed, as.integer(round(pops$suppressed)),
               info = "end_suppressed must equal start_suppressed when no VL intervention is applied")
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 2: VL TESTING IMPROVES SUPPRESSION — EXPECTED VALUE CHECK
# ============================================================================

test_that("VL testing at 60% coverage with 20% efficacy produces expected additional suppressed", {
  cat("\n========================================\n")
  cat("TEST 2: VL Testing Improves Suppression — Expected Value Check\n")
  cat("========================================\n")
  
  set_vl_params(efficacy = 0.20)
  
  coverage_pct          <- 60
  unsuppressed_rate     <- 1 - ctx$percent_suppressed / 100   # 0.15
  number_reached        <- pops$on_art * (coverage_pct / 100) # 18,000
  expected_additional   <- number_reached * unsuppressed_rate * 0.20  # 540
  expected_end_supp     <- pops$suppressed + expected_additional      # 26,040
  
  ints <- zero_interventions()
  ints$vl_monitoring_routine <- coverage_pct
  
  out <- calculate_scenario_outcomes(ctx, ints, pops)
  
  cat(sprintf("  number_reached (expected):        %g\n", number_reached))
  cat(sprintf("  expected_additional_suppressed:   %g\n", expected_additional))
  cat(sprintf("  actual additional_suppressed:     %g\n", out$additional_suppressed))
  cat(sprintf("  expected_end_suppressed:          %g\n", expected_end_supp))
  cat(sprintf("  actual end_suppressed:            %g\n", out$end_suppressed))
  
  expect_equal(out$additional_suppressed, round(expected_additional),
               info = "additional_suppressed must equal on_art * coverage * unsuppressed_rate * efficacy")
  
  expect_equal(out$end_suppressed, round(expected_end_supp),
               info = "end_suppressed must equal start_suppressed + additional_suppressed")
  
  expect_gt(out$end_suppressed, round(pops$suppressed),
            label = "end_suppressed must exceed starting suppressed when VL intervention is active")
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 3: DOUBLING VL COVERAGE DOUBLES ADDITIONAL SUPPRESSED
# ============================================================================

test_that("Doubling VL coverage from 40% to 80% doubles additional suppressed", {
  cat("\n========================================\n")
  cat("TEST 3: Doubled Coverage → Double Additional Suppressed\n")
  cat("========================================\n")
  
  set_vl_params(efficacy = 0.20)
  
  ints_base   <- zero_interventions(); ints_base$vl_monitoring_routine   <- 40
  ints_double <- zero_interventions(); ints_double$vl_monitoring_routine <- 80
  
  out_base   <- calculate_scenario_outcomes(ctx, ints_base,   pops)
  out_double <- calculate_scenario_outcomes(ctx, ints_double, pops)
  
  cat(sprintf("  additional_suppressed (40%% coverage): %g\n", out_base$additional_suppressed))
  cat(sprintf("  additional_suppressed (80%% coverage): %g\n", out_double$additional_suppressed))
  cat(sprintf("  ratio (80%% / 40%%):                   %.2f\n",
              out_double$additional_suppressed / out_base$additional_suppressed))
  
  expect_equal(out_double$additional_suppressed,
               2 * out_base$additional_suppressed,
               info = "Doubling VL coverage must double additional suppressed (linear relationship)")
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 4: SCALING DOWN VL COVERAGE SCALES DOWN ADDITIONAL SUPPRESSED
# ============================================================================

test_that("Reducing VL coverage from 60% to 20% reduces additional suppressed to 1/3", {
  cat("\n========================================\n")
  cat("TEST 4: Scaled-Down Coverage → Proportionally Fewer Suppressed\n")
  cat("========================================\n")
  
  set_vl_params(efficacy = 0.20)
  
  ints_base  <- zero_interventions(); ints_base$vl_monitoring_routine  <- 60
  ints_low   <- zero_interventions(); ints_low$vl_monitoring_routine   <- 20
  
  out_base <- calculate_scenario_outcomes(ctx, ints_base, pops)
  out_low  <- calculate_scenario_outcomes(ctx, ints_low,  pops)
  
  cat(sprintf("  additional_suppressed (60%% coverage): %g\n", out_base$additional_suppressed))
  cat(sprintf("  additional_suppressed (20%% coverage): %g\n", out_low$additional_suppressed))
  cat(sprintf("  ratio (20%% / 60%%):                   %.2f  (expected: 0.33)\n",
              out_low$additional_suppressed / out_base$additional_suppressed))
  
  expect_equal(out_low$additional_suppressed,
               round(out_base$additional_suppressed / 3),
               info = "Reducing coverage to 1/3 must reduce additional_suppressed to 1/3")
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 5: HIGHER VL MONITORING EFFICACY PRODUCES MORE SUPPRESSION
# ============================================================================

test_that("Tripling VL monitoring efficacy from 10% to 30% triples additional suppressed", {
  cat("\n========================================\n")
  cat("TEST 5: Higher VL Monitoring Efficacy → Proportionally More Suppressed\n")
  cat("========================================\n")
  
  coverage <- 60
  
  set_vl_params(efficacy = 0.10)
  ints_low_eff <- zero_interventions(); ints_low_eff$vl_monitoring_routine <- coverage
  out_low_eff  <- calculate_scenario_outcomes(ctx, ints_low_eff, pops)
  
  set_vl_params(efficacy = 0.30)
  ints_high_eff <- zero_interventions(); ints_high_eff$vl_monitoring_routine <- coverage
  out_high_eff  <- calculate_scenario_outcomes(ctx, ints_high_eff, pops)
  
  cat(sprintf("  additional_suppressed (10%% efficacy): %g\n", out_low_eff$additional_suppressed))
  cat(sprintf("  additional_suppressed (30%% efficacy): %g\n", out_high_eff$additional_suppressed))
  cat(sprintf("  ratio (30%% / 10%%):                   %.2f  (expected: 3.00)\n",
              out_high_eff$additional_suppressed / out_low_eff$additional_suppressed))
  
  expect_equal(out_high_eff$additional_suppressed,
               3 * out_low_eff$additional_suppressed,
               info = "Tripling efficacy must triple additional_suppressed (linear relationship)")
  
  # Restore standard efficacy
  set_vl_params(efficacy = 0.20)
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 6: CASCADE CONSISTENCY AT END OF YEAR
# ============================================================================

test_that("Cascade hierarchy holds and all values are non-negative with VL intervention active", {
  cat("\n========================================\n")
  cat("TEST 6: Cascade Consistency at End of Year\n")
  cat("========================================\n")
  
  set_vl_params(efficacy = 0.20)
  
  ints <- zero_interventions(); ints$vl_monitoring_routine <- 60
  out  <- calculate_scenario_outcomes(ctx, ints, pops)
  
  cat(sprintf("  end_plhiv:      %g\n", out$end_plhiv))
  cat(sprintf("  end_diagnosed:  %g\n", out$end_diagnosed))
  cat(sprintf("  end_on_art:     %g\n", out$end_on_art))
  cat(sprintf("  end_suppressed: %g\n", out$end_suppressed))
  
  # Non-negativity
  expect_gte(out$end_suppressed, 0,      label = "end_suppressed must be >= 0")
  expect_gte(out$end_on_art,     0,      label = "end_on_art must be >= 0")
  expect_gte(out$end_diagnosed,  0,      label = "end_diagnosed must be >= 0")
  expect_gte(out$end_plhiv,      0,      label = "end_plhiv must be >= 0")
  
  # Cascade hierarchy
  expect_lte(out$end_suppressed, out$end_on_art,
             label = "end_suppressed must be <= end_on_art")
  expect_lte(out$end_on_art,    out$end_diagnosed,
             label = "end_on_art must be <= end_diagnosed")
  expect_lte(out$end_diagnosed, out$end_plhiv,
             label = "end_diagnosed must be <= end_plhiv")
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 7: VL MONITORING DOES NOT AFFECT TESTING OR DIAGNOSIS OUTPUTS
# ============================================================================

test_that("VL monitoring does not produce tests_performed, new_diagnoses, or art_initiations", {
  cat("\n========================================\n")
  cat("TEST 7: VL Monitoring Isolation — No Bleed Into Testing Outputs\n")
  cat("========================================\n")
  
  # VL monitoring is a suppression intervention, not a testing intervention.
  # It must not produce diagnoses, tests, or ART initiations — only additional_suppressed.
  
  set_vl_params(efficacy = 0.20)
  
  ints_zero <- zero_interventions()
  ints_vl   <- zero_interventions(); ints_vl$vl_monitoring_routine <- 60
  
  out_zero <- calculate_scenario_outcomes(ctx, ints_zero, pops)
  out_vl   <- calculate_scenario_outcomes(ctx, ints_vl,   pops)
  
  cat(sprintf("  tests_performed  (zero): %g  | (vl=60%%): %g\n",
              out_zero$tests_performed, out_vl$tests_performed))
  cat(sprintf("  new_diagnoses    (zero): %g  | (vl=60%%): %g\n",
              out_zero$new_diagnoses, out_vl$new_diagnoses))
  cat(sprintf("  art_initiations  (zero): %g  | (vl=60%%): %g\n",
              out_zero$art_initiations, out_vl$art_initiations))
  cat(sprintf("  additional_supp  (zero): %g  | (vl=60%%): %g  (should differ)\n",
              out_zero$additional_suppressed, out_vl$additional_suppressed))
  
  expect_equal(out_vl$tests_performed, out_zero$tests_performed,
               info = "VL monitoring must not add to tests_performed")
  
  expect_equal(out_vl$new_diagnoses, out_zero$new_diagnoses,
               info = "VL monitoring must not generate new_diagnoses")
  
  expect_equal(out_vl$art_initiations, out_zero$art_initiations,
               info = "VL monitoring must not generate art_initiations")
  
  expect_gt(out_vl$additional_suppressed, out_zero$additional_suppressed,
            label = "VL monitoring MUST increase additional_suppressed (positive control)")
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 8: VL EFFECT ON end_suppressed SURVIVES WITH TESTING ACTIVE
# (regression test for the n_established_on_art overflow bug)
# ============================================================================
# When testing is active it generates new ART initiates who are counted as
# suppressed in additional_suppressed.  Before the fix, end_suppressed_pre_mort
# could exceed n_established_on_art, and the entire VL gain sat in the overflow
# that was silently discarded.  This test confirms that increasing VL coverage
# still moves end_suppressed even when a large testing programme is running.

test_that("VL effect on end_suppressed is visible even with high testing volume active", {
  cat("\n========================================\n")
  cat("TEST 8: VL Effect Survives With Testing Active (regression)\n")
  cat("========================================\n")
  
  set_vl_params(efficacy = 0.20)
  restore_ltfu_rates()   # re-enable LTFU so the scenario is realistic
  
  # Large testing programme: enough to generate substantial new initiates and
  # overflow n_established_on_art — this is the condition that triggered the bug.
  ints_base <- zero_interventions()
  ints_base$test_facility_general <- 500000   # high absolute volume
  ints_base$vl_monitoring_routine <- 40
  
  ints_scen <- ints_base
  ints_scen$vl_monitoring_routine <- 80       # double VL coverage
  
  out_base <- calculate_scenario_outcomes(ctx, ints_base, pops,
                                          is_baseline                    = TRUE,
                                          baseline_interventions          = ints_base)
  out_scen <- calculate_scenario_outcomes(ctx, ints_scen, pops,
                                          baseline_interventions          = ints_base,
                                          baseline_additional_suppressed  = out_base$additional_suppressed)
  
  cat(sprintf("  art_initiations (base):          %g\n", out_base$art_initiations))
  cat(sprintf("  additional_suppressed (base):    %g\n", out_base$additional_suppressed))
  cat(sprintf("  additional_suppressed (scen):    %g\n", out_scen$additional_suppressed))
  cat(sprintf("  end_suppressed (base):           %g\n", out_base$end_suppressed))
  cat(sprintf("  end_suppressed (scen):           %g\n", out_scen$end_suppressed))
  cat(sprintf("  diff_suppressed:                 %g\n", out_scen$end_suppressed - out_base$end_suppressed))
  cat(sprintf("  infections_averted (scen):       %g\n", out_scen$adult_infections_averted))
  
  # Regression assertion: end_suppressed must increase when VL coverage doubles,
  # regardless of how many new initiates the testing programme generated.
  expect_gt(out_scen$end_suppressed, out_base$end_suppressed,
            label = "end_suppressed must increase with higher VL coverage even when testing is active")
  
  # Consistency check: if additional_suppressed changed, end_suppressed must move
  # in the same direction (they were previously decoupled by the overflow bug).
  if (out_scen$additional_suppressed > out_base$additional_suppressed) {
    expect_gt(out_scen$end_suppressed, out_base$end_suppressed,
              label = "end_suppressed must move in the same direction as additional_suppressed")
  }
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 9: end_suppressed AND additional_suppressed ARE CONSISTENT
# (structural invariant: they must move together across a VL coverage sweep)
# ============================================================================
# Sweeps VL coverage from 0 to 100 % with testing active and verifies that
# end_suppressed increases monotonically — proving there is no threshold at
# which additional_suppressed gains are silently swallowed.

test_that("end_suppressed increases monotonically with VL coverage when testing is active", {
  cat("\n========================================\n")
  cat("TEST 9: end_suppressed Monotone With VL Sweep (testing active)\n")
  cat("========================================\n")
  
  set_vl_params(efficacy = 0.20)
  restore_ltfu_rates()
  
  base_ints <- zero_interventions()
  base_ints$test_facility_general <- 300000   # enough to create new-initiate overflow
  
  coverages      <- c(0, 20, 40, 60, 80, 100)
  end_supp_vals  <- numeric(length(coverages))
  add_supp_vals  <- numeric(length(coverages))
  
  # Use the zero-testing run as baseline so suppression_delta is clean
  baseline_out <- calculate_scenario_outcomes(ctx, base_ints, pops,
                                              is_baseline           = TRUE,
                                              baseline_interventions = base_ints)
  
  for (i in seq_along(coverages)) {
    ints <- base_ints
    ints$vl_monitoring_routine <- coverages[i]
    out  <- calculate_scenario_outcomes(ctx, ints, pops,
                                        baseline_interventions         = base_ints,
                                        baseline_additional_suppressed = baseline_out$additional_suppressed)
    end_supp_vals[i] <- out$end_suppressed
    add_supp_vals[i] <- out$additional_suppressed
    cat(sprintf("  VL %3d%% → additional_suppressed: %6g | end_suppressed: %6g\n",
                coverages[i], add_supp_vals[i], end_supp_vals[i]))
  }
  
  # Both series must be non-decreasing
  for (i in 2:length(coverages)) {
    expect_gte(add_supp_vals[i], add_supp_vals[i - 1],
               label = sprintf("additional_suppressed must not decrease from VL=%d%% to VL=%d%%",
                               coverages[i-1], coverages[i]))
    expect_gte(end_supp_vals[i], end_supp_vals[i - 1],
               label = sprintf("end_suppressed must not decrease from VL=%d%% to VL=%d%%",
                               coverages[i-1], coverages[i]))
  }
  
  # The two series must agree in direction at every step (the decoupling bug
  # would cause add_supp to rise while end_supp stays flat).
  for (i in 2:length(coverages)) {
    add_diff  <- add_supp_vals[i] - add_supp_vals[i - 1]
    supp_diff <- end_supp_vals[i] - end_supp_vals[i - 1]
    if (add_diff > 0) {
      expect_gt(supp_diff, 0,
                label = sprintf(
                  "end_suppressed must rise when additional_suppressed rises (VL %d%% → %d%%)",
                  coverages[i-1], coverages[i]))
    }
  }
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEARDOWN — restore production parameters
# ============================================================================

restore_ltfu_rates()
set_vl_params(efficacy = 0.20)