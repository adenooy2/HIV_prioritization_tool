# ============================================================================
# HIV Intervention Impact Calculator - TESTING FRAMEWORK
# ============================================================================
# Comprehensive testing suite for validating model calculations and logic
# ============================================================================
rm(list=ls())
library(testthat)
library(dplyr)

##Source logic file - if updating logic frequently, change local path
tryCatch(
  {
    # Source logic – personal/local (change if needed)
    #Alex local: "/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP/Mock-Up logic.R"
    source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP/Mock-Up logic.R")
    message("Sourced local file successfully.")
  },
  error = function(e) {
    message("Local source failed. Trying GitHub version...")
    
    tryCatch(
      {
        source("https://raw.githubusercontent.com/adenooy2/HIV_prioritization_tool/refs/heads/main/code/MockUps/MVP/Mock-up%20TIER%20interface%20MVP.R")
        message("Sourced GitHub file successfully.")
      },
      error = function(e2) {
        stop("Both local and GitHub sources failed:\n",
             "Local error: ", e$message, "\n",
             "GitHub error: ", e2$message)
      }
    )
  }
)


# ============================================================================
# TEST: Testing INTERVENTION LOGIC 
# ============================================================================
test_basic_testing <- function() {
  cat("\n=== TEST: General testing INTERVENTION LOGIC ===\n")
  
  #Test parameters - multiplier
  intervention_params_test=intervention_params
  intervention_params_test$multiplier[intervention_params_test$intervention_key=="test_facility_general"]=1
  intervention_params_test$multiplier[intervention_params_test$intervention_key=="test_facility_targeted"]=2
  
  #Test parameters - efficacy
  intervention_params_test$efficacy[intervention_params_test$intervention_key=="test_facility_general"]=0.97
  intervention_params_test$efficacy[intervention_params_test$intervention_key=="test_facility_targeted"]=0.97
  
  #Test parameters - linkage
  intervention_params_test$linkage_rate[intervention_params_test$intervention_key=="test_facility_general"]=0.9
  intervention_params_test$linkage_rate[intervention_params_test$intervention_key=="test_facility_targeted"]=0.8
  
  
  intervention_groups=build_intervention_groups(intervention_params_test)
  
  general_test= intervention_groups$testing$interventions$test_facility_general
  target_test= intervention_groups$testing$interventions$test_facility_targeted
  
  assign("intervention_groups", intervention_groups, envir = .GlobalEnv)
  
  context <- list(
    total_population = 1000000,
    hiv_prevalence = 0.05,
    percent_diagnosed = 80,
    percent_on_art = 75,
    percent_suppressed = 85,
    new_infections_per_year = 5000,
    aids_deaths_per_year = 1000
  )
  
  pops <- calculate_populations(context)
  
  # Calculate base yield
  base_yield <- (pops$undiagnosed+pops$ltfu) / pops$hiv_negative
  
  cat(sprintf("  Incidence rate: %.4f (%.2f per 1000)\n", 
              base_yield, base_yield * 1000))
  
  #test 1
  # Scale up general facility testing by 10 000 people
  baseline <- default_baseline_interventions

  target <- baseline
  target$test_facility_general = baseline$test_facility_general + 10000
  
  #Impact
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected positive tests 
  expected_positive_general <- round(10000 * base_yield * general_test$test_yield_multiplier*general_test$efficacy)
  
  cat(sprintf("\nExpected outcomes from 500 additional people receiving pep:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test pep 1: Infections averted matches expectation
  test_that("Infections averted matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test pep 1 PASSED: Infections averted match expectation\n")
  
  
  # Scale down pep by 500 people
  baseline <- default_baseline_interventions
  baseline$pep=1000
  target <- baseline
  target$pep <- baseline$pep - 500
  
  # Expected infections averted
  expected_averted <- round(-500 * incidence_rate * efficacy_pep)
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Test pep 2: Infections averted matches expectation
  test_that("Infections averted matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test vmmc 2 PASSED: Infections averted match expectation\n")
}




# ============================================================================
# RUN ALL TESTS
# ============================================================================

run_all_tests <- function() {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════╗\n")
  cat("║   HIV INTERVENTION IMPACT CALCULATOR - COMPREHENSIVE TEST SUITE   ║\n")
  cat("╚════════════════════════════════════════════════════════════════════╝\n")
  
  # Load data
  cat("\nLoading intervention parameters...\n")
  intervention_params <- load_intervention_params()
  intervention_groups <- build_intervention_groups(intervention_params)
  cat("✓ Data loaded successfully\n")
  
  # Run tests
  results <- list()
  results$prep <- test_prevention_prep()
  results$vmmc <- test_prevention_vmmc()
  results$pep=test_prevention_pep()
  results$ctx=test_prevention_ctx()

  
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════════╗\n")
  cat("║                      ALL TESTS COMPLETED                           ║\n")
  cat("╚════════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  
  return(results)
}

# Run the test suite
test_results <- run_all_tests()
# test_results <- run_all_tests()
