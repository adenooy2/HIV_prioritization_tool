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
test_basic_testing_general <- function() {
  cat("\n=== TEST: General testing INTERVENTION LOGIC ===\n")
  
  re_test_prop=0.5
  base_vs_assumption=10
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
    aids_deaths_per_year = 1000,
    birth_rate=24,
    prop_pop_male=49,
    prop_pop_under_14=40
  )
  
  pops <- calculate_populations(context)
  print(pops)
  
  # Calculate base yield
  base_yield <- 0.9* (pops$undiagnosed+pops$ltfu) / pops$sexually_active
  base_yield=min(base_yield,0.1)
  
  cat(sprintf("  Base yield: %.4f (%.2f per 1000)\n", 
              base_yield, base_yield * 1000))
  
  #test 1
  # Scale up general facility testing by 10 000 people
  baseline <- default_baseline_interventions

  target <- baseline
  target$test_facility_general = baseline$test_facility_general + 10000
  
  #Impact
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected positive tests 
  expected_positive_general <- 10000 * base_yield * as.numeric(general_test$test_yield_multiplier)*general_test$efficacy
  
  cat(sprintf("\nExpected outcomes from 10,000 aditional general facility tests:\n"))
  cat(sprintf("  Expected positive: %.1f\n", round(expected_positive_general)))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Actual positive: %f\n", impact$new_positive))
  
  # Test general testing 1 (positives)
  test_that("Infections averted matches expected value", {
    expect_equal(impact$new_positive, round(expected_positive_general))
  })
  
  cat("✓ Test basics 1 PASSED: Expected positive tests matched actual\n")
  
  ###New diagnoses
  
  expected_new_dx=expected_positive_general*(1-re_test_prop)
  
  cat(sprintf("\nExpected new diagnoses from 10,000 aditional general facility tests:\n"))
  cat(sprintf("  Expected new diagnosed: %.1f\n", round(expected_new_dx)))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Actual new_dx: %f\n", impact$new_diagnoses))
  
  # Test general testing 2
  test_that("New diagnoses matches expected value", {
    expect_equal(impact$new_diagnoses, round(expected_new_dx))
  })
  
  cat("✓ Test basics 2 PASSED: Expected new_dx matched actual\n")
  
  ###ART inititations
  
  expected_new_art=expected_positive_general*general_test$linkage_rate
  
  cat(sprintf("\nExpected new ART from 10,000 aditional general facility tests:\n"))
  cat(sprintf("  Expected new ART: %.1f\n", round(expected_new_art)))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Actual ART: %f\n", impact$art_initiations))
  
  # Test general testing 2
  test_that("New diagnoses matches expected value", {
    expect_equal(impact$art_initiations, round(expected_new_art))
  })
  
  cat("✓ Test basics 3 PASSED: Expected new art matched actual\n")
  
  ###Newly suppressed
  
  expected_new_VS=expected_new_art*(context$percent_suppressed-base_vs_assumption)/100
  
  print(paste("expected_vs_rate: ",context$percent_suppressed-base_vs_assumption))
  
  cat(sprintf("\nExpected new VS from 10,000 aditional general facility tests:\n"))
  cat(sprintf("  Expected new VS: %.1f\n", round(expected_new_VS)))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Actual new VS: %f\n", impact$additional_suppressed))
  
  # Test general testing 2
  test_that("New VS matches expected value", {
    expect_equal(impact$additional_suppressed, round(expected_new_VS))
  })
  
  cat("✓ Test basics 4 PASSED: Expected new VS matched actual\n")
  
}

############TEST SET TWO: COMPARING TESTING TYPES #################
test_different_testing <- function() {
  cat("\n=== TEST: General testing INTERVENTION LOGIC ===\n")
  
  re_test_prop=0.5
  base_vs_assumption=10
  #Test parameters - multiplier
  intervention_params_test=intervention_params
  intervention_params_test$yield_multiplier[intervention_params_test$intervention_key=="test_facility_general"]=1
  intervention_params_test$yield_multiplier[intervention_params_test$intervention_key=="test_facility_targeted"]=2
  
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
    aids_deaths_per_year = 1000,
    birth_rate=24,
    prop_pop_male=49,
    prop_pop_under_14=40
  )
  
  pops <- calculate_populations(context)
  print(pops)
  
  # Calculate base yield
  base_yield <- 0.9* (pops$undiagnosed+pops$ltfu) / pops$sexually_active
  base_yield=min(base_yield,0.1)
  
  cat(sprintf("  Base yield: %.4f (%.2f per 1000)\n", 
              base_yield, base_yield * 1000))
  
  #test 1
  # Scale up general facility testing by 10 000 people and targeted testings by 10 000
  baseline <- default_baseline_interventions
  
  target <- baseline
  target$test_facility_general = baseline$test_facility_general + 10000
  target$test_facility_targeted = baseline$test_facility_targeted + 10000
  
  #Impact
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected positive tests 
  expected_positive_general=(10000 * base_yield * as.numeric(general_test$test_yield_multiplier)*general_test$efficacy)
  expected_positive_target=(10000 * base_yield * as.numeric(target_test$test_yield_multiplier)*target_test$efficacy)
  expected_positive_total=expected_positive_general+expected_positive_target
  
  cat(sprintf("\nExpected targeted = 2x expected general:\n"))
  cat(sprintf("  Expected positive targeted: %.1f\n", round(expected_positive_target)))
  cat(sprintf("  Expected positive general: %.1f\n", round(expected_positive_general)))
  
  # Test expected general vs targeted
  test_that("Targeted positives = 2 * expected general averted matches expected value", {
    expect_equal(round(2*expected_positive_general), round(expected_positive_target))
  })
  
  
  # Expected positive total
  cat(sprintf("  Expected positive : %.1f\n", round(expected_positive_total)))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Actual positive: %f\n", impact$new_positive))
  
  test_that("Test expected positives facility = actual", {
    expect_equal(round(expected_positive_total), round(impact$new_positive))
  })
  
  ###New diagnoses
  
  expected_new_dx=expected_positive_total*(1-re_test_prop)
  
  cat(sprintf("\nExpected new diagnoses from 10,000 aditional general facility tests and 10000 additional targeted facility tests:\n"))
  cat(sprintf("  Expected new diagnosed: %.1f\n", round(expected_new_dx)))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Actual new_dx: %f\n", impact$new_diagnoses))
  
  # Test general testing 2
  test_that("New diagnoses matches expected value", {
    expect_equal(impact$new_diagnoses, round(expected_new_dx))
  })
  
  cat("✓ Test multiple modalities: Expected new_dx matched actual\n")
  
  ###ART inititations
  expected_art_general=expected_positive_general*general_test$linkage_rate
  expected_art_targeted=expected_positive_target*target_test$linkage_rate
  
  expected_art_total=expected_art_general+expected_art_targeted
  
  cat(sprintf("\nExpected new ART from 10,000 aditional general and 10,000. targeted facility tests:\n"))
  cat(sprintf(" Expected new ART: %.1f\n", round(expected_art_total)))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Actual ART: %f\n", impact$art_initiations))
  
  # Modlaities ART inititations
  test_that("New ART matches expected value", {
    expect_equal(impact$art_initiations, round(expected_art_total))
  })
  
  cat("✓ Test modalities PASSED: Expected new art matched actual\n")
  
  ###Virally Suppressed
  expected_vs=expected_art_total*(context$percent_suppressed-base_vs_assumption)/100
  
  
  cat(sprintf("\nExpected new VS from 10,000 aditional general and 10,000 targeted facility tests:\n"))
  cat(sprintf("  Expected new VS: %.1f\n", round(expected_vs)))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Actual new VS: %f\n", impact$additional_suppressed))
  
  # Test general testing 2
  test_that("New VS matches expected value", {
    expect_equal(impact$additional_suppressed, round(expected_vs))
  })
  
  cat("✓ Test modalities PASSED: Expected new VS matched actual\n")
  
  
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
  results$basic_tests <- test_basic_testing_general()
  results$comp_testing=test_different_testing()
 

  
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
