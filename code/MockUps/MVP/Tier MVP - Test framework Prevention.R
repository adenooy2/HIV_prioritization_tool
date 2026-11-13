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
# TEST: PREVENTION INTERVENTION LOGIC (PREP)
# ============================================================================

test_prevention_prep <- function() {
  cat("\n=== TEST Prep: PREVENTION INTERVENTION LOGIC ===\n")
  
  #oral prep efficacy - 80%, prep_len efficacy=0.9
  intervention_params_test=intervention_params
  intervention_params_test$efficacy[intervention_params_test$intervention_key=="prep_oral"]=0.8
  intervention_params_test$efficacy[intervention_params_test$intervention_key=="prep_lenacapavir"]=0.9
  intervention_groups=build_intervention_groups(intervention_params_test)
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
    prop_pop_male=0.49,
    prop_pop_under_14=0.4
  )
  
  pops <- calculate_populations(context)
  
  # Calculate incidence rate
  incidence_rate <- context$new_infections_per_year / pops$hiv_negative
  
  cat(sprintf("  Incidence rate: %.4f (%.2f per 1000)\n", 
              incidence_rate, incidence_rate * 1000))
  
  # Scale up oral PrEP by 1,000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$prep_oral <- baseline$prep_oral + 1000
  
  # Set oral PrEP parameters
  prep <- intervention_groups$prevention$interventions$prep_oral
  efficacy_oral <- prep$efficacy
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  
  
  # Expected infections averted
  expected_averted <- round(1000 * incidence_rate * efficacy_oral)
  
  cat(sprintf("\nExpected outcomes from 1000 additional oral PrEP users:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test Prep 1: Infections averted matches expectation
  test_that("Infections averted matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test Prep 1 PASSED: Infections averted match expectation\n")
  
  
  # Scale down oral PrEP by 1,000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$prep_oral <- baseline$prep_oral - 1000
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected infections averted
  expected_averted <- -round(1000 * incidence_rate * efficacy_oral)
  
  cat(sprintf("\nExpected outcomes from 1000 fewer oral PrEP users:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test Prep 2: Infections averted matches expectation (should be negative)
  test_that("Infections averted matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test Prep 2 PASSED: Infections averted match expectation\n")
  
  
  # Scale up len PrEP by 1,000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$prep_lenacapavir <- baseline$prep_lenacapavir + 1000
  
  #len efficacy
  prep_len <- intervention_groups$prevention$interventions$prep_lenacapavir
  efficacy_len <- prep_len$efficacy
  
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected infections averted
  expected_averted <- round(1000 * incidence_rate * efficacy_len)
  
  cat(sprintf("\nExpected outcomes from 1000 more lenacapavir PrEP users:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test prep.3: Infections averted matches expectation from lenacapavir
  test_that("Infections averted from prepr with lenacapavir matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test Prep 3 PASSED: Infections averted match expectation\n")
  
  # Scale up len PrEP by 500 people and oral prep by 1000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$prep_lenacapavir <- baseline$prep_lenacapavir + 500
  target$prep_oral <- baseline$prep_oral + 1000
  
  impact <- calculate_impact(context, baseline, target, pops)
  expected_averted <- round(500 * incidence_rate * efficacy_len+1000 * incidence_rate * efficacy_oral)
  
  cat(sprintf("\nExpected outcomes from 500 more lenacapavir PrEP users and 1000 more oral prep users:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test prep 4: Infections averted matches expectation from 500 additional lenacapavir
  test_that("Infections averted from prep with lenacapavir (+500) and oral prep (+1000) matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test Prep 4 PASSED: Infections averted match expectation\n")
  
  # Scale up len PrEP by 500 people and oral prep down by 1000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$prep_lenacapavir <- baseline$prep_lenacapavir + 500
  target$prep_oral <- baseline$prep_oral - 1000
  
  impact <- calculate_impact(context, baseline, target, pops)
  expected_averted <- round(500 * incidence_rate * efficacy_len-1000 * incidence_rate * efficacy_oral)
  
  cat(sprintf("\nExpected outcomes from 500 more lenacapavir PrEP users and 1000 fewer oral prep users:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test prep 5: Infections averted matches expectation from 500 additional lenacapavir
  test_that("Infections averted from prep with lenacapavir (+500) and oral prep (-1000) matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test Prep 5 PASSED: Infections averted match expectation\n")
  
  # Prep to more than eligible popoulatin should have same impact as prep to eligible population (i.e zero difference)
  pops$high_risk_negative=10000
  baseline <- default_baseline_interventions
  baseline$prep_oral=10000
  target <- baseline
  target$prep_oral <- 15000
  
  impact <- calculate_impact(context, baseline, target, pops)
  expected_averted =0
  
  cat(sprintf("\nExpected o infections averted when more than eligible pop on prep compared to eligible pop:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test prep 6 : No difference in infection averted between 100% eligble pop and 150% eligible pop
  test_that("Infections averted from prep with lenacapavir (+500) and oral prep (-1000) matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  pops <- calculate_populations(context)
  cat("✓ Test Prep 6 PASSED: Infections averted match expectation\n")
  
  # Test prep 7: Cost calculations: scale up oral prep by 1000 and lenacapavir by 2000
  
  prep_oral <- intervention_groups$prevention$interventions$prep_oral
  prep_lenacapavir=intervention_groups$prevention$interventions$prep_lenacapavir
  
  baseline <- default_baseline_interventions
  target <- baseline
  target$prep_lenacapavir <- baseline$prep_lenacapavir + 2000
  target$prep_oral <- baseline$prep_oral + 1000
  
  impact <- calculate_impact(context, baseline, target, pops)
  expected_cost <- 1000 * prep_oral$unit_cost+2000*prep_lenacapavir$unit_cost
  
  
  cat(sprintf("\nExpected scale up costs (+1000 oral, +2000 len):\n"))
  cat(sprintf("  Prep cost: %.1f\n", expected_cost))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Total cost: %d\n", impact$total_cost))
  
  
  
  test_that("Cost matches expected value", {
    expect_equal(impact$total_cost, expected_cost)
  })
  
  cat("✓ Test prep 7 PASSED: prep scale up costs work\n")
  cat(sprintf("  Expected cost: $%s\n", format(expected_cost, big.mark=",")))
  cat(sprintf("  Actual cost: $%s\n", format(impact$total_cost, big.mark=",")))
  
  return(impact)
}


# ============================================================================
# TEST: PREVENTION INTERVENTION LOGIC (VMMC)
# ============================================================================

test_prevention_vmmc <- function() {
  cat("\n=== TEST VMMC: PREVENTION INTERVENTION LOGIC ===\n")
  
  #vmmc efficacy - 60%
  intervention_params_test=intervention_params
  intervention_params_test$efficacy[intervention_params_test$intervention_key=="vmmc"]=0.6
  intervention_groups=build_intervention_groups(intervention_params_test)
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
    prop_pop_male=0.49,
    prop_pop_under_14=0.4
  )
  
  pops <- calculate_populations(context)
  
  # Calculate incidence rate
  incidence_rate <- context$new_infections_per_year / pops$hiv_negative
  
  cat(sprintf("  Incidence rate: %.4f (%.2f per 1000)\n", 
              incidence_rate, incidence_rate * 1000))
  
  # Scale up VMMC by 1,000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$vmmc <- baseline$vmmc + 1000
  
  # Set vmmc parameters
  vmmc <- intervention_groups$prevention$interventions$vmmc
  efficacy_vmmc <- vmmc$efficacy
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected infections averted
  expected_averted <- round(1000 * incidence_rate * efficacy_vmmc)
  
  cat(sprintf("\nExpected outcomes from 1000 additional VMMC's:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test vmmc 1: Infections averted matches expectation
  test_that("Infections averted matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test vmmc 1 PASSED: Infections averted match expectation\n")
  
  # Scale down VMMC by 1,000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$vmmc <- baseline$vmmc - 1000
  
  # Expected infections averted
  expected_averted <- round(-1000 * incidence_rate * efficacy_vmmc)
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Test vmmc 2: Infections averted matches expectation
  test_that("Infections averted matches expected value", {
    expect_equal(impact$infections_averted, expected_averted)
  })
  
  cat("✓ Test vmmc 2 PASSED: Infections averted match expectation\n")
}

# ============================================================================
# TEST: PREVENTION INTERVENTION LOGIC (PEP)
# ============================================================================

test_prevention_pep <- function() {
  cat("\n=== TEST PEP: PREVENTION INTERVENTION LOGIC ===\n")
  
  #PEP efficacy - 50%
  intervention_params_test=intervention_params
  intervention_params_test$efficacy[intervention_params_test$intervention_key=="pep"]=0.5
  intervention_groups=build_intervention_groups(intervention_params_test)
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
    prop_pop_male=0.49,
    prop_pop_under_14=0.4
  )
  
  pops <- calculate_populations(context)
  
  # Calculate incidence rate
  incidence_rate <- context$new_infections_per_year / pops$hiv_negative
  
  cat(sprintf("  Incidence rate: %.4f (%.2f per 1000)\n", 
              incidence_rate, incidence_rate * 1000))
  
  # Scale up PEP by 500 people
  baseline <- default_baseline_interventions
  baseline$pep=1000
  target <- baseline
  target$pep <- baseline$pep + 500
  
  # Set pep parameters
  pep <- intervention_groups$prevention$interventions$pep
  efficacy_pep <- pep$efficacy
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected infections averted
  expected_averted <- round(500 * incidence_rate * efficacy_pep)
  
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
# TEST: PREVENTION INTERVENTION LOGIC (CTX)
# ============================================================================

test_prevention_ctx <- function() {
  cat("\n=== TEST Cotrimoxazole: PREVENTION INTERVENTION LOGIC ===\n")
  
  #CXT efficacy - 10%
  intervention_params_test=intervention_params
  intervention_params_test$efficacy[intervention_params_test$intervention_key=="cotrimoxazole"]=0.1
  intervention_groups=build_intervention_groups(intervention_params_test)
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
    prop_pop_male=0.49,
    prop_pop_under_14=0.4
  )
  
  pops <- calculate_populations(context)
  
  # Calculate incidence rate
  mortality_rate <- context$aids_deaths_per_year / pops$plhiv
  cat(sprintf("  mortality rate: %.4f (%.2f per 1000)\n", 
              mortality_rate, mortality_rate * 1000))
  
  
  # Scale up CTX by 20% from 60% to 80%
  baseline <- default_baseline_interventions
  baseline$cotrimoxazole=60
  target <- baseline
  target$cotrimoxazole <- baseline$cotrimoxazole + 20
  
  # Set pep parameters
  ctx <- intervention_groups$prevention$interventions$cotrimoxazole
  efficacy_ctx <- ctx$efficacy
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected deaths averted
  expected_deaths_averted <- round(0.2*mortality_rate*pops$plhiv * efficacy_ctx)
  print(expected_deaths_averted)
  
  # Test ctx 1: Deaths averted matches expectation (increase coverage)
  test_that("Deaths averted matches expected value", {
    expect_equal(impact$deaths_averted, expected_deaths_averted)
  })
  
  cat("✓ Test ctx 1 PASSED: Deaths averted match expectation (increase coverage)\n")
  
  
  # Scale down CTX by 20% from 60% to 40%
  baseline <- default_baseline_interventions
  baseline$cotrimoxazole=60
  target <- baseline
  target$cotrimoxazole <- baseline$cotrimoxazole - 20
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Expected deaths averted
  expected_deaths_averted <- round(-0.2*mortality_rate*pops$plhiv * efficacy_ctx)
  print(expected_deaths_averted)
  
  # Test ctx 2: Deaths averted matches expectation (decrease coverage)
  test_that("Deaths averted matches expected value", {
    expect_equal(impact$deaths_averted, expected_deaths_averted)
  })
  
  cat("✓ Test ctx 2 PASSED: Deaths averted match expectation (decrease coverage)\n")
  
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
