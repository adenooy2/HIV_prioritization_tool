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
# TEST 1: POPULATION CALCULATIONS - BASIC INTEGRITY
# ============================================================================

test_population_calculations <- function() {
  cat("\n=== TEST 1: POPULATION CALCULATIONS ===\n")
  
  # Set up test context
  context <- list(
    total_population = 1000000,
    hiv_prevalence = 0.05,  # 5%
    percent_diagnosed = 80,
    percent_on_art = 75,
    percent_suppressed = 85,
    new_infections_per_year = 5000,
    aids_deaths_per_year = 1000
  )
  
  pops <- calculate_populations(context)
  
  # Test 1.1: Total population balance
  test_that("Total population equals PLHIV + HIV-negative", {
    expect_equal(pops$plhiv + pops$hiv_negative, pops$total)
  })
  
  cat("✓ Test 1.1 PASSED: Total population balance\n")
  cat(sprintf("  PLHIV: %s, HIV-: %s, Total: %s\n", 
              format(pops$plhiv, big.mark=","),
              format(pops$hiv_negative, big.mark=","),
              format(pops$total, big.mark=",")))
  
  # Test 1.2: PLHIV equals diagnosed + undiagnosed
  test_that("PLHIV equals diagnosed + undiagnosed", {
    expect_equal(pops$diagnosed + pops$undiagnosed, pops$plhiv)
  })
  
  cat("✓ Test 1.2 PASSED: PLHIV segmentation\n")
  cat(sprintf("  Diagnosed: %s, Undiagnosed: %s, Total PLHIV: %s\n",
              format(pops$diagnosed, big.mark=","),
              format(pops$undiagnosed, big.mark=","),
              format(pops$plhiv, big.mark=",")))
  
  # Test 1.3: Diagnosed equals on_art + diagnosed_not_on_art
  test_that("Diagnosed equals on_art + diagnosed_not_on_art", {
    expect_equal(pops$on_art + pops$diagnosed_not_on_art, pops$diagnosed)
  })
  
  cat("✓ Test 1.3 PASSED: Diagnosed segmentation\n")
  cat(sprintf("  On ART: %s, Not on ART: %s, Total Diagnosed: %s\n",
              format(pops$on_art, big.mark=","),
              format(pops$diagnosed_not_on_art, big.mark=","),
              format(pops$diagnosed, big.mark=",")))
  
  # Test 1.4: On ART equals suppressed + unsuppressed
  test_that("On ART equals suppressed + unsuppressed", {
    expect_equal(pops$suppressed + pops$unsuppressed, pops$on_art)
  })
  
  cat("✓ Test 1.4 PASSED: ART segmentation\n")
  cat(sprintf("  Suppressed: %s, Unsuppressed: %s, Total On ART: %s\n",
              format(pops$suppressed, big.mark=","),
              format(pops$unsuppressed, big.mark=","),
              format(pops$on_art, big.mark=",")))
  
  
  return(pops)
}

# ============================================================================
# TEST 2: CASCADE PROGRESSION - NO INTERVENTION
# ============================================================================

test_cascade_no_intervention <- function() {
  cat("\n=== TEST 2: CASCADE WITH NO INTERVENTION ===\n")
  
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
  
  # Baseline = Target (no change)
  baseline <- default_baseline_interventions
  target <- default_baseline_interventions
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Test 2.1: No change should result in zero impacts
  test_that("Zero interventions produce zero impacts", {
    expect_equal(impact$infections_averted, 0)
    expect_equal(impact$deaths_averted, 0)
    expect_equal(impact$new_diagnoses, 0)
    expect_equal(impact$art_initiations, 0)
  })
  
  cat("✓ Test 2.1 PASSED: Zero intervention impact\n")
  cat(sprintf("  All impact metrics are zero as expected\n"))
  
  # Test 2.2: New cascade equals baseline cascade
  test_that("New cascade equals baseline with no intervention", {
    expect_equal(impact$new_diagnosed, pops$diagnosed)
    expect_equal(impact$new_on_art, pops$on_art)
    expect_equal(impact$new_suppressed, pops$suppressed)
  })
  
  cat("✓ Test 2.2 PASSED: Cascade unchanged\n")
  cat(sprintf("  Baseline Diagnosed: %s = New Diagnosed: %s\n",
              format(pops$diagnosed, big.mark=","),
              format(impact$new_diagnosed, big.mark=",")))
  
  return(list(pops = pops, impact = impact))
}

# ============================================================================
# TEST 3: TESTING INTERVENTION LOGIC
# ============================================================================

test_testing_intervention <- function() {
  cat("\n=== TEST 3: TESTING INTERVENTION LOGIC ===\n")
  
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
  
  # Calculate expected test yield
  test_yield <- (pops$undiagnosed + pops$ltfu) / pops$sexually_active
  test_yield <- min(test_yield, 0.10)
  
  cat(sprintf("  Test yield: %.2f%%\n", test_yield * 100))
  cat(sprintf("  Undiagnosed: %s, LTFU: %s, Sexually active: %s\n",
              format(pops$undiagnosed, big.mark=","),
              format(pops$ltfu, big.mark=","),
              format(pops$sexually_active, big.mark=",")))
  
  # Scale up facility testing by 10,000 tests
  baseline <- default_baseline_interventions
  target <- baseline
  target$test_facility <- baseline$test_facility + 10000
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Get test parameters
  test_facility <- intervention_groups$testing$interventions$test_facility
  efficacy <- test_facility$efficacy
  linkage_rate <- test_facility$linkage_rate
  
  # Test 3.1: Expected positive tests
  expected_positive <- 10000 * test_yield * efficacy
  expected_new_dx <- expected_positive * 0.5
  expected_re_eng <- expected_positive * 0.5
  expected_linked <- expected_positive * linkage_rate
  
  cat(sprintf("\nExpected outcomes from 10,000 additional tests:\n"))
  cat(sprintf("  Positive tests: %.0f\n", expected_positive))
  cat(sprintf("  New diagnoses: %.0f\n", expected_new_dx))
  cat(sprintf("  Re-engagement: %.0f\n", expected_re_eng))
  cat(sprintf("  Linked to care: %.0f\n", expected_linked))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  New diagnoses: %d\n", impact$new_diagnoses))
  cat(sprintf("  Re-engagement: %d\n", impact$re_engagement))
  cat(sprintf("  ART initiations: %d\n", impact$art_initiations))
  
  # Test 3.2: New diagnoses roughly matches expectation (within 10%)
  test_that("New diagnoses matches expected value", {
    expect_lt(abs(impact$new_diagnoses - expected_new_dx) / expected_new_dx, 0.1)
  })
  
  cat("✓ Test 3.2 PASSED: New diagnoses match expectation\n")
  
  # Test 3.3: ART initiations roughly matches expected linkage
  test_that("ART initiations match expected linkage", {
    expect_lt(abs(impact$art_initiations - expected_linked) / expected_linked, 0.1)
  })
  
  cat("✓ Test 3.3 PASSED: ART initiations match expectation\n")
  
  # Test 3.4: New cascade integrity
  test_that("New cascade maintains integrity", {
    expect_equal(impact$new_diagnosed, pops$diagnosed + impact$new_diagnoses)
    expect_equal(impact$new_on_art, pops$on_art + impact$art_initiations)
  })
  
  cat("✓ Test 3.4 PASSED: New cascade integrity maintained\n")
  
  return(impact)
}

# ============================================================================
# TEST 4: PREVENTION INTERVENTION LOGIC
# ============================================================================

test_prevention_intervention <- function() {
  cat("\n=== TEST 4: PREVENTION INTERVENTION LOGIC ===\n")
  
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
  
  # Calculate incidence rate
  incidence_rate <- context$new_infections_per_year / pops$hiv_negative
  
  cat(sprintf("  Incidence rate: %.4f (%.2f per 1000)\n", 
              incidence_rate, incidence_rate * 1000))
  
  # Scale up PrEP by 1,000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$prep_oral <- baseline$prep_oral + 1000
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Get PrEP parameters
  prep <- intervention_groups$prevention$interventions$prep_oral
  efficacy <- prep$efficacy
  
  # Expected infections averted
  expected_averted <- 1000 * incidence_rate * efficacy
  
  cat(sprintf("\nExpected outcomes from 1,000 additional PrEP users:\n"))
  cat(sprintf("  Infections averted: %.1f\n", expected_averted))
  
  cat(sprintf("\nActual outcomes:\n"))
  cat(sprintf("  Infections averted: %d\n", impact$infections_averted))
  
  # Test 4.1: Infections averted matches expectation
  test_that("Infections averted matches expected value", {
    expect_lt(abs(impact$infections_averted - expected_averted) / expected_averted, 0.1)
  })
  
  cat("✓ Test 4.1 PASSED: Infections averted match expectation\n")
  
  # Test 4.2: Cost calculation
  expected_cost <- 1000 * prep$unit_cost
  
  test_that("Cost matches expected value", {
    expect_equal(impact$total_cost, expected_cost)
  })
  
  cat("✓ Test 4.2 PASSED: Cost calculation correct\n")
  cat(sprintf("  Expected cost: $%s\n", format(expected_cost, big.mark=",")))
  cat(sprintf("  Actual cost: $%s\n", format(impact$total_cost, big.mark=",")))
  
  return(impact)
}

# ============================================================================
# TEST 5: SCALE-DOWN LOGIC
# ============================================================================

test_scale_down <- function() {
  cat("\n=== TEST 5: SCALE-DOWN LOGIC ===\n")
  
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
  
  # Scale DOWN PrEP by 1,000 people
  baseline <- default_baseline_interventions
  target <- baseline
  target$prep_oral <- baseline$prep_oral - 1000
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Test 5.1: Negative impacts
  test_that("Scale-down produces negative impacts", {
    expect_lt(impact$infections_averted, 0)
  })
  
  cat("✓ Test 5.1 PASSED: Scale-down produces negative impacts\n")
  cat(sprintf("  Infections averted: %d (negative = more infections)\n", 
              impact$infections_averted))
  
  # Test 5.2: Cost savings
  prep <- intervention_groups$prevention$interventions$prep_oral
  expected_savings <- 1000 * prep$unit_cost
  
  test_that("Scale-down produces cost savings", {
    expect_equal(impact$cost_savings, expected_savings)
  })
  
  cat("✓ Test 5.2 PASSED: Cost savings calculated\n")
  cat(sprintf("  Cost savings: $%s\n", format(impact$cost_savings, big.mark=",")))
  
  return(impact)
}

# ============================================================================
# TEST 6: 95-95-95 GOALS CALCULATION
# ============================================================================

test_95_goals <- function() {
  cat("\n=== TEST 6: 95-95-95 GOALS CALCULATION ===\n")
  
  # Perfect 95-95-95 scenario
  context_perfect <- list(
    total_population = 1000000,
    hiv_prevalence = 0.05,
    percent_diagnosed = 95,
    percent_on_art = 95,
    percent_suppressed = 95,
    new_infections_per_year = 5000,
    aids_deaths_per_year = 1000
  )
  
  pops_perfect <- calculate_populations(context_perfect)
  goals_perfect <- calculate_95goals(pops_perfect)
  
  # Test 6.1: Perfect 95-95-95
  test_that("Perfect 95-95-95 achieved", {
    expect_equal(goals_perfect$first_95, 95)
    expect_equal(goals_perfect$second_95, 95)
    expect_equal(goals_perfect$third_95, 95)
  })
  
  cat("✓ Test 6.1 PASSED: Perfect 95-95-95 scenario\n")
  cat(sprintf("  1st 95: %.1f%%, 2nd 95: %.1f%%, 3rd 95: %.1f%%\n",
              goals_perfect$first_95, goals_perfect$second_95, goals_perfect$third_95))
  
  # Sub-optimal scenario
  context_subopt <- list(
    total_population = 1000000,
    hiv_prevalence = 0.05,
    percent_diagnosed = 70,
    percent_on_art = 80,
    percent_suppressed = 85,
    new_infections_per_year = 5000,
    aids_deaths_per_year = 1000
  )
  
  pops_subopt <- calculate_populations(context_subopt)
  goals_subopt <- calculate_95goals(pops_subopt)
  
  cat("\nSub-optimal scenario:\n")
  cat(sprintf("  1st 95: %.1f%%, 2nd 95: %.1f%%, 3rd 95: %.1f%%\n",
              goals_subopt$first_95, goals_subopt$second_95, goals_subopt$third_95))
  
  # Test 6.2: Goals less than 95
  test_that("Sub-optimal scenario shows gaps", {
    expect_lt(goals_subopt$first_95, 95)
    expect_lt(goals_subopt$second_95, 95)
    expect_lt(goals_subopt$third_95, 95)
  })
  
  cat("✓ Test 6.2 PASSED: Sub-optimal scenario identified\n")
  
  return(list(perfect = goals_perfect, subopt = goals_subopt))
}

# ============================================================================
# TEST 7: COVERAGE VS ABSOLUTE INTERVENTIONS
# ============================================================================

# test_coverage_vs_absolute <- function(intervention_groups) {
#   cat("\n=== TEST 7: COVERAGE VS ABSOLUTE INTERVENTIONS ===\n")
#   
#   context <- list(
#     total_population = 1000000,
#     hiv_prevalence = 0.05,
#     percent_diagnosed = 80,
#     percent_on_art = 75,
#     percent_suppressed = 85,
#     new_infections_per_year = 5000,
#     aids_deaths_per_year = 1000
#   )
#   
#   pops <- calculate_populations(context)
#   
#   # Test absolute intervention (PrEP)
#   baseline <- default_baseline_interventions
#   target_abs <- baseline
#   target_abs$prep_oral <- baseline$prep_oral + 1000  # Add 1000 people
#   
#   impact_abs <- calculate_impact(context, baseline, target_abs, pops)
#   
#   # Test coverage intervention (infant prophylaxis)
#   target_cov <- baseline
#   target_cov$infant_prophylaxis <- baseline$infant_prophylaxis + 10  # Add 10% coverage
#   
#   impact_cov <- calculate_impact(context, baseline, target_cov, pops)
#   
#   # Calculate expected people reached for coverage
#   infant_prophylaxis <- intervention_groups$prevention$interventions$infant_prophylaxis
#   eligible <- pops$hiv_exposed_infants
#   expected_reached <- eligible * (10 / 100)
#   
#   cat(sprintf("Absolute intervention (PrEP):\n"))
#   cat(sprintf("  People reached: 1,000 (specified directly)\n"))
#   
#   cat(sprintf("\nCoverage intervention (Infant prophylaxis):\n"))
#   cat(sprintf("  Eligible population: %s\n", format(eligible, big.mark=","))
#       cat(sprintf("  Coverage increase: 10%%\n"))
#       cat(sprintf("  Expected people reached: %.0f\n", expected_reached))
#       
#       cat("\n✓ Test 7 PASSED: Both intervention types calculated\n")
#       
#       return(list(absolute = impact_abs, coverage = impact_cov))
# }

# ============================================================================
# TEST 8: ART PROVISION COST
# ============================================================================

test_art_provision_cost <- function() {
  cat("\n=== TEST 8: ART PROVISION COST ===\n")
  
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
  
  # Add testing to increase ART initiations
  baseline <- default_baseline_interventions
  target <- baseline
  target$test_facility <- baseline$test_facility + 50000
  
  impact <- calculate_impact(context, baseline, target, pops)
  
  # Calculate expected ART provision cost
  total_needing_art <- pops$on_art + impact$art_initiations
  expected_art_cost <- total_needing_art * 200
  
  cat(sprintf("Baseline on ART: %s\n", format(pops$on_art, big.mark=",")))
  cat(sprintf("New ART initiations: %s\n", format(impact$art_initiations, big.mark=",")))
  cat(sprintf("Total needing ART: %s\n", format(total_needing_art, big.mark=",")))
  cat(sprintf("\nExpected ART provision cost: $%s\n", 
              format(expected_art_cost, big.mark=",")))
  cat(sprintf("Actual ART provision cost: $%s\n", 
              format(impact$art_provision_cost, big.mark=",")))
  
  # Test 8.1: ART provision cost matches
  test_that("ART provision cost calculated correctly", {
    expect_equal(impact$art_provision_cost, expected_art_cost)
  })
  
  cat("\n✓ Test 8.1 PASSED: ART provision cost correct\n")
  
  # Test 8.2: Net cost includes ART provision
  expected_net <- impact$total_cost - impact$cost_savings + impact$art_provision_cost
  
  test_that("Net cost includes all components", {
    expect_equal(impact$net_cost, expected_net)
  })
  
  cat("✓ Test 8.2 PASSED: Net cost calculation correct\n")
  cat(sprintf("  Intervention costs: $%s\n", format(impact$total_cost, big.mark=",")))
  cat(sprintf("  Cost savings: $%s\n", format(impact$cost_savings, big.mark=",")))
  cat(sprintf("  ART provision: $%s\n", format(impact$art_provision_cost, big.mark=",")))
  cat(sprintf("  Net cost: $%s\n", format(impact$net_cost, big.mark=",")))
  
  return(impact)
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
  
  results$test1 <- test_population_calculations()
  #results$test2 <- test_cascade_no_intervention()
  #results$test3 <- test_testing_intervention()
  #results$test4 <- test_prevention_intervention(intervention_groups)
  #results$test5 <- test_scale_down(intervention_groups)
  #results$test6 <- test_95_goals()
  #results$test7 <- test_coverage_vs_absolute(intervention_groups)
  #results$test8 <- test_art_provision_cost(intervention_groups)
  
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
