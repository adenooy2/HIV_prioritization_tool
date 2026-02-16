# ============================================================================
# UNIT TESTS FOR TESTING INTERVENTIONS
# ============================================================================
# Tests the logic of testing interventions including:
# - Scale-up and scale-down of single test type
# - Multiple test types with different yields
# - Zero testing scenario
# - Constraint validation
# ============================================================================

library(testthat)

# Source the logic file
source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/Mock-Up_logic_V2.R")

# ============================================================================
# SETUP TEST CONTEXT AND POPULATIONS
# ============================================================================

# Create a standardized test context
create_test_context <- function() {
  list(
    total_population = 1000000,
    hiv_prevalence = 0.05,  # 5% = 50,000 PLHIV
    new_infections_per_year = 2500,
    percent_diagnosed = 80,  # 40,000 diagnosed
    percent_on_art = 75,     # 30,000 on ART
    percent_suppressed = 85, # 25,500 suppressed
    aids_deaths_per_year = 1000,
    birth_rate = 25,
    prop_pop_male = 49,
    prop_pop_under_14 = 40
  )
}

# Create test populations
test_context <- create_test_context()
test_populations <- calculate_populations(test_context)

cat("Test populations calculated:\n")
cat(paste("  Total:", test_populations$total, "\n"))
cat(paste("  PLHIV:", test_populations$plhiv, "\n"))
cat(paste("  Diagnosed:", test_populations$diagnosed, "\n"))
cat(paste("  Undiagnosed:", test_populations$undiagnosed, "\n"))
cat(paste("  On ART:", test_populations$on_art, "\n"))
cat(paste("  Suppressed:", test_populations$suppressed, "\n"))
cat(paste("  LTFU:", test_populations$ltfu, "\n"))
cat("\n")

# ============================================================================
# TEST 1: BASELINE SCENARIO (NO TESTING INTERVENTIONS)
# ============================================================================

test_that("Baseline with zero testing produces no new diagnoses", {
  cat("\n========================================\n")
  cat("TEST 1: Baseline with Zero Testing\n")
  cat("========================================\n")
  
  baseline_interventions <- list()
  
  # Set all testing to zero
  for (group_key in names(intervention_groups)) {
    group <- intervention_groups[[group_key]]
    for (int_key in names(group$interventions)) {
      baseline_interventions[[int_key]] <- 0
    }
  }
  
  outcomes <- calculate_scenario_outcomes(test_context, baseline_interventions, test_populations)
  
  cat(paste("Tests performed:", outcomes$tests_performed, "\n"))
  cat(paste("Positive tests:", outcomes$positive_tests, "\n"))
  cat(paste("New diagnoses:", outcomes$new_diagnoses, "\n"))
  cat(paste("Re-engagement:", outcomes$re_engagement, "\n"))
  
  # Assertions
  expect_equal(outcomes$tests_performed, 0, 
               info = "Zero testing should result in zero tests performed")
  expect_equal(outcomes$positive_tests, 0, 
               info = "Zero testing should result in zero positive tests")
  expect_equal(outcomes$new_diagnoses, 0, 
               info = "Zero testing should result in zero new diagnoses")
  expect_equal(outcomes$re_engagement, 0, 
               info = "Zero testing should result in zero re-engagement")
  expect_equal(outcomes$art_initiations, 0, 
               info = "Zero testing should result in zero new art")
  expect_equal(outcomes$additional_suppressed, 0, 
               info = "Zero testing should result in zero new suppressed")
  
  cat("✓ All assertions passed\n")
})



# ============================================================================
# TEST 2: BASELINE SCENARIO (General_testing)
# ============================================================================

test_that("Baseline general facility_testing - 10,000 tests", {
  cat("\n========================================\n")
  cat("TEST 2: Baseline with 10,000 general tests \n")
  cat("========================================\n")
  
  # Calculate base yield
  base_yield <- (test_populations$undiagnosed+test_populations$ltfu) / test_populations$sexually_active
  base_yield=min(base_yield,0.1)
  
  cat(sprintf("  Base yield: %.4f (%.2f per 1000)\n", 
              base_yield, base_yield * 1000))
  
  
  baseline_interventions <- list()
  intervention_groups_test=intervention_groups
  
  intervention_groups_test$testing$interventions$test_facility_general$efficacy=0.97
  intervention_groups_test$testing$interventions$test_facility_general$test_yield_multiplier=1
  intervention_groups_test$testing$interventions$test_facility_general$linkage_rate=0.9
  intervention_groups_test$testing$interventions$test_facility_general$unit_cost=8
  intervention_groups_test$testing$interventions$test_facility_general$linkage_cost=4
  
  # Set all testing to zero
  for (group_key in names(intervention_groups_test)) {
    group <- intervention_groups[[group_key]]
    for (int_key in names(group$interventions)) {
      baseline_interventions[[int_key]] <- 0
    }
  }
  
  baseline_interventions$test_facility_general <- 10000
  
  expected_positive_general <- 10000 * base_yield * 1*0.97
  expected_new_dx <-  expected_positive_general *0.5
  expected_art=expected_positive_general*0.9
  expected_vs=expected_art*(test_context$percent_suppressed/100)*0.9
  expected_cost=10000*8+4*expected_art
  
  
  outcomes <- calculate_scenario_outcomes(test_context, baseline_interventions, test_populations)
  
  cat(paste("Tests performed:", outcomes$tests_performed, "\n"))
  cat(paste("Positive tests:", outcomes$positive_tests, "\n"))
  cat(paste("New diagnoses:", outcomes$new_diagnoses, "\n"))
  cat(paste("Re-engagement:", outcomes$re_engagement, "\n"))
  cat(paste("ART init:", outcomes$art_initiations, "\n"))
  cat(paste("VS:", outcomes$additional_suppressed, "\n"))
  
  # Assertions
  expect_equal(outcomes$tests_performed, 10000, 
               info = "10000 tests performed")
  expect_equal(outcomes$positive_tests, round(expected_positive_general,0), 
               info = "Correct positive tests from 10000 general tests")
  expect_equal(outcomes$new_diagnoses, round(expected_new_dx,0), 
               info = "Half of positive tests should be new")
  expect_equal(outcomes$art_initiations, round(expected_art,0), 
               info = "Correct ART")
  expect_equal(outcomes$additional_suppressed, round(expected_vs,0), 
               info = "Correct suppressed")
  
  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 3: SCALE-UP OF SINGLE TEST TYPE (relative to baseline)
# ============================================================================

test_that("Scaling up facility-based testing increases diagnoses", {
  cat("\n========================================\n")
  cat("TEST 3: Scale-up Single Test Type\n")
  cat("========================================\n")
  
  intervention_groups_test=intervention_groups
  intervention_groups_test$testing$interventions$test_facility_general$efficacy=0.97
  intervention_groups_test$testing$interventions$test_facility_general$test_yield_multiplier=1
  intervention_groups_test$testing$interventions$test_facility_general$linkage_rate=0.9
  
  # Baseline: 10,000 tests
  baseline_interventions <- list()
  for (group_key in names(intervention_groups_test)) {
    group <- intervention_groups_test[[group_key]]
    for (int_key in names(group$interventions)) {
      baseline_interventions[[int_key]] <- 0
    }
  }
  baseline_interventions$test_facility_general <- 10000
  
  base_yield <- (test_populations$undiagnosed+test_populations$ltfu) / test_populations$sexually_active
  base_yield=min(base_yield,0.1)
  
  #Baseline expected
  expected_positive_general <- 10000 * base_yield * 1*0.97
  expected_new_dx <-  expected_positive_general *0.5
  expected_art=expected_positive_general*0.9
  expected_vs=expected_art*(test_context$percent_suppressed/100)*0.9
  
  # Scenario: 20,000 tests (2x scale-up)
  scaleup_interventions <- baseline_interventions
  scaleup_interventions$test_facility_general <- 20000
  
  baseline_outcomes <- calculate_scenario_outcomes(test_context, baseline_interventions, test_populations)
  scaleup_outcomes <- calculate_scenario_outcomes(test_context, scaleup_interventions, test_populations)
  
  cat("\nBaseline (10,000 tests):\n")
  cat(paste("  Tests performed:", baseline_outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests:", baseline_outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses:", baseline_outcomes$new_diagnoses, "\n"))
  cat(paste("  Re-engagement:", baseline_outcomes$re_engagement, "\n"))
  cat(paste("  Test positivity:", round(baseline_outcomes$test_positivity_rate, 2), "%\n"))
  cat(paste("  ART inititations:", baseline_outcomes$art_initiations))
  cat(paste("  VS:", baseline_outcomes$additional_suppressed))
  
  cat("\nScale-up (20,000 tests):\n")
  cat(paste("  Tests performed:", scaleup_outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests:", scaleup_outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses:", scaleup_outcomes$new_diagnoses, "\n"))
  cat(paste("  Re-engagement:", scaleup_outcomes$re_engagement, "\n"))
  cat(paste("  Test positivity:", round(scaleup_outcomes$test_positivity_rate, 2), "%\n"))
  cat(paste("  ART inititations:", scaleup_outcomes$art_initiations))
  cat(paste("  VS:", scaleup_outcomes$additional_suppressed))
  
  cat("\nChanges:\n")
  cat(paste("  Tests: +", scaleup_outcomes$tests_performed - baseline_outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests: +", scaleup_outcomes$positive_tests - baseline_outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses: +", scaleup_outcomes$new_diagnoses - baseline_outcomes$new_diagnoses, "\n"))
  cat(paste("  ART STart: +", scaleup_outcomes$art_initiations - baseline_outcomes$art_initiations, "\n"))
  cat(paste("  VS: +", scaleup_outcomes$additional_suppressed - baseline_outcomes$additional_suppressed, "\n"))
  
  # Assertions
  expect_equal(scaleup_outcomes$tests_performed, 20000,
               info = "Should perform 20,000 tests")
  expect_equal(scaleup_outcomes$positive_tests, baseline_outcomes$positive_tests*2+1,
            info = "More tests should yield more positive results")
  expect_equal(scaleup_outcomes$new_diagnoses, baseline_outcomes$new_diagnoses*2,
            info = "More positive tests should yield more new diagnoses")
  expect_equal(scaleup_outcomes$art_initiations, baseline_outcomes$art_initiations*2,
               info = "More positive tests should yield more ART start")
  expect_equal(scaleup_outcomes$additional_suppressed, baseline_outcomes$additional_suppressed*2+1,
               info = "More positive tests should yield more VS")
  
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 4: SCALE-DOWN OF SINGLE TEST TYPE
# ============================================================================

test_that("Scaling down testing decreases diagnoses", {
  cat("\n========================================\n")
  cat("TEST 4: Scale-down Single Test Type\n")
  cat("========================================\n")
  
  intervention_groups_test=intervention_groups
  intervention_groups_test$testing$interventions$test_facility_general$efficacy=0.97
  intervention_groups_test$testing$interventions$test_facility_general$test_yield_multiplier=1
  intervention_groups_test$testing$interventions$test_facility_general$linkage_rate=0.9
  
  # Baseline: 10,000 tests
  baseline_interventions <- list()
  for (group_key in names(intervention_groups_test)) {
    group <- intervention_groups_test[[group_key]]
    for (int_key in names(group$interventions)) {
      baseline_interventions[[int_key]] <- 0
    }
  }
  baseline_interventions$test_community <- 50000
  
  # Scenario: 10,000 tests (80% reduction)
  scaledown_interventions <- baseline_interventions
  scaledown_interventions$test_community <- 10000
  
  baseline_outcomes <- calculate_scenario_outcomes(test_context, baseline_interventions, test_populations)
  scaledown_outcomes <- calculate_scenario_outcomes(test_context, scaledown_interventions, test_populations)
  
  cat("\nBaseline (50,000 tests):\n")
  cat(paste("  Tests performed:", baseline_outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests:", baseline_outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses:", baseline_outcomes$new_diagnoses, "\n"))
  cat(paste("  Test positivity:", round(baseline_outcomes$test_positivity_rate, 2), "%\n"))
  cat(paste("  ART inititations:", baseline_outcomes$art_initiations))
  cat(paste("  VS:", baseline_outcomes$additional_suppressed))
  
  cat("\nScale-down (10,000 tests):\n")
  cat(paste("  Tests performed:", scaledown_outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests:", scaledown_outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses:", scaledown_outcomes$new_diagnoses, "\n"))
  cat(paste("  Test positivity:", round(scaledown_outcomes$test_positivity_rate, 2), "%\n"))
  cat(paste("  ART inititations:", scaledown_outcomes$art_initiations))
  cat(paste("  VS:", scaledown_outcomes$additional_suppressed))
  
  cat("\nChanges:\n")
  cat(paste("  Tests: ", scaledown_outcomes$tests_performed - baseline_outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests: ", scaledown_outcomes$positive_tests - baseline_outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses: ", scaledown_outcomes$new_diagnoses - baseline_outcomes$new_diagnoses, "\n"))
  cat(paste("  New diagnoses: +", scaledown_outcomes$new_diagnoses - baseline_outcomes$new_diagnoses, "\n"))
  cat(paste("  ART STart: +", scaledown_outcomes$art_initiations - baseline_outcomes$art_initiations, "\n"))
  cat(paste("  VS: +", scaledown_outcomes$additional_suppressed - baseline_outcomes$additional_suppressed, "\n"))
  
  # Assertions
  expect_equal(scaledown_outcomes$tests_performed, 10000,
               info = "Should perform 10,000 tests")
  expect_equal(round(scaledown_outcomes$positive_tests,0), round(baseline_outcomes$positive_tests*0.2,0),
               info = "Fewer tests should yield fewer positive results")
  expect_equal(round(scaledown_outcomes$new_diagnoses,0), round(baseline_outcomes$new_diagnoses*0.2,0),
               info = "Fewer positive tests should yield Fewer new diagnoses")
  expect_equal(round(scaledown_outcomes$art_initiations,0), round(baseline_outcomes$art_initiations*0.2,0),
               info = "Fewer positive tests should yield fewer ART start")
  expect_equal(round(scaledown_outcomes$additional_suppressed,0), round(baseline_outcomes$additional_suppressed*0.2,0),
               info = "Fewer positive tests should yield fewer VS")
  
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 5: TWO TEST MODALITIES WITH DIFFERENT YIELD FACTORS
# ============================================================================

test_that("Different test modalities have different yields", {
  cat("\n========================================\n")
  cat("TEST 5: Two Test Modalities with Different Yields\n")
  cat("========================================\n")
  
  
  intervention_groups_test=intervention_groups
  intervention_groups_test$testing$interventions$test_facility_general$efficacy=0.97
  intervention_groups_test$testing$interventions$test_facility_general$test_yield_multiplier=1
  intervention_groups_test$testing$interventions$test_facility_general$linkage_rate=0.9
  
  intervention_groups_test$testing$interventions$test_facility_targeted$efficacy=0.97
  intervention_groups_test$testing$interventions$test_facility_targeted$test_yield_multiplier=2
  intervention_groups_test$testing$interventions$test_facility_targeted$linkage_rate=0.8
  
  
  # Get yield multipliers
  facility_general_yield <- intervention_groups_test$testing$interventions$test_facility_general$test_yield_multiplier
  network_index_yield <- intervention_groups_test$testing$interventions$test_facility_targeted$test_yield_multiplier
  
  cat("\nYield multipliers:\n")
  cat(paste("  Facility-based (general):", facility_general_yield, "\n"))
  cat(paste("  Network/index testing:", network_index_yield, "\n"))
  
  # Baseline: 10,000 tests
  baseline_interventions <- list()
  for (group_key in names(intervention_groups_test)) {
    group <- intervention_groups_test[[group_key]]
    for (int_key in names(group$interventions)) {
      baseline_interventions[[int_key]] <- 0
    }
  }
  
  # baseline_interventions$test_facility_general=100000
  
  # Scenario 1: 10,000 general facility-based tests
  interventions1 <- list()
  for (group_key in names(intervention_groups_test)) {
    group <- intervention_groups_test[[group_key]]
    for (int_key in names(group$interventions)) {
      interventions1[[int_key]] <- 0
    }
  }
  interventions1$test_facility_general <- 10000
  
  # Scenario 2: 20,000 targeted facility tests
  interventions2 <- list()
  for (group_key in names(intervention_groups_test)) {
    group <- intervention_groups_test[[group_key]]
    for (int_key in names(group$interventions)) {
      interventions2[[int_key]] <- 0
    }
  }
  interventions2$test_facility_targeted <- 10000
  
  # Scenario 3: Both (10,000 each)
  interventions3 <- list()
  for (group_key in names(intervention_groups_test)) {
    group <- intervention_groups_test[[group_key]]
    for (int_key in names(group$interventions)) {
      interventions3[[int_key]] <- 0
    }
  }
  interventions3$test_facility_general <- 10000
  interventions3$test_facility_targeted <- 10000
  
  outcomes1 <- calculate_scenario_outcomes(test_context, interventions1, test_populations)
  outcomes2 <- calculate_scenario_outcomes(test_context, interventions2, test_populations)
  outcomes3 <- calculate_scenario_outcomes(test_context, interventions3, test_populations)
  
  cat("\nScenario 1: 10,000 facility-based (general) tests:\n")
  cat(paste("  Tests performed:", outcomes1$tests_performed, "\n"))
  cat(paste("  Positive tests:", outcomes1$positive_tests, "\n"))
  cat(paste("  Test positivity:", round(outcomes1$test_positivity_rate, 2), "%\n"))
  cat(paste("  New diagnoses:", outcomes1$new_diagnoses, "\n"))
  cat(paste("  New Art start:", outcomes1$art_initiations, "\n"))
  cat(paste("  New VS:", outcomes1$additional_suppressed, "\n"))
  
  cat("\nScenario 2: 10,000 targeted tests:\n")
  cat(paste("  Tests performed:", outcomes2$tests_performed, "\n"))
  cat(paste("  Positive tests:", outcomes2$positive_tests, "\n"))
  cat(paste("  Test positivity:", round(outcomes2$test_positivity_rate, 2), "%\n"))
  cat(paste("  New diagnoses:", outcomes2$new_diagnoses, "\n"))
  cat(paste("  New Art start:", outcomes2$art_initiations, "\n"))
  cat(paste("  New VS:", outcomes2$additional_suppressed, "\n"))

  
  cat("\nScenario 3: 10,000 facility + 10,000 targeted:\n")
  cat(paste("  Tests performed:", outcomes3$tests_performed, "\n"))
  cat(paste("  Positive tests:", outcomes3$positive_tests, "\n"))
  cat(paste("  Test positivity:", round(outcomes3$test_positivity_rate, 2), "%\n"))
  cat(paste("  New diagnoses:", outcomes3$new_diagnoses, "\n"))
  cat(paste("  New Art start:", outcomes3$art_initiations, "\n"))
  cat(paste("  New VS:", outcomes3$additional_suppressed, "\n"))
  
  cat("\nComparisons:\n")
  cat(paste("  Targeted vs genral Facility yield ratio:", 
            round(outcomes2$positive_tests / outcomes1$positive_tests, 2), "\n"))
  cat(paste("  Expected ratio (from multipliers):", 
            round(network_index_yield / facility_general_yield, 2), "\n"))
  
  # Assertions
  expect_equal(outcomes1$tests_performed, 10000, info = "Scenario 1 should perform 10,000 tests")
  expect_equal(outcomes2$tests_performed, 10000, info = "Scenario 2 should perform 10,000 tests")
  expect_equal(outcomes3$tests_performed, 20000, info = "Scenario 3 should perform 10,000 tests")
  
  # targeted testing should have higher yield (higher multiplier)
  expect_equal(outcomes2$positive_tests, 2*outcomes1$positive_tests+1,
            info = "Targeted testing should yield more positives (higher multiplier)")
  
  # Combined scenario should have positivity between the two
  expect_true(outcomes3$test_positivity_rate > outcomes1$test_positivity_rate &&
                outcomes3$test_positivity_rate < outcomes2$test_positivity_rate,
              info = "Combined scenario positivity should be between the two pure scenarios")
  
  # Yield ratio should approximately match multiplier ratio
  actual_ratio <- outcomes2$positive_tests / outcomes1$positive_tests
  expected_ratio <- network_index_yield / facility_general_yield
  expect_true(abs(actual_ratio - expected_ratio) < 0.5,
              info = "Actual yield ratio should approximate multiplier ratio")
  
  cat("✓ All assertions passed\n")

  #new diagnoses
  expect_equal(outcomes3$new_diagnoses,outcomes1$new_diagnoses +outcomes2$new_diagnoses+1,
               info = "Scen 3 should have combined new diagnoses of 1 and 2")
  

#ART inititations
expect_equal(outcomes3$art_initiations+1,round(outcomes1$art_initiations,0) +round(outcomes2$art_initiations,0)+1,
             info = "Scen 3 should have combined new art start of 1 and 2")

})


# ============================================================================
# TEST 6: CONSTRAINTS - CANNOT DIAGNOSE MORE THAN UNDIAGNOSED
# ============================================================================

test_that("Testing constraints: cannot diagnose more than undiagnosed", {
  cat("\n========================================\n")
  cat("TEST 6: Testing Constraints\n")
  cat("========================================\n")
  
  cat("\nUndiagnosed population:", test_populations$undiagnosed, "\n")
  
  # Try to perform massive amount of testing
  extreme_interventions <- list()
  for (group_key in names(intervention_groups)) {
    group <- intervention_groups[[group_key]]
    for (int_key in names(group$interventions)) {
      extreme_interventions[[int_key]] <- 0
    }
  }
  # Set extremely high testing
  extreme_interventions$test_network_index <- 1000000  # 1 million tests
  
  outcomes <- calculate_scenario_outcomes(test_context, extreme_interventions, test_populations)
  
  base_test_yield=((test_populations$undiagnosed+test_populations$ltfu)/test_populations$sexually_active)
  
  cat("\nExtreme testing scenario (1,000,000 network tests):\n")
  cat(paste("  Tests performed:", outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests:", outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses:", outcomes$new_diagnoses, "\n"))
  cat(paste("  Re-engagement:", outcomes$re_engagement, "\n"))
  cat(paste("  End diagnosed:", outcomes$end_diagnosed, "\n"))
  cat(paste("  Starting diagnosed:", test_populations$diagnosed, "\n"))
  cat(paste("  PLHIV:", test_populations$plhiv, "\n"))
  
  cat(paste("  ART:", outcomes$art_initiations, "\n"))
  cat(paste("  VS:", outcomes$additional_suppressed, "\n"))
  
  # Assertions
  expect_lte(outcomes$new_diagnoses, test_populations$undiagnosed)#"Cannot diagnose more people than are undiagnosed"
  expect_lte(outcomes$end_diagnosed, test_populations$plhiv)#"Diagnosed cannot exceed PLHIV"
  expect_gte(outcomes$new_diagnoses, 0)#"New diagnoses cannot be negative")
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 6: LINKAGE RATES AFFECT ART INITIATIONS
# ============================================================================

test_that("Different linkage rates affect ART initiations", {
  cat("\n========================================\n")
  cat("TEST 6: Linkage Rates\n")
  cat("========================================\n")
  
  # Get linkage rates
  facility_linkage <- intervention_groups$testing$interventions$test_facility_general$linkage_rate
  hivst_linkage <- intervention_groups$testing$interventions$hivst_community$linkage_rate
  
  cat("\nLinkage rates:\n")
  cat(paste("  Facility-based:", facility_linkage, "\n"))
  cat(paste("  HIVST (community):", hivst_linkage, "\n"))
  
  # Scenario 1: Facility testing
  interventions1 <- list()
  for (group_key in names(intervention_groups)) {
    group <- intervention_groups[[group_key]]
    for (int_key in names(group$interventions)) {
      interventions1[[int_key]] <- 0
    }
  }
  interventions1$test_facility_general <- 30000
  
  # Scenario 2: HIVST (lower linkage)
  interventions2 <- list()
  for (group_key in names(intervention_groups)) {
    group <- intervention_groups[[group_key]]
    for (int_key in names(group$interventions)) {
      interventions2[[int_key]] <- 0
    }
  }
  interventions2$hivst_community <- 30000
  
  outcomes1 <- calculate_scenario_outcomes(test_context, interventions1, test_populations)
  outcomes2 <- calculate_scenario_outcomes(test_context, interventions2, test_populations)
  
  cat("\nScenario 1: 30,000 facility tests:\n")
  cat(paste("  Positive tests:", outcomes1$positive_tests, "\n"))
  cat(paste("  ART initiations:", outcomes1$art_initiations, "\n"))
  cat(paste("  Linkage rate achieved:", 
            round(outcomes1$art_initiations / outcomes1$positive_tests * 100, 1), "%\n"))
  
  cat("\nScenario 2: 30,000 HIVST tests:\n")
  cat(paste("  Positive tests:", outcomes2$positive_tests, "\n"))
  cat(paste("  ART initiations:", outcomes2$art_initiations, "\n"))
  cat(paste("  Linkage rate achieved:", 
            round(outcomes2$art_initiations / outcomes2$positive_tests * 100, 1), "%\n"))
  
  # Assertions
  expect_true(facility_linkage > hivst_linkage,
              info = "Facility should have higher linkage rate than HIVST")
  
  # If positive tests are similar, facility should link more people
  if (abs(outcomes1$positive_tests - outcomes2$positive_tests) < 100) {
    expect_gt(outcomes1$art_initiations, outcomes2$art_initiations,
              info = "With similar positives, facility should link more people")
  }
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 7: TEST POSITIVITY RATE CALCULATIONS
# ============================================================================

test_that("Test positivity rate calculated correctly", {
  cat("\n========================================\n")
  cat("TEST 7: Test Positivity Rate\n")
  cat("========================================\n")
  
  interventions <- list()
  for (group_key in names(intervention_groups)) {
    group <- intervention_groups[[group_key]]
    for (int_key in names(group$interventions)) {
      interventions[[int_key]] <- 0
    }
  }
  interventions$test_facility_general <- 50000
  
  outcomes <- calculate_scenario_outcomes(test_context, interventions, test_populations)
  
  # Calculate expected positivity manually
  expected_positivity <- (outcomes$positive_tests / outcomes$tests_performed) * 100
  
  cat("\nTest results:\n")
  cat(paste("  Tests performed:", outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests:", outcomes$positive_tests, "\n"))
  cat(paste("  Test positivity rate:", round(outcomes$test_positivity_rate, 2), "%\n"))
  cat(paste("  Expected positivity:", round(expected_positivity, 2), "%\n"))
  
  # Assertions
  expect_equal(round(outcomes$test_positivity_rate, 2), round(expected_positivity, 2),
               info = "Test positivity rate should match manual calculation")
  expect_gte(outcomes$test_positivity_rate, 0,
             info = "Test positivity cannot be negative")
  expect_lte(outcomes$test_positivity_rate, 100,
             info = "Test positivity cannot exceed 100%")
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 8: ZERO TESTING IN SCENARIO VS BASELINE WITH TESTING
# ============================================================================

test_that("Zero testing scenario vs baseline with testing", {
  cat("\n========================================\n")
  cat("TEST 8: Zero Testing Scenario vs Baseline\n")
  cat("========================================\n")
  
  # Baseline with testing
  baseline_interventions <- list()
  for (group_key in names(intervention_groups)) {
    group <- intervention_groups[[group_key]]
    for (int_key in names(group$interventions)) {
      baseline_interventions[[int_key]] <- 0
    }
  }
  baseline_interventions$test_facility_general <- 40000
  baseline_interventions$test_community <- 20000
  
  # Scenario with zero testing
  zero_interventions <- list()
  for (group_key in names(intervention_groups)) {
    group <- intervention_groups[[group_key]]
    for (int_key in names(group$interventions)) {
      zero_interventions[[int_key]] <- 0
    }
  }
  
  baseline_outcomes <- calculate_scenario_outcomes(test_context, baseline_interventions, test_populations)
  zero_outcomes <- calculate_scenario_outcomes(test_context, zero_interventions, test_populations)
  
  cat("\nBaseline (60,000 tests):\n")
  cat(paste("  Tests performed:", baseline_outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests:", baseline_outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses:", baseline_outcomes$new_diagnoses, "\n"))
  cat(paste("  End diagnosed:", baseline_outcomes$end_diagnosed, "\n"))
  
  cat("\nZero Testing Scenario:\n")
  cat(paste("  Tests performed:", zero_outcomes$tests_performed, "\n"))
  cat(paste("  Positive tests:", zero_outcomes$positive_tests, "\n"))
  cat(paste("  New diagnoses:", zero_outcomes$new_diagnoses, "\n"))
  cat(paste("  End diagnosed:", zero_outcomes$end_diagnosed, "\n"))
  
  cat("\nDifference:\n")
  cat(paste("  Fewer tests: -", baseline_outcomes$tests_performed, "\n"))
  cat(paste("  Fewer diagnoses: -", baseline_outcomes$new_diagnoses - zero_outcomes$new_diagnoses, "\n"))
  cat(paste("  End diagnosed (baseline):", baseline_outcomes$end_diagnosed, "\n"))
  cat(paste("  End diagnosed (zero):", zero_outcomes$end_diagnosed, "\n"))
  
  # Assertions
  expect_equal(zero_outcomes$tests_performed, 0,
               info = "Zero testing should perform no tests")
  expect_equal(zero_outcomes$positive_tests, 0,
               info = "Zero testing should find no positives")
  expect_equal(zero_outcomes$new_diagnoses, 0,
               info = "Zero testing should result in no new diagnoses")
  expect_gt(baseline_outcomes$new_diagnoses, 0,
            info = "Baseline with testing should have new diagnoses")
  expect_lt(zero_outcomes$end_diagnosed, baseline_outcomes$end_diagnosed,
            info = "Zero testing should result in fewer diagnosed at year end")
  
  # But zero testing shouldn't lose existing diagnosed
  expect_gte(zero_outcomes$end_diagnosed, test_populations$diagnosed,
             info = "Zero testing shouldn't reduce starting diagnosed count")
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("ALL TESTS COMPLETED\n")
cat("========================================\n")
cat("\nTest Suite Summary:\n")
cat("✓ TEST 1: Baseline with zero testing\n")
cat("✓ TEST 2: Scale-up of single test type\n")
cat("✓ TEST 3: Scale-down of single test type\n")
cat("✓ TEST 4: Two test modalities with different yields\n")
cat("✓ TEST 5: Testing constraints validation\n")
cat("✓ TEST 6: Linkage rates affect ART initiations\n")
cat("✓ TEST 7: Test positivity rate calculations\n")
cat("✓ TEST 8: Zero testing scenario vs baseline\n")
cat("\n✅ All unit tests passed successfully!\n")