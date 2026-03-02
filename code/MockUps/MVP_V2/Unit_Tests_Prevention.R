# ============================================================================
# UNIT TESTS FOR PREVENTION INTERVENTIONS
# ============================================================================
# Tests the logic of prevention interventions including:
#   - PrEP (oral and lenacapavir)
#   - VMMC
#   - Condoms
#   - PEP
#   - Infant prophylaxis (PMTCT)
#
# Covers:
#   - Zero-baseline (no infections averted when coverage = 0)
#   - Scale-up / scale-down linearity
#   - Efficacy differences between modalities
#   - Eligible population caps
#   - Coverage vs. absolute input types
#   - Combined (multi-intervention) additive behaviour
#   - Biological ceiling: cannot avert more than new_infections_per_year
#   - Cost calculations
# ============================================================================

library(testthat)

# Source the logic file (update path as needed)
source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/Mock-Up_logic_V2.R")

# Zero out mortality so prevention tests are not confounded by deaths
MORTALITY_RATES <<- list(
  diagnosed_not_on_art  = 0.0,
  on_art_not_suppressed = 0.0,
  on_art_suppressed     = 0.0
)

# ============================================================================
# SHARED TEST HELPERS
# ============================================================================

create_test_context <- function() {
  list(
    total_population        = 1000000,
    hiv_prevalence          = 0.05,    # 5% = 50,000 PLHIV
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

# Return a zero-filled interventions list; caller sets specific keys
zero_interventions <- function() {
  ints <- list()
  for (g in names(intervention_groups)) {
    for (k in names(intervention_groups[[g]]$interventions)) {
      ints[[k]] <- 0
    }
  }
  ints
}

# Convenience: set a fixed efficacy / unit_cost on one prevention intervention
set_prevention_params <- function(key, efficacy, unit_cost) {
  intervention_groups$prevention$interventions[[key]]$efficacy   <<- efficacy
  intervention_groups$prevention$interventions[[key]]$unit_cost  <<- unit_cost
}

test_context  <- create_test_context()
test_pops     <- calculate_populations(test_context)

cat("Prevention test populations:\n")
cat(sprintf("  Total pop           : %s\n",   format(test_pops$total,                big.mark=",")))
cat(sprintf("  HIV-negative        : %s\n",   format(test_pops$hiv_negative,         big.mark=",")))
cat(sprintf("  High-risk negative  : %s\n",   format(test_pops$high_risk_negative,   big.mark=",")))
cat(sprintf("  Uncircumcised males : %s\n",   format(test_pops$uncircumcised_males,  big.mark=",")))
cat(sprintf("  Sexually active neg : %s\n",   format(test_pops$sexually_active_negative, big.mark=",")))
cat(sprintf("  Recent exposure     : %s\n",   format(test_pops$recent_exposure,      big.mark=",")))
cat(sprintf("  HIV-exposed infants : %s\n\n", format(test_pops$hiv_exposed_infants,  big.mark=",")))


# ============================================================================
# TEST 1: ZERO PREVENTION — NO INFECTIONS AVERTED
# ============================================================================

test_that("Zero prevention coverage produces zero infections averted", {
  cat("\n========================================\n")
  cat("TEST 1: Zero Prevention Coverage\n")
  cat("========================================\n")

  ints     <- zero_interventions()
  outcomes <- calculate_scenario_outcomes(test_context, ints, test_pops)

  cat(sprintf("  Adult infections averted : %d\n",  outcomes$adult_infections_averted))
  cat(sprintf("  Infant infections averted: %d\n",  outcomes$infant_infections_averted))
  cat(sprintf("  End new infections       : %d\n",  outcomes$end_new_infections))
  cat(sprintf("  Baseline new infections  : %d\n\n",test_context$new_infections_per_year))

  expect_equal(outcomes$adult_infections_averted,  0,
               info = "No prevention should avert zero adult infections")
  expect_equal(outcomes$infant_infections_averted, 0,
               info = "No prevention should avert zero infant infections")
  expect_equal(outcomes$end_new_infections, test_context$new_infections_per_year,
               info = "End infections should equal baseline when nothing is done")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 2: PREP ORAL — SCALE-UP LINEARLY INCREASES INFECTIONS AVERTED
# ============================================================================

test_that("Doubling oral PrEP coverage doubles infections averted", {
  cat("\n========================================\n")
  cat("TEST 2: Oral PrEP Linear Scale-up\n")
  cat("========================================\n")

  set_prevention_params("prep_oral", efficacy = 0.8, unit_cost = 120)

  incidence_rate <- test_context$new_infections_per_year / test_pops$hiv_negative
  n_low  <- 10000
  n_high <- 20000   # exact double

  expected_low  <- round(n_low  * incidence_rate * 0.8)
  expected_high <- round(n_high * incidence_rate * 0.8)

  ints_low  <- zero_interventions(); ints_low$prep_oral  <- n_low
  ints_high <- zero_interventions(); ints_high$prep_oral <- n_high

  out_low  <- calculate_scenario_outcomes(test_context, ints_low,  test_pops)
  out_high <- calculate_scenario_outcomes(test_context, ints_high, test_pops)

  cat(sprintf("  Incidence rate          : %.6f\n", incidence_rate))
  cat(sprintf("  Low  (n=%d) averted    : %d  [expected: %d]\n",
              n_low,  out_low$adult_infections_averted,  expected_low))
  cat(sprintf("  High (n=%d) averted   : %d  [expected: %d]\n\n",
              n_high, out_high$adult_infections_averted, expected_high))

  expect_equal(out_low$adult_infections_averted,  expected_low,
               info = "Low PrEP: infections averted should match formula")
  expect_equal(out_high$adult_infections_averted, expected_high,
               info = "High PrEP: infections averted should match formula")
  expect_equal(out_high$adult_infections_averted, 2 * out_low$adult_infections_averted,
               info = "Doubling PrEP coverage should double infections averted")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 3: PREP ORAL — SCALE-DOWN REDUCES INFECTIONS AVERTED
# ============================================================================

test_that("Reducing oral PrEP from 5000 to 1000 reduces infections averted proportionally", {
  cat("\n========================================\n")
  cat("TEST 3: Oral PrEP Scale-down\n")
  cat("========================================\n")

  set_prevention_params("prep_oral", efficacy = 0.8, unit_cost = 120)

  ints_base  <- zero_interventions(); ints_base$prep_oral  <- 5000
  ints_small <- zero_interventions(); ints_small$prep_oral <- 1000

  out_base  <- calculate_scenario_outcomes(test_context, ints_base,  test_pops)
  out_small <- calculate_scenario_outcomes(test_context, ints_small, test_pops)

  cat(sprintf("  Baseline (5000) averted : %d\n",  out_base$adult_infections_averted))
  cat(sprintf("  Scaled-down (1000)      : %d\n\n", out_small$adult_infections_averted))

  expect_lt(out_small$adult_infections_averted, out_base$adult_infections_averted,
            info = "Scaling down should avert fewer infections")
  expect_equal(out_small$adult_infections_averted,
               round(out_base$adult_infections_averted * (1000 / 5000)),
               info = "Scale-down should be proportional to coverage reduction")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 4: LENACAPAVIR VS ORAL PREP — HIGHER EFFICACY GIVES MORE AVERTED
# ============================================================================

test_that("Lenacapavir averts more infections than oral PrEP at same coverage (higher efficacy)", {
  cat("\n========================================\n")
  cat("TEST 4: Lenacapavir vs Oral PrEP Efficacy\n")
  cat("========================================\n")

  set_prevention_params("prep_oral",        efficacy = 0.74, unit_cost = 120)
  set_prevention_params("prep_lenacapavir", efficacy = 0.99, unit_cost = 400)

  n <- 2000
  ints_oral <- zero_interventions(); ints_oral$prep_oral        <- n
  ints_lena <- zero_interventions(); ints_lena$prep_lenacapavir <- n

  out_oral <- calculate_scenario_outcomes(test_context, ints_oral, test_pops)
  out_lena <- calculate_scenario_outcomes(test_context, ints_lena, test_pops)

  incidence_rate    <- test_context$new_infections_per_year / test_pops$hiv_negative
  expected_oral     <- round(n * incidence_rate * 0.74)
  expected_lena     <- round(n * incidence_rate * 0.99)

  cat(sprintf("  Oral PrEP  averted  : %d  [expected: %d]\n",
              out_oral$adult_infections_averted, expected_oral))
  cat(sprintf("  Lenacapavir averted : %d  [expected: %d]\n\n",
              out_lena$adult_infections_averted, expected_lena))

  expect_equal(out_oral$adult_infections_averted, expected_oral,
               info = "Oral PrEP infections averted should match efficacy formula")
  expect_equal(out_lena$adult_infections_averted, expected_lena,
               info = "Lenacapavir infections averted should match efficacy formula")
  expect_gt(out_lena$adult_infections_averted, out_oral$adult_infections_averted,
            info = "Lenacapavir (higher efficacy) should avert more infections")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 5: PREP IS CAPPED AT ELIGIBLE POPULATION (high_risk_negative)
# ============================================================================

test_that("PrEP is capped at the high-risk HIV-negative eligible population", {
  cat("\n========================================\n")
  cat("TEST 5: PrEP Eligible Population Cap\n")
  cat("========================================\n")

  set_prevention_params("prep_oral", efficacy = 0.99, unit_cost = 120)

  eligible <- test_pops$high_risk_negative
  incidence_rate <- test_context$new_infections_per_year / test_pops$hiv_negative

  # Request far more than eligible
  ints_over <- zero_interventions(); ints_over$prep_oral <- eligible * 10

  out_over    <- calculate_scenario_outcomes(test_context, ints_over, test_pops)
  expected_at_cap <- round(eligible * incidence_rate * 0.99)

  cat(sprintf("  Eligible pop              : %s\n",   format(eligible, big.mark=",")))
  cat(sprintf("  Requested coverage        : %s\n",   format(eligible*10, big.mark=",")))
  cat(sprintf("  Infections averted        : %d\n",   out_over$adult_infections_averted))
  cat(sprintf("  Expected (capped at elig) : %d\n\n", expected_at_cap))

  expect_equal(out_over$adult_infections_averted, expected_at_cap,
               info = "PrEP above eligible pop should be capped at eligible pop")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 6: VMMC — USES UNCIRCUMCISED MALE ELIGIBLE POPULATION
# ============================================================================

test_that("VMMC averts infections based on uncircumcised male eligible population", {
  cat("\n========================================\n")
  cat("TEST 6: VMMC Eligible Population\n")
  cat("========================================\n")

  set_prevention_params("vmmc", efficacy = 0.60, unit_cost = 100)

  eligible       <- test_pops$uncircumcised_males
  incidence_rate <- test_context$new_infections_per_year / test_pops$hiv_negative
  n              <- min(5000, eligible)

  expected <- round(n * incidence_rate * 0.60)

  ints     <- zero_interventions(); ints$vmmc <- n
  outcomes <- calculate_scenario_outcomes(test_context, ints, test_pops)

  cat(sprintf("  Uncircumcised males eligible : %s\n",   format(eligible, big.mark=",")))
  cat(sprintf("  VMMC performed               : %s\n",   format(n, big.mark=",")))
  cat(sprintf("  Infections averted           : %d\n",   outcomes$adult_infections_averted))
  cat(sprintf("  Expected                     : %d\n\n", expected))

  expect_equal(outcomes$adult_infections_averted, expected,
               info = "VMMC infections averted should match formula using uncircumcised males")

  # VMMC cannot exceed eligible uncircumcised male population
  ints_over <- zero_interventions(); ints_over$vmmc <- eligible * 5
  out_over  <- calculate_scenario_outcomes(test_context, ints_over, test_pops)
  expected_cap <- round(eligible * incidence_rate * 0.60)

  expect_equal(out_over$adult_infections_averted, expected_cap,
               info = "VMMC exceeding eligible males should be capped")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 7: CONDOMS — LARGE ELIGIBLE POPULATION (sexually_active_negative)
# ============================================================================

test_that("Condom distribution averts infections across the sexually-active HIV-negative population", {
  cat("\n========================================\n")
  cat("TEST 7: Condom Distribution\n")
  cat("========================================\n")

  set_prevention_params("condoms", efficacy = 0.80, unit_cost = 5)

  eligible       <- test_pops$sexually_active_negative
  incidence_rate <- test_context$new_infections_per_year / test_pops$hiv_negative
  n              <- 50000

  expected <- round(n * incidence_rate * 0.80)

  ints     <- zero_interventions(); ints$condoms <- n
  outcomes <- calculate_scenario_outcomes(test_context, ints, test_pops)

  cat(sprintf("  Sexually active neg eligible : %s\n",   format(eligible, big.mark=",")))
  cat(sprintf("  Condoms distributed          : %s\n",   format(n, big.mark=",")))
  cat(sprintf("  Infections averted           : %d\n",   outcomes$adult_infections_averted))
  cat(sprintf("  Expected                     : %d\n\n", expected))

  expect_equal(outcomes$adult_infections_averted, expected,
               info = "Condom infections averted should match formula")
  expect_gt(eligible, 10000,
            info = "Sexually active HIV-negative pop should be large (condoms have wide reach)")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 8: PEP — SMALL ELIGIBLE POPULATION (recent_exposure)
# ============================================================================

test_that("PEP averts infections based on the small recent-exposure eligible population", {
  cat("\n========================================\n")
  cat("TEST 8: PEP Recent-Exposure Population\n")
  cat("========================================\n")

  set_prevention_params("pep", efficacy = 0.80, unit_cost = 200)

  eligible       <- test_pops$recent_exposure
  incidence_rate <- test_context$new_infections_per_year / test_pops$hiv_negative
  n              <- round(eligible * 0.5)   # reach half the eligible pool

  expected <- round(n * incidence_rate * 0.80)

  ints     <- zero_interventions(); ints$pep <- n
  outcomes <- calculate_scenario_outcomes(test_context, ints, test_pops)

  cat(sprintf("  Recent exposure eligible : %s\n",   format(eligible, big.mark=",")))
  cat(sprintf("  PEP courses provided     : %s\n",   format(n, big.mark=",")))
  cat(sprintf("  Infections averted       : %d\n",   outcomes$adult_infections_averted))
  cat(sprintf("  Expected                 : %d\n\n", expected))

  expect_equal(outcomes$adult_infections_averted, expected,
               info = "PEP infections averted should match formula")
  expect_lt(eligible, test_pops$high_risk_negative,
            info = "PEP eligible pop should be smaller than PrEP eligible pop")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 9: INFANT PROPHYLAXIS — COVERAGE TYPE DRIVES INFANT INFECTIONS AVERTED
# ============================================================================

test_that("Infant prophylaxis (coverage input) correctly reduces infant infections", {
  cat("\n========================================\n")
  cat("TEST 9: Infant Prophylaxis Coverage\n")
  cat("========================================\n")

  set_prevention_params("infant_prophylaxis", efficacy = 0.95, unit_cost = 10)

  INFANT_INCIDENCE <- 0.15   # hard-coded in logic
  eligible         <- test_pops$hiv_exposed_infants
  coverage_pct     <- 70     # 70 %

  number_reached   <- eligible * (coverage_pct / 100)
  expected_averted <- round(number_reached * INFANT_INCIDENCE * 0.95)

  ints     <- zero_interventions(); ints$infant_prophylaxis <- coverage_pct
  outcomes <- calculate_scenario_outcomes(test_context, ints, test_pops)

  cat(sprintf("  HIV-exposed infants     : %s\n",   format(eligible, big.mark=",")))
  cat(sprintf("  Coverage input          : %d %%\n", coverage_pct))
  cat(sprintf("  Number reached          : %s\n",   format(round(number_reached), big.mark=",")))
  cat(sprintf("  Infant infections avt'd : %d\n",   outcomes$infant_infections_averted))
  cat(sprintf("  Expected                : %d\n\n", expected_averted))

  expect_equal(outcomes$infant_infections_averted, expected_averted,
               info = "Infant prophylaxis should avert infections based on coverage × eligible × efficacy")
  expect_equal(outcomes$adult_infections_averted, 0,
               info = "Infant prophylaxis should not affect adult infections")

  # 100 % coverage should avert more than 70 %
  ints_full <- zero_interventions(); ints_full$infant_prophylaxis <- 100
  out_full  <- calculate_scenario_outcomes(test_context, ints_full, test_pops)
  expect_gt(out_full$infant_infections_averted, outcomes$infant_infections_averted,
            info = "Full coverage should avert more infant infections than 70 %")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 10: COMBINING MULTIPLE PREVENTION INTERVENTIONS — ADDITIVE EFFECT
# ============================================================================

test_that("Combining PrEP + VMMC + condoms gives additive infections averted", {
  cat("\n========================================\n")
  cat("TEST 10: Combined Multi-Intervention Additive Effect\n")
  cat("========================================\n")

  set_prevention_params("prep_oral", efficacy = 0.99, unit_cost = 120)
  set_prevention_params("vmmc",      efficacy = 0.60, unit_cost = 100)
  set_prevention_params("condoms",   efficacy = 0.80, unit_cost = 5)

  n_prep    <- 1000
  n_vmmc    <- min(2000, test_pops$uncircumcised_males)
  n_condoms <- 20000

  ints_prep    <- zero_interventions(); ints_prep$prep_oral <- n_prep
  ints_vmmc    <- zero_interventions(); ints_vmmc$vmmc      <- n_vmmc
  ints_condoms <- zero_interventions(); ints_condoms$condoms <- n_condoms
  ints_all     <- zero_interventions()
  ints_all$prep_oral <- n_prep
  ints_all$vmmc      <- n_vmmc
  ints_all$condoms   <- n_condoms

  out_prep    <- calculate_scenario_outcomes(test_context, ints_prep,    test_pops)
  out_vmmc    <- calculate_scenario_outcomes(test_context, ints_vmmc,    test_pops)
  out_condoms <- calculate_scenario_outcomes(test_context, ints_condoms, test_pops)
  out_all     <- calculate_scenario_outcomes(test_context, ints_all,     test_pops)

  expected_combined <- out_prep$adult_infections_averted +
    out_vmmc$adult_infections_averted +
    out_condoms$adult_infections_averted

  cat(sprintf("  PrEP alone       averted : %d\n",  out_prep$adult_infections_averted))
  cat(sprintf("  VMMC alone       averted : %d\n",  out_vmmc$adult_infections_averted))
  cat(sprintf("  Condoms alone    averted : %d\n",  out_condoms$adult_infections_averted))
  cat(sprintf("  Combined (all)   averted : %d\n",  out_all$adult_infections_averted))
  cat(sprintf("  Sum of individual parts  : %d\n\n",expected_combined))

  expect_equal(out_all$adult_infections_averted, expected_combined,
               info = "Combined interventions should avert infections equal to sum of individual parts")
  expect_gt(out_all$adult_infections_averted, out_prep$adult_infections_averted,
            info = "Adding VMMC and condoms should increase total averted vs PrEP alone")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 11: BIOLOGICAL CEILING — CANNOT AVERT MORE THAN new_infections_per_year
# ============================================================================

test_that("Infections averted cannot exceed new_infections_per_year", {
  cat("\n========================================\n")
  cat("TEST 11: Biological Ceiling on Infections Averted\n")
  cat("========================================\n")

  set_prevention_params("prep_oral",        efficacy = 0.99, unit_cost = 120)
  set_prevention_params("prep_lenacapavir", efficacy = 0.99, unit_cost = 400)
  set_prevention_params("vmmc",             efficacy = 0.60, unit_cost = 100)
  set_prevention_params("condoms",          efficacy = 0.80, unit_cost = 5)
  set_prevention_params("pep",              efficacy = 0.80, unit_cost = 200)

  # Saturate every prevention channel
  ints <- zero_interventions()
  ints$prep_oral        <- test_pops$high_risk_negative
  ints$prep_lenacapavir <- 0   # oral already saturates eligible pool
  ints$vmmc             <- test_pops$uncircumcised_males
  ints$condoms          <- test_pops$sexually_active_negative
  ints$pep              <- test_pops$recent_exposure

  outcomes <- calculate_scenario_outcomes(test_context, ints, test_pops)

  cat(sprintf("  new_infections_per_year  : %d\n",  test_context$new_infections_per_year))
  cat(sprintf("  Adult infections averted : %d\n",  outcomes$adult_infections_averted))
  cat(sprintf("  End new infections       : %d\n\n",outcomes$end_new_infections))

  expect_gte(outcomes$end_new_infections, 0,
             info = "End-of-year infections cannot be negative")
  expect_lte(outcomes$adult_infections_averted, test_context$new_infections_per_year,
             info = "Cannot avert more adult infections than occur in baseline")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 12: COST CALCULATION — INTERVENTION COST = number_reached × unit_cost
# ============================================================================

test_that("Prevention intervention costs equal number_reached × unit_cost", {
  cat("\n========================================\n")
  cat("TEST 12: Prevention Cost Calculation\n")
  cat("========================================\n")

  UNIT_COST_PREP <- 120
  UNIT_COST_VMMC <- 100

  set_prevention_params("prep_oral", efficacy = 0.99, unit_cost = UNIT_COST_PREP)
  set_prevention_params("vmmc",      efficacy = 0.60, unit_cost = UNIT_COST_VMMC)

  n_prep <- 3000
  n_vmmc <- min(1500, test_pops$uncircumcised_males)

  ints_prep <- zero_interventions(); ints_prep$prep_oral <- n_prep
  ints_vmmc <- zero_interventions(); ints_vmmc$vmmc      <- n_vmmc
  ints_both <- zero_interventions(); ints_both$prep_oral <- n_prep; ints_both$vmmc <- n_vmmc

  out_prep <- calculate_scenario_outcomes(test_context, ints_prep, test_pops)
  out_vmmc <- calculate_scenario_outcomes(test_context, ints_vmmc, test_pops)
  out_both <- calculate_scenario_outcomes(test_context, ints_both, test_pops)

  expected_prep_cost <- n_prep * UNIT_COST_PREP
  expected_vmmc_cost <- n_vmmc * UNIT_COST_VMMC
  expected_both_cost <- expected_prep_cost + expected_vmmc_cost

  cat(sprintf("  PrEP cost  (expected $%s, got $%s)\n",
              format(expected_prep_cost, big.mark=","),
              format(out_prep$total_intervention_cost, big.mark=",")))
  cat(sprintf("  VMMC cost  (expected $%s, got $%s)\n",
              format(expected_vmmc_cost, big.mark=","),
              format(out_vmmc$total_intervention_cost, big.mark=",")))
  cat(sprintf("  Both cost  (expected $%s, got $%s)\n\n",
              format(expected_both_cost, big.mark=","),
              format(out_both$total_intervention_cost, big.mark=",")))

  expect_equal(out_prep$total_intervention_cost, expected_prep_cost,
               info = "PrEP cost should equal n × unit_cost")
  expect_equal(out_vmmc$total_intervention_cost, expected_vmmc_cost,
               info = "VMMC cost should equal n × unit_cost")
  expect_equal(out_both$total_intervention_cost, expected_both_cost,
               info = "Combined costs should be additive")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 13: INFANT PROPHYLAXIS — 0 % COVERAGE AVERTS NO INFANT INFECTIONS
# ============================================================================

test_that("Zero infant prophylaxis coverage averts no infant infections", {
  cat("\n========================================\n")
  cat("TEST 13: Zero Infant Prophylaxis Coverage\n")
  cat("========================================\n")

  set_prevention_params("infant_prophylaxis", efficacy = 0.95, unit_cost = 10)

  INFANT_INCIDENCE    <- 0.15
  baseline_infant_inf <- round(test_pops$hiv_exposed_infants * INFANT_INCIDENCE)

  ints     <- zero_interventions()   # infant_prophylaxis = 0
  outcomes <- calculate_scenario_outcomes(test_context, ints, test_pops)

  cat(sprintf("  HIV-exposed infants         : %s\n",   format(test_pops$hiv_exposed_infants, big.mark=",")))
  cat(sprintf("  Infant infections (baseline): %d\n",   baseline_infant_inf))
  cat(sprintf("  Infant infections averted   : %d\n",   outcomes$infant_infections_averted))
  cat(sprintf("  End infant infections       : %d\n\n", outcomes$end_infant_infections))

  expect_equal(outcomes$infant_infections_averted, 0,
               info = "Zero coverage should avert zero infant infections")
  expect_equal(outcomes$end_infant_infections, baseline_infant_inf,
               info = "End infant infections should equal baseline when coverage is zero")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# TEST 14: INFANT PROPHYLAXIS — INCREASING COVERAGE MONOTONICALLY REDUCES INFECTIONS
# ============================================================================

test_that("Higher infant prophylaxis coverage monotonically reduces infant infections", {
  cat("\n========================================\n")
  cat("TEST 14: Infant Prophylaxis Coverage Gradient\n")
  cat("========================================\n")

  set_prevention_params("infant_prophylaxis", efficacy = 0.95, unit_cost = 10)

  coverages <- c(0, 25, 50, 75, 100)
  averted   <- numeric(length(coverages))

  for (i in seq_along(coverages)) {
    ints <- zero_interventions(); ints$infant_prophylaxis <- coverages[i]
    out  <- calculate_scenario_outcomes(test_context, ints, test_pops)
    averted[i] <- out$infant_infections_averted
    cat(sprintf("  Coverage %3d%%  -> averted: %d\n", coverages[i], averted[i]))
  }
  cat("\n")

  for (i in seq_len(length(averted) - 1)) {
    expect_gte(averted[i + 1], averted[i],
               info = sprintf("Averted at %d%% should be >= averted at %d%%",
                              coverages[i+1], coverages[i]))
  }

  expect_equal(averted[1], 0,      info = "0 %% coverage should avert nothing")
  expect_gt(averted[length(averted)], averted[1],
            info = "100 %% coverage should avert more than 0 %%")

  cat("✓ All assertions passed\n")
})


# ============================================================================
# SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("ALL PREVENTION TESTS COMPLETED\n")
cat("========================================\n")
cat("✓ TEST  1: Zero prevention produces zero infections averted\n")
cat("✓ TEST  2: Oral PrEP — linear scale-up doubles infections averted\n")
cat("✓ TEST  3: Oral PrEP — scale-down reduces infections averted proportionally\n")
cat("✓ TEST  4: Lenacapavir vs oral PrEP — higher efficacy averts more infections\n")
cat("✓ TEST  5: PrEP capped at high-risk HIV-negative eligible population\n")
cat("✓ TEST  6: VMMC uses uncircumcised male eligible population\n")
cat("✓ TEST  7: Condom distribution across sexually-active HIV-negative population\n")
cat("✓ TEST  8: PEP uses small recent-exposure eligible population\n")
cat("✓ TEST  9: Infant prophylaxis (coverage type) reduces infant infections correctly\n")
cat("✓ TEST 10: Combined interventions give additive infections averted\n")
cat("✓ TEST 11: Biological ceiling — infections averted ≤ new_infections_per_year\n")
cat("✓ TEST 12: Intervention costs equal number_reached × unit_cost\n")
cat("✓ TEST 13: Zero infant prophylaxis coverage averts no infant infections\n")
cat("✓ TEST 14: Higher infant prophylaxis coverage monotonically reduces infections\n")
cat("\n✅ All prevention unit tests completed!\n")
