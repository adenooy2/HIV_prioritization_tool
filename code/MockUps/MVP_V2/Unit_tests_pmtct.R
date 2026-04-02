# ============================================================================
# UNIT TESTS FOR PMTCT, INFANT INFECTION, EID, ANC/PNC & INFANT MORTALITY
# ============================================================================
# Tests the PMTCT and infant cascade logic including:
#   - PMTCT sub-populations in calculate_populations()
#   - pregnant_hiv_testable excludes already-diagnosed HIV+ women
#   - ANC testing eligible pool and PMTCT diagnosis pathway
#   - PNC testing deducts only HIV+ undiagnosed women caught at ANC
#   - HIV-negative women remain fully eligible for PNC after ANC
#   - ANC VL testing shifts unsuppressed -> suppressed pregnant women
#   - PMTCT linkage and suppression rates applied to newly diagnosed women
#   - MTCT cascade stratified rates drive end_infant_infections
#   - Infant prophylaxis reduces residual transmission using CSV efficacy
#   - EID cost applies to all HIV-exposed infants; effect only to infected
#   - EID diagnosis count uses actual post-cascade yield
#   - Infant mortality cascade: linkage and suppression from EID
#   - Infant deaths averted wired into total deaths_averted and end_deaths
#   - Full combination of PMTCT interventions reduces infant infections
#   - Cascade consistency checks
# ============================================================================

library(testthat)

source("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/Mock-Up_logic_V2.R")

# ============================================================================
# HELPERS
# ============================================================================

# Standard test context (1M population, 5% prevalence, birth_rate = 25)
# Hand-computed reference values:
#   births                     = 1,000,000 × 25/1000       = 25,000
#   hiv_positive_births        = 25,000 × 0.05 × 1.5       = 1,875
#   pregnant_hiv_pos_cascade   = 25,000 × 0.05             = 1,250
#   pregnant_on_art            = 1,250 × 0.80 × 0.75       = 750
#   pregnant_on_art_suppressed = 750 × 0.85                = 637.5
#   pregnant_on_art_unsuppressed = 750 × 0.15              = 112.5
#   pregnant_not_on_art        = 1,250 × (1 - 0.80×0.75)  = 1,250 × 0.40 = 500
#   pregnant_undiagnosed       = 1,250 × (1 - 0.80)       = 250
#   pregnant_hiv_testable      = 25,000 × (1 - 0.05×0.80) = 25,000 × 0.96 = 24,000
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
    prop_pop_under_14       = 40,
    circ_prevalence         = 60,
    prop_high_risk          = 0.05,
    rr_high                 = 8.0
  )
}

zero_interventions <- function() {
  ints <- list()
  for (g in names(intervention_groups))
    for (k in names(intervention_groups[[g]]$interventions))
      ints[[k]] <- 0
  ints
}

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
  INFANT_MORTALITY_RATES <<- list(untreated = 0, on_art = 0, suppressed = 0)
}

restore_infant_mortality <- function() {
  INFANT_MORTALITY_RATES <<- list(untreated = 0.35, on_art = 0.10, suppressed = 0.03)
}

# Set PMTCT intervention parameters directly on intervention_groups
set_pmtct_params <- function(anc_eff       = 0.99,
                             anc_link      = 0.85,
                             anc_link_cost = 100,
                             anc_cost      = 5,
                             pnc_eff       = 0.99,
                             pnc_link      = 0.85,
                             pnc_link_cost = 100,
                             pnc_cost      = 5,
                             anc_vl_eff    = 0.40,
                             anc_vl_cost   = 10,
                             inf_prophy_eff = 0.54,
                             inf_prophy_cost = 15,
                             eid_eff        = 0.95,
                             eid_link       = 0.80,
                             eid_link_cost  = 200,
                             eid_cost       = 20) {
  intervention_groups$testing$interventions$anc_hiv_testing$efficacy      <<- anc_eff
  intervention_groups$testing$interventions$anc_hiv_testing$linkage_rate  <<- anc_link
  intervention_groups$testing$interventions$anc_hiv_testing$linkage_cost  <<- anc_link_cost
  intervention_groups$testing$interventions$anc_hiv_testing$unit_cost     <<- anc_cost
  intervention_groups$testing$interventions$pnc_hiv_testing$efficacy      <<- pnc_eff
  intervention_groups$testing$interventions$pnc_hiv_testing$linkage_rate  <<- pnc_link
  intervention_groups$testing$interventions$pnc_hiv_testing$linkage_cost  <<- pnc_link_cost
  intervention_groups$testing$interventions$pnc_hiv_testing$unit_cost     <<- pnc_cost
  intervention_groups$treatment_monitoring$interventions$anc_vl_testing$efficacy  <<- anc_vl_eff
  intervention_groups$treatment_monitoring$interventions$anc_vl_testing$unit_cost <<- anc_vl_cost
  intervention_groups$prevention$interventions$infant_prophylaxis$efficacy  <<- inf_prophy_eff
  intervention_groups$prevention$interventions$infant_prophylaxis$unit_cost <<- inf_prophy_cost
  intervention_groups$testing$interventions$eid$efficacy      <<- eid_eff
  intervention_groups$testing$interventions$eid$linkage_rate  <<- eid_link
  intervention_groups$testing$interventions$eid$linkage_cost  <<- eid_link_cost
  intervention_groups$testing$interventions$eid$unit_cost     <<- eid_cost
}

ctx  <- make_context()
pops <- calculate_populations(ctx)
zero_mortality()
set_pmtct_params()

cat(sprintf("PMTCT population reference values:\n"))
cat(sprintf("  births:                      %g\n", pops$pregnant_women))
cat(sprintf("  hiv_exposed_infants:         %g\n", pops$hiv_exposed_infants))
cat(sprintf("  pregnant_hiv_pos_cascade:    %g\n", pops$pregnant_hiv_pos_cascade))
cat(sprintf("  pregnant_on_art:             %g\n", pops$pregnant_on_art))
cat(sprintf("  pregnant_on_art_suppressed:  %g\n", pops$pregnant_on_art_suppressed))
cat(sprintf("  pregnant_on_art_unsuppressed:%g\n", pops$pregnant_on_art_unsuppressed))
cat(sprintf("  pregnant_not_on_art:         %g\n", pops$pregnant_not_on_art))
cat(sprintf("  pregnant_undiagnosed:        %g\n", pops$pregnant_undiagnosed))
cat(sprintf("  pregnant_hiv_testable:       %g\n", pops$pregnant_hiv_testable))

# ============================================================================
# TEST 1: PMTCT POPULATIONS IN CALCULATE_POPULATIONS
# ============================================================================

test_that("PMTCT sub-populations are internally consistent", {
  cat("\n========================================\n")
  cat("TEST 1: PMTCT Sub-Populations in calculate_populations()\n")
  cat("========================================\n")
  
  # pregnant_on_art_suppressed + unsuppressed = pregnant_on_art
  expect_equal(
    pops$pregnant_on_art_suppressed + pops$pregnant_on_art_unsuppressed,
    pops$pregnant_on_art,
    tolerance = 1,
    info = "Suppressed + unsuppressed on ART must equal total pregnant_on_art"
  )
  
  # pregnant_on_art + pregnant_not_on_art = pregnant_hiv_pos_cascade
  expect_equal(
    pops$pregnant_on_art + pops$pregnant_not_on_art,
    pops$pregnant_hiv_pos_cascade,
    tolerance = 1,
    info = "On ART + not on ART must equal total HIV+ pregnant women"
  )
  
  # pregnant_undiagnosed is a subset of pregnant_not_on_art
  expect_lte(
    pops$pregnant_undiagnosed, pops$pregnant_not_on_art,
    label = "Undiagnosed cannot exceed not_on_art (undiagnosed is a sub-group)"
  )
  
  # pregnant_hiv_testable = HIV-neg pregnant + HIV+ undiagnosed pregnant
  expected_testable <- pops$pregnant_women - 
    (pops$pregnant_hiv_pos_cascade - pops$pregnant_undiagnosed)
  expect_equal(
    pops$pregnant_hiv_testable, expected_testable, tolerance = 1,
    info = "pregnant_hiv_testable = all pregnant minus already-diagnosed HIV+ women"
  )
  
  # pregnant_hiv_testable < pregnant_women (diagnosed HIV+ removed)
  expect_lt(
    pops$pregnant_hiv_testable, pops$pregnant_women,
    label = "Already-diagnosed HIV+ women excluded from testing pool"
  )
  
  cat(sprintf("  pregnant_on_art (supp + unsupp):     %g == %g\n",
              pops$pregnant_on_art_suppressed + pops$pregnant_on_art_unsuppressed,
              pops$pregnant_on_art))
  cat(sprintf("  pregnant_hiv_pos_cascade (art+noart):%g == %g\n",
              pops$pregnant_on_art + pops$pregnant_not_on_art,
              pops$pregnant_hiv_pos_cascade))
  cat(sprintf("  pregnant_hiv_testable:               %g (vs pregnant_women %g)\n",
              pops$pregnant_hiv_testable, pops$pregnant_women))
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 2: PMTCT POPULATIONS SCALE WITH CONTEXT PARAMETERS
# ============================================================================

test_that("PMTCT populations scale correctly with prevalence, diagnosis, ART and suppression rates", {
  cat("\n========================================\n")
  cat("TEST 2: PMTCT Populations Scale With Context Parameters\n")
  cat("========================================\n")
  
  ctx_high_prev <- modifyList(make_context(), list(hiv_prevalence = 0.15))
  ctx_low_art   <- modifyList(make_context(), list(percent_on_art = 40))
  ctx_high_supp <- modifyList(make_context(), list(percent_suppressed = 95))
  
  pops_high_prev <- calculate_populations(ctx_high_prev)
  pops_low_art   <- calculate_populations(ctx_low_art)
  pops_high_supp <- calculate_populations(ctx_high_supp)
  
  # Higher prevalence → more HIV+ pregnant women, larger not_on_art group
  expect_gt(pops_high_prev$pregnant_hiv_pos_cascade, pops$pregnant_hiv_pos_cascade)
  expect_gt(pops_high_prev$pregnant_not_on_art,      pops$pregnant_not_on_art)
  
  # Lower ART coverage → more pregnant_not_on_art
  expect_gt(pops_low_art$pregnant_not_on_art, pops$pregnant_not_on_art)
  
  # Higher suppression → more pregnant_on_art_suppressed, fewer unsuppressed
  expect_gt(pops_high_supp$pregnant_on_art_suppressed,   pops$pregnant_on_art_suppressed)
  expect_lt(pops_high_supp$pregnant_on_art_unsuppressed, pops$pregnant_on_art_unsuppressed)
  
  cat(sprintf("  pregnant_not_on_art (base / low ART):  %g / %g\n",
              pops$pregnant_not_on_art, pops_low_art$pregnant_not_on_art))
  cat(sprintf("  pregnant_on_art_supp (base / high supp): %g / %g\n",
              pops$pregnant_on_art_suppressed, pops_high_supp$pregnant_on_art_suppressed))
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 3: ANC HIV TESTING — ELIGIBLE POOL AND PMTCT DIAGNOSIS
# ============================================================================

test_that("ANC HIV testing draws from pregnant_hiv_testable and diagnoses HIV+ undiagnosed pregnant women", {
  cat("\n========================================\n")
  cat("TEST 3: ANC HIV Testing — Eligible Pool and PMTCT Diagnosis\n")
  cat("========================================\n")
  
  ints_none <- zero_interventions()
  ints_anc  <- zero_interventions(); ints_anc$anc_hiv_testing <- 80
  
  out_none <- calculate_scenario_outcomes(ctx, ints_none, pops)
  out_anc  <- calculate_scenario_outcomes(ctx, ints_anc,  pops)
  
  cat(sprintf("  pmtct_newly_diagnosed (no ANC):  %g\n", out_none$pmtct_newly_diagnosed))
  cat(sprintf("  pmtct_newly_diagnosed (ANC 80%%): %g\n", out_anc$pmtct_newly_diagnosed))
  cat(sprintf("  pregnant_undiagnosed:            %g\n", pops$pregnant_undiagnosed))
  
  # ANC diagnoses HIV+ undiagnosed pregnant women
  expect_gt(out_anc$pmtct_newly_diagnosed, out_none$pmtct_newly_diagnosed)
  
  # Cannot diagnose more than the undiagnosed HIV+ pregnant pool
  expect_lte(out_anc$pmtct_newly_diagnosed, pops$pregnant_undiagnosed)
  
  # Higher ANC coverage → more PMTCT diagnoses
  ints_anc_high <- zero_interventions(); ints_anc_high$anc_hiv_testing <- 95
  out_anc_high  <- calculate_scenario_outcomes(ctx, ints_anc_high, pops)
  expect_gt(out_anc_high$pmtct_newly_diagnosed, out_anc$pmtct_newly_diagnosed)
  
  cat(sprintf("  pmtct_newly_diagnosed (ANC 95%%): %g\n", out_anc_high$pmtct_newly_diagnosed))
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 4: PNC TESTING — HIV-NEGATIVE WOMEN REMAIN FULLY ELIGIBLE AFTER ANC
# ============================================================================

test_that("PNC eligible pool deducts only HIV+ undiagnosed women caught at ANC; HIV-negative women remain", {
  cat("\n========================================\n")
  cat("TEST 4: PNC Eligible Pool After ANC\n")
  cat("========================================\n")
  
  # Run ANC alone, PNC alone, and combined
  ints_anc  <- zero_interventions(); ints_anc$anc_hiv_testing  <- 90
  ints_pnc  <- zero_interventions(); ints_pnc$pnc_hiv_testing  <- 90
  ints_both <- zero_interventions()
  ints_both$anc_hiv_testing <- 90; ints_both$pnc_hiv_testing <- 90
  
  out_anc  <- calculate_scenario_outcomes(ctx, ints_anc,  pops)
  out_pnc  <- calculate_scenario_outcomes(ctx, ints_pnc,  pops)
  out_both <- calculate_scenario_outcomes(ctx, ints_both, pops)
  
  cat(sprintf("  pmtct_newly_diagnosed (ANC only):    %g\n", out_anc$pmtct_newly_diagnosed))
  cat(sprintf("  pmtct_newly_diagnosed (PNC only):    %g\n", out_pnc$pmtct_newly_diagnosed))
  cat(sprintf("  pmtct_newly_diagnosed (ANC + PNC):   %g\n", out_both$pmtct_newly_diagnosed))
  cat(sprintf("  pregnant_undiagnosed:                %g\n", pops$pregnant_undiagnosed))
  
  # Combined diagnoses more than either alone
  expect_gt(out_both$pmtct_newly_diagnosed, out_anc$pmtct_newly_diagnosed)
  expect_gt(out_both$pmtct_newly_diagnosed, out_pnc$pmtct_newly_diagnosed)
  
  # Combined cannot exceed the total undiagnosed pool
  expect_lte(out_both$pmtct_newly_diagnosed, pops$pregnant_undiagnosed)
  
  # ANC + PNC combined < ANC_diagnosed + PNC_diagnosed
  # (because PNC pool is reduced by what ANC already caught)
  expect_lt(
    out_both$pmtct_newly_diagnosed,
    out_anc$pmtct_newly_diagnosed + out_pnc$pmtct_newly_diagnosed,
    label = "Combined diagnoses < sum of parts — PNC pool correctly deducted"
  )
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 5: ANC VL TESTING — SHIFTS UNSUPPRESSED PREGNANT WOMEN TO SUPPRESSED
# ============================================================================

test_that("ANC VL testing reduces mtct_pregnant_unsuppressed and increases mtct_pregnant_suppressed", {
  cat("\n========================================\n")
  cat("TEST 5: ANC VL Testing — Suppression Shift in MTCT Cascade\n")
  cat("========================================\n")
  
  ints_none   <- zero_interventions()
  ints_vl_low <- zero_interventions(); ints_vl_low$anc_vl_testing  <- 40
  ints_vl_high<- zero_interventions(); ints_vl_high$anc_vl_testing <- 90
  
  out_none    <- calculate_scenario_outcomes(ctx, ints_none,    pops)
  out_vl_low  <- calculate_scenario_outcomes(ctx, ints_vl_low,  pops)
  out_vl_high <- calculate_scenario_outcomes(ctx, ints_vl_high, pops)
  
  cat(sprintf("  mtct_pregnant_suppressed   (none / vl40 / vl90): %g / %g / %g\n",
              out_none$mtct_pregnant_suppressed,
              out_vl_low$mtct_pregnant_suppressed,
              out_vl_high$mtct_pregnant_suppressed))
  cat(sprintf("  mtct_pregnant_unsuppressed (none / vl40 / vl90): %g / %g / %g\n",
              out_none$mtct_pregnant_unsuppressed,
              out_vl_low$mtct_pregnant_unsuppressed,
              out_vl_high$mtct_pregnant_unsuppressed))
  
  # VL testing increases suppressed group, decreases unsuppressed group
  expect_gt(out_vl_low$mtct_pregnant_suppressed,    out_none$mtct_pregnant_suppressed)
  expect_lt(out_vl_low$mtct_pregnant_unsuppressed,  out_none$mtct_pregnant_unsuppressed)
  
  # Higher VL coverage → larger shift
  expect_gt(out_vl_high$mtct_pregnant_suppressed,   out_vl_low$mtct_pregnant_suppressed)
  expect_lt(out_vl_high$mtct_pregnant_unsuppressed, out_vl_low$mtct_pregnant_unsuppressed)
  
  # VL testing reduces end_infant_infections (more suppressed = lower MTCT rate)
  expect_lt(out_vl_low$end_infant_infections,  out_none$end_infant_infections)
  expect_lt(out_vl_high$end_infant_infections, out_vl_low$end_infant_infections)
  
  cat(sprintf("  end_infant_infections (none / vl40 / vl90): %g / %g / %g\n",
              out_none$end_infant_infections,
              out_vl_low$end_infant_infections,
              out_vl_high$end_infant_infections))
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 6: PMTCT LINKAGE AND SUPPRESSION RATES APPLIED TO NEWLY DIAGNOSED
# ============================================================================

test_that("Newly diagnosed pregnant women split by linkage and suppression rates into MTCT groups", {
  cat("\n========================================\n")
  cat("TEST 6: PMTCT Linkage and Suppression Rates\n")
  cat("========================================\n")
  
  # Set known linkage rate and suppression discount
  set_pmtct_params(anc_link = 0.80)
  
  ints_anc <- zero_interventions(); ints_anc$anc_hiv_testing <- 80
  out_anc  <- calculate_scenario_outcomes(ctx, ints_anc, pops)
  
  cat(sprintf("  pmtct_newly_diagnosed:  %g\n", out_anc$pmtct_newly_diagnosed))
  cat(sprintf("  pmtct_newly_linked:     %g\n", out_anc$pmtct_newly_linked))
  cat(sprintf("  pmtct_newly_suppressed: %g\n", out_anc$pmtct_newly_suppressed))
  
  # Linked must be <= diagnosed (linkage rate < 1)
  expect_lte(out_anc$pmtct_newly_linked, out_anc$pmtct_newly_diagnosed)
  
  # Suppressed must be <= linked
  expect_lte(out_anc$pmtct_newly_suppressed, out_anc$pmtct_newly_linked)
  
  # All three should be > 0 when ANC coverage > 0 and there are undiagnosed women
  expect_gt(out_anc$pmtct_newly_diagnosed,  0)
  expect_gt(out_anc$pmtct_newly_linked,     0)
  expect_gt(out_anc$pmtct_newly_suppressed, 0)
  
  # Higher linkage rate → more linked
  set_pmtct_params(anc_link = 0.95)
  out_high_link <- calculate_scenario_outcomes(ctx, ints_anc, pops)
  expect_gt(out_high_link$pmtct_newly_linked, out_anc$pmtct_newly_linked)
  
  set_pmtct_params()  # restore defaults
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 7: MTCT CASCADE — STRATIFIED RATES DRIVE INFANT INFECTIONS
# ============================================================================

test_that("Baseline infant infections computed from stratified MTCT rates match manual calculation", {
  cat("\n========================================\n")
  cat("TEST 7: MTCT Cascade — Stratified Rates Drive Infant Infections\n")
  cat("========================================\n")
  
  ints_none <- zero_interventions()
  out_none  <- calculate_scenario_outcomes(ctx, ints_none, pops)
  
  # With zero interventions, MTCT groups equal baseline populations
  # Manual: end_infant_infections =
  #   pregnant_on_art_suppressed   * 0.02 +
  #   pregnant_on_art_unsuppressed * 0.15 +
  #   pregnant_not_on_art          * 0.35
  expected_infections <- pops$pregnant_on_art_suppressed   * MTCT_RATES$on_art_suppressed +
    pops$pregnant_on_art_unsuppressed * MTCT_RATES$on_art_unsuppressed +
    pops$pregnant_not_on_art          * MTCT_RATES$not_on_art
  
  cat(sprintf("  Manual MTCT calculation: %g\n", expected_infections))
  cat(sprintf("  end_infant_infections:   %g\n", out_none$end_infant_infections))
  
  expect_equal(out_none$end_infant_infections, round(expected_infections), tolerance = 1,
               info = "Zero interventions: infant infections must match manual stratified MTCT calculation")
  
  # MTCT cascade groups sum to pregnant_hiv_pos_cascade (at zero interventions)
  cascade_sum <- out_none$mtct_pregnant_suppressed +
    out_none$mtct_pregnant_unsuppressed +
    out_none$mtct_pregnant_no_art
  expect_equal(cascade_sum, round(pops$pregnant_hiv_pos_cascade), tolerance = 1,
               info = "MTCT cascade groups must sum to total HIV+ pregnant women")
  
  cat(sprintf("  cascade sum: %g == pregnant_hiv_pos_cascade: %g\n",
              cascade_sum, pops$pregnant_hiv_pos_cascade))
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 8: INFANT PROPHYLAXIS — USES CSV EFFICACY, REDUCES RESIDUAL TRANSMISSION
# ============================================================================

test_that("Infant prophylaxis reduces end_infant_infections using CSV efficacy; higher coverage = fewer infections", {
  cat("\n========================================\n")
  cat("TEST 8: Infant Prophylaxis — CSV Efficacy and Coverage Effect\n")
  cat("========================================\n")
  
  set_pmtct_params(inf_prophy_eff = 0.54)
  
  ints_none  <- zero_interventions()
  ints_low   <- zero_interventions(); ints_low$infant_prophylaxis  <- 40
  ints_high  <- zero_interventions(); ints_high$infant_prophylaxis <- 90
  
  out_none  <- calculate_scenario_outcomes(ctx, ints_none,  pops)
  out_low   <- calculate_scenario_outcomes(ctx, ints_low,   pops)
  out_high  <- calculate_scenario_outcomes(ctx, ints_high,  pops)
  
  cat(sprintf("  end_infant_infections (none / 40%% / 90%%): %g / %g / %g\n",
              out_none$end_infant_infections,
              out_low$end_infant_infections,
              out_high$end_infant_infections))
  cat(sprintf("  infant_infections_averted (40%% / 90%%):    %g / %g\n",
              out_low$infant_infections_averted,
              out_high$infant_infections_averted))
  
  # Prophylaxis reduces infections
  expect_lt(out_low$end_infant_infections,  out_none$end_infant_infections)
  expect_lt(out_high$end_infant_infections, out_low$end_infant_infections)
  
  # More coverage → more infections averted
  expect_gt(out_high$infant_infections_averted, out_low$infant_infections_averted)
  
  # end_infant_infections must remain non-negative
  expect_gte(out_high$end_infant_infections, 0)
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 9: EID — COST APPLIES TO ALL HIV-EXPOSED; EFFECT ONLY TO INFECTED
# ============================================================================

test_that("EID cost applies to all HIV-exposed infants tested; diagnosis count reflects only infected infants", {
  cat("\n========================================\n")
  cat("TEST 9: EID — Cost on All Exposed, Effect on Infected Only\n")
  cat("========================================\n")
  
  set_pmtct_params(eid_eff = 0.95, eid_cost = 20, eid_link_cost = 200, eid_link = 0.80)
  
  ints_eid <- zero_interventions(); ints_eid$eid <- 80
  
  out_none <- calculate_scenario_outcomes(ctx, zero_interventions(), pops)
  out_eid  <- calculate_scenario_outcomes(ctx, ints_eid, pops)
  
  # Expected: 80% of hiv_exposed_infants reach by EID
  expected_reached <- pops$hiv_exposed_infants * 0.80
  
  cat(sprintf("  hiv_exposed_infants:      %g\n",   pops$hiv_exposed_infants))
  cat(sprintf("  expected_reached (80%%):   %g\n",   expected_reached))
  cat(sprintf("  eid_infants_diagnosed:    %g\n",   out_eid$eid_infants_diagnosed))
  cat(sprintf("  end_infant_infections:    %g\n",   out_eid$end_infant_infections))
  
  # eid_infants_diagnosed <= end_infant_infections (can only find actually infected infants)
  expect_lte(out_eid$eid_infants_diagnosed, out_eid$end_infant_infections)
  
  # EID diagnoses > 0 when coverage > 0 and there are infected infants
  expect_gt(out_eid$eid_infants_diagnosed, 0)
  
  # Zero EID → zero diagnosed
  expect_equal(out_none$eid_infants_diagnosed, 0)
  
  # EID cost increased (testing all exposed infants)
  expect_gt(out_eid$total_intervention_cost, out_none$total_intervention_cost)
  
  # Higher EID coverage → more diagnosed
  ints_eid_high <- zero_interventions(); ints_eid_high$eid <- 95
  out_eid_high  <- calculate_scenario_outcomes(ctx, ints_eid_high, pops)
  expect_gt(out_eid_high$eid_infants_diagnosed, out_eid$eid_infants_diagnosed)
  
  cat(sprintf("  eid_infants_diagnosed (80%% / 95%%): %g / %g\n",
              out_eid$eid_infants_diagnosed, out_eid_high$eid_infants_diagnosed))
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 10: EID YIELD USES ACTUAL POST-CASCADE INFECTION RATE
# ============================================================================

test_that("EID yield reflects actual post-cascade infection rate, not a flat rate", {
  cat("\n========================================\n")
  cat("TEST 10: EID Yield Uses Actual Post-Cascade Infection Rate\n")
  cat("========================================\n")
  
  # With ANC VL testing reducing infections, EID should diagnose fewer
  # (same coverage but lower yield because fewer infants are actually infected)
  ints_eid_only <- zero_interventions(); ints_eid_only$eid <- 80
  ints_eid_vl   <- zero_interventions()
  ints_eid_vl$eid <- 80; ints_eid_vl$anc_vl_testing <- 90
  
  out_eid_only <- calculate_scenario_outcomes(ctx, ints_eid_only, pops)
  out_eid_vl   <- calculate_scenario_outcomes(ctx, ints_eid_vl,   pops)
  
  cat(sprintf("  end_infant_infections (EID only / EID+VL):  %g / %g\n",
              out_eid_only$end_infant_infections,
              out_eid_vl$end_infant_infections))
  cat(sprintf("  eid_infants_diagnosed (EID only / EID+VL):  %g / %g\n",
              out_eid_only$eid_infants_diagnosed,
              out_eid_vl$eid_infants_diagnosed))
  
  # ANC VL reduces infections → EID yield (infected / exposed) falls → fewer diagnosed
  expect_lt(out_eid_vl$end_infant_infections, out_eid_only$end_infant_infections)
  expect_lt(out_eid_vl$eid_infants_diagnosed, out_eid_only$eid_infants_diagnosed)
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 11: INFANT MORTALITY CASCADE — LINKAGE AND SUPPRESSION FROM EID
# ============================================================================

test_that("Infant mortality cascade: EID links infants to ART; higher coverage reduces infant deaths", {
  cat("\n========================================\n")
  cat("TEST 11: Infant Mortality Cascade — EID Linkage and Suppression\n")
  cat("========================================\n")
  
  restore_infant_mortality()
  set_pmtct_params(eid_eff = 0.95, eid_link = 0.80)
  
  ints_none     <- zero_interventions()
  ints_eid_low  <- zero_interventions(); ints_eid_low$eid  <- 40
  ints_eid_high <- zero_interventions(); ints_eid_high$eid <- 90
  
  out_none     <- calculate_scenario_outcomes(ctx, ints_none,     pops)
  out_eid_low  <- calculate_scenario_outcomes(ctx, ints_eid_low,  pops)
  out_eid_high <- calculate_scenario_outcomes(ctx, ints_eid_high, pops)
  
  cat(sprintf("  infant_on_art     (none / eid40 / eid90): %g / %g / %g\n",
              out_none$infant_on_art,
              out_eid_low$infant_on_art,
              out_eid_high$infant_on_art))
  cat(sprintf("  total_infant_deaths (none / eid40 / eid90): %g / %g / %g\n",
              out_none$total_infant_deaths,
              out_eid_low$total_infant_deaths,
              out_eid_high$total_infant_deaths))
  cat(sprintf("  infant_deaths_averted (eid40 / eid90): %g / %g\n",
              out_eid_low$infant_deaths_averted,
              out_eid_high$infant_deaths_averted))
  
  # Zero EID → zero infant_on_art
  expect_equal(out_none$infant_on_art, 0)
  
  # EID links infants to ART
  expect_gt(out_eid_low$infant_on_art,  0)
  expect_gt(out_eid_high$infant_on_art, out_eid_low$infant_on_art)
  
  # Treatment reduces infant deaths
  expect_lt(out_eid_low$total_infant_deaths,  out_none$total_infant_deaths)
  expect_lt(out_eid_high$total_infant_deaths, out_eid_low$total_infant_deaths)
  
  # Deaths averted > 0 when EID > 0
  expect_gt(out_eid_low$infant_deaths_averted,  0)
  expect_gt(out_eid_high$infant_deaths_averted, out_eid_low$infant_deaths_averted)
  
  zero_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 12: INFANT DEATHS WIRED INTO TOTAL DEATHS_AVERTED AND END_DEATHS
# ============================================================================

test_that("Infant deaths averted contribute to deaths_averted; infant deaths contribute to end_deaths", {
  cat("\n========================================\n")
  cat("TEST 12: Infant Mortality Wired Into Aggregate Death Outcomes\n")
  cat("========================================\n")
  
  restore_infant_mortality()
  set_pmtct_params(eid_eff = 0.95, eid_link = 0.80)
  
  ints_none <- zero_interventions()
  ints_eid  <- zero_interventions(); ints_eid$eid <- 80
  
  out_none <- calculate_scenario_outcomes(ctx, ints_none, pops)
  out_eid  <- calculate_scenario_outcomes(ctx, ints_eid,  pops)
  
  cat(sprintf("  end_deaths (none / eid80):     %g / %g\n",
              out_none$end_deaths, out_eid$end_deaths))
  cat(sprintf("  deaths_averted (none / eid80): %g / %g\n",
              out_none$deaths_averted, out_eid$deaths_averted))
  cat(sprintf("  total_infant_deaths (none / eid80): %g / %g\n",
              out_none$total_infant_deaths, out_eid$total_infant_deaths))
  
  # With infant mortality enabled, deaths are positive even at zero EID
  expect_gt(out_none$end_deaths, 0)
  
  # EID reduces total end_deaths (infants on ART die at lower rates)
  expect_lt(out_eid$end_deaths, out_none$end_deaths)
  
  # EID increases deaths_averted
  expect_gt(out_eid$deaths_averted, out_none$deaths_averted)
  
  # Difference in end_deaths matches total_infant_deaths difference
  deaths_diff <- out_none$end_deaths - out_eid$end_deaths
  infant_diff <- out_none$total_infant_deaths - out_eid$total_infant_deaths
  expect_equal(deaths_diff, infant_diff, tolerance = 1,
               info = "Reduction in end_deaths must equal reduction in total_infant_deaths")
  
  zero_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 13: INFANT SUPPRESSED <= INFANT ON ART
# ============================================================================

test_that("infant_suppressed is a subset of infant_on_art (cannot suppress more than are on ART)", {
  cat("\n========================================\n")
  cat("TEST 13: Infant Cascade Consistency — Suppressed Subset of On ART\n")
  cat("========================================\n")
  
  restore_infant_mortality()
  set_pmtct_params(eid_link = 0.80)
  
  ints_eid <- zero_interventions(); ints_eid$eid <- 80
  out_eid  <- calculate_scenario_outcomes(ctx, ints_eid, pops)
  
  cat(sprintf("  infant_on_art:    %g\n", out_eid$infant_on_art))
  cat(sprintf("  infant_suppressed:%g\n", out_eid$infant_suppressed))
  
  expect_lte(out_eid$infant_suppressed, out_eid$infant_on_art)
  expect_gte(out_eid$infant_suppressed, 0)
  expect_gte(out_eid$infant_on_art,     0)
  
  zero_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 14: FULL PMTCT COMBINATION REDUCES INFANT INFECTIONS AND DEATHS
# ============================================================================

test_that("All PMTCT interventions combined produce fewer infant infections and deaths than any alone", {
  cat("\n========================================\n")
  cat("TEST 14: Full PMTCT Combination — Monotone Reduction\n")
  cat("========================================\n")
  
  restore_infant_mortality()
  set_pmtct_params()
  
  ints_none  <- zero_interventions()
  ints_anc   <- zero_interventions(); ints_anc$anc_hiv_testing   <- 90
  ints_vl    <- zero_interventions(); ints_vl$anc_vl_testing     <- 80
  ints_prophy<- zero_interventions(); ints_prophy$infant_prophylaxis <- 80
  ints_eid   <- zero_interventions(); ints_eid$eid               <- 80
  
  ints_all   <- zero_interventions()
  ints_all$anc_hiv_testing   <- 90
  ints_all$pnc_hiv_testing   <- 70
  ints_all$anc_vl_testing    <- 80
  ints_all$infant_prophylaxis <- 80
  ints_all$eid               <- 80
  
  out_none   <- calculate_scenario_outcomes(ctx, ints_none,   pops)
  out_anc    <- calculate_scenario_outcomes(ctx, ints_anc,    pops)
  out_vl     <- calculate_scenario_outcomes(ctx, ints_vl,     pops)
  out_prophy <- calculate_scenario_outcomes(ctx, ints_prophy, pops)
  out_eid    <- calculate_scenario_outcomes(ctx, ints_eid,    pops)
  out_all    <- calculate_scenario_outcomes(ctx, ints_all,    pops)
  
  cat(sprintf("  end_infant_infections (none):    %g\n", out_none$end_infant_infections))
  cat(sprintf("  end_infant_infections (ANC):     %g\n", out_anc$end_infant_infections))
  cat(sprintf("  end_infant_infections (VL):      %g\n", out_vl$end_infant_infections))
  cat(sprintf("  end_infant_infections (prophy):  %g\n", out_prophy$end_infant_infections))
  cat(sprintf("  end_infant_infections (all):     %g\n", out_all$end_infant_infections))
  cat(sprintf("  total_infant_deaths   (none):    %g\n", out_none$total_infant_deaths))
  cat(sprintf("  total_infant_deaths   (all):     %g\n", out_all$total_infant_deaths))
  
  # Each intervention alone reduces infections vs none
  expect_lt(out_anc$end_infant_infections,    out_none$end_infant_infections)
  expect_lt(out_vl$end_infant_infections,     out_none$end_infant_infections)
  expect_lt(out_prophy$end_infant_infections, out_none$end_infant_infections)
  
  # All combined beats any single intervention
  expect_lt(out_all$end_infant_infections, out_anc$end_infant_infections)
  expect_lt(out_all$end_infant_infections, out_vl$end_infant_infections)
  expect_lt(out_all$end_infant_infections, out_prophy$end_infant_infections)
  
  # All combined reduces infant deaths vs no interventions
  expect_lt(out_all$total_infant_deaths, out_none$total_infant_deaths)
  
  # end_infant_infections always non-negative
  expect_gte(out_all$end_infant_infections, 0)
  
  zero_mortality()
  cat("✓ All assertions passed\n")
})

# ============================================================================
# TEST 15: ZERO COVERAGE — MTCT CASCADE REPRODUCES RAW POPULATION RATES
# ============================================================================

test_that("Zero interventions: MTCT cascade groups equal baseline populations exactly", {
  cat("\n========================================\n")
  cat("TEST 15: Zero Coverage — Raw Population Rates Preserved\n")
  cat("========================================\n")
  
  ints_none <- zero_interventions()
  out_none  <- calculate_scenario_outcomes(ctx, ints_none, pops)
  
  cat(sprintf("  mtct_pregnant_suppressed   vs pops: %g == %g\n",
              out_none$mtct_pregnant_suppressed, pops$pregnant_on_art_suppressed))
  cat(sprintf("  mtct_pregnant_unsuppressed vs pops: %g == %g\n",
              out_none$mtct_pregnant_unsuppressed, pops$pregnant_on_art_unsuppressed))
  cat(sprintf("  mtct_pregnant_no_art       vs pops: %g == %g\n",
              out_none$mtct_pregnant_no_art, pops$pregnant_not_on_art))
  
  expect_equal(out_none$mtct_pregnant_suppressed,   round(pops$pregnant_on_art_suppressed),   tolerance = 1)
  expect_equal(out_none$mtct_pregnant_unsuppressed, round(pops$pregnant_on_art_unsuppressed), tolerance = 1)
  expect_equal(out_none$mtct_pregnant_no_art,       round(pops$pregnant_not_on_art),          tolerance = 1)
  
  cat("✓ All assertions passed\n")
})

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n========================================\n")
cat("ALL PMTCT & INFANT CASCADE TESTS COMPLETED\n")
cat("========================================\n")
cat("✓ TEST 1:  PMTCT sub-populations internally consistent\n")
cat("✓ TEST 2:  PMTCT populations scale with context parameters\n")
cat("✓ TEST 3:  ANC HIV testing — eligible pool and PMTCT diagnosis pathway\n")
cat("✓ TEST 4:  PNC testing deducts only HIV+ undiagnosed caught at ANC\n")
cat("✓ TEST 5:  ANC VL testing shifts unsuppressed -> suppressed in MTCT cascade\n")
cat("✓ TEST 6:  PMTCT linkage and suppression rates applied to newly diagnosed\n")
cat("✓ TEST 7:  MTCT cascade stratified rates drive infant infections\n")
cat("✓ TEST 8:  Infant prophylaxis uses CSV efficacy, reduces residual transmission\n")
cat("✓ TEST 9:  EID cost on all exposed; effect only on infected infants\n")
cat("✓ TEST 10: EID yield uses actual post-cascade infection rate\n")
cat("✓ TEST 11: Infant mortality cascade — EID linkage and suppression\n")
cat("✓ TEST 12: Infant mortality wired into aggregate deaths_averted and end_deaths\n")
cat("✓ TEST 13: infant_suppressed is a subset of infant_on_art\n")
cat("✓ TEST 14: Full PMTCT combination reduces infant infections and deaths\n")
cat("✓ TEST 15: Zero coverage — MTCT cascade reproduces raw population rates\n")