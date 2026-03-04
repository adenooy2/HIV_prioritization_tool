# ============================================================================
# STRATIFIED FORCE-OF-INFECTION (FOI) MODULE
# HIV Intervention Impact Calculator — TIER-plus
# ============================================================================
#
# CONCEPTUAL MODEL:
#   New infections arise from three distinct risk strata, each with its own
#   transmission rate (β) calibrated to the country baseline. Interventions
#   act by either:
#     (a) reducing the INFECTIOUS PRESSURE (viral suppression → fewer
#         unsuppressed PLHIV in circulation), or
#     (b) reducing the SUSCEPTIBLE POOL or per-contact risk within a stratum
#         (PrEP, condoms, VMMC → prevention coverage)
#
# STRATA:
#   1. High-risk (KP, high-concurrency partners)       ~5% of HIV-negative adults
#   2. General sexual risk (sexually active adults)    ~55% of HIV-negative adults
#   3. Female/pregnant on ART (PMTCT pathway)          handled separately via MTCT
#
# VMMC NOTE:
#   VMMC only acts on uncircumcised men in the general stratum.
#   Eligible pool = general_male × (1 - baseline_circumcision_prevalence)
#   β for circumcised men is already lower, so VMMC shifts men from
#   high-susceptibility to low-susceptibility, not to zero.
#
# CALIBRATION:
#   β for each stratum is back-calculated so that the model exactly reproduces
#   observed new_infections_per_year at baseline. This means the model is
#   always internally consistent with country-reported data, regardless of
#   assumed stratum sizes or risk ratios.
#
# ============================================================================


# ----------------------------------------------------------------------------
# STEP 1: DEFINE STRATUM PARAMETERS
# ----------------------------------------------------------------------------
# These are the structural assumptions about how the population is divided
# and how risk is distributed. They can be overridden via country CSV.
#
# UPDATE TARGETS (mark with # CALIBRATE):
#   - prop_high_risk: proportion of HIV-negative sexually active adults who
#     are "high risk" (KP + partners). Literature range ~3-8% in SSA.
#   - rr_high: relative transmission rate for high-risk vs general.
#     Conceptually captures higher partner numbers, lower condom use, etc.
#     Literature range ~5-15x for FSW/MSM vs general population.
#   - prop_male_general: male share of general stratum (roughly 0.5)
#   - vmmc_efficacy_partial: residual protection after circumcision
#     (VMMC reduces, not eliminates, male acquisition risk)

define_strata_params <- function(context = NULL) {
  
  # Allow country-level overrides from context if present
  prop_high_risk      <- if (!is.null(context$prop_high_risk))   context$prop_high_risk   else 0.05   # CALIBRATE
  rr_high             <- if (!is.null(context$rr_high))          context$rr_high          else 8.0    # CALIBRATE: high-risk RR vs general
  prop_male_general   <- if (!is.null(context$prop_male_general)) context$prop_male_general else 0.50  # CALIBRATE
  circ_prevalence     <- if (!is.null(context$circ_prevalence))  context$circ_prevalence  else 0.20   # CALIBRATE: baseline male circumcision prevalence
  vmmc_risk_reduction <- if (!is.null(context$vmmc_risk_reduction)) context$vmmc_risk_reduction else 0.60  # CALIBRATE: ~60% risk reduction from RCTs
  
  list(
    prop_high_risk      = prop_high_risk,
    prop_general        = 1 - prop_high_risk,
    rr_high             = rr_high,
    prop_male_general   = prop_male_general,
    circ_prevalence     = circ_prevalence,
    vmmc_risk_reduction = vmmc_risk_reduction
  )
}


# ----------------------------------------------------------------------------
# STEP 2: PARTITION POPULATIONS INTO STRATA
# ----------------------------------------------------------------------------
# Takes the flat populations object (already calculated) and splits
# HIV-negative sexually active adults into risk strata.
#
# Returns a named list of stratum-specific counts used in FOI calculation.

partition_into_strata <- function(populations, strata_params) {
  
  hiv_neg_active <- populations$sexually_active_negative  # total HIV-neg sexually active
  
  # --- High-risk stratum ---
  # KP and high-concurrency partners
  n_high_risk <- hiv_neg_active * strata_params$prop_high_risk
  
  # --- General stratum ---
  n_general   <- hiv_neg_active * strata_params$prop_general
  
  # VMMC-relevant sub-partition of general stratum:
  # Only uncircumcised men are eligible for VMMC benefit
  n_general_male        <- n_general * strata_params$prop_male_general
  n_general_male_uncirc <- n_general_male * (1 - strata_params$circ_prevalence)
  n_general_male_circ   <- n_general_male * strata_params$circ_prevalence
  n_general_female      <- n_general * (1 - strata_params$prop_male_general)
  
  # --- Infectious pressure (shared across strata) ---
  # Unsuppressed PLHIV = those on ART but not suppressed + those not on ART
  # (diagnosed not on ART + undiagnosed are all assumed unsuppressed)
  n_unsuppressed <- populations$unsuppressed +           # on ART, not suppressed
                    populations$diagnosed_not_on_art +   # diagnosed but not initiated
                    populations$undiagnosed              # never diagnosed
  
  list(
    n_high_risk           = n_high_risk,
    n_general             = n_general,
    n_general_male_uncirc = n_general_male_uncirc,
    n_general_male_circ   = n_general_male_circ,
    n_general_female      = n_general_female,
    n_unsuppressed        = n_unsuppressed,
    hiv_neg_active        = hiv_neg_active
  )
}


# ----------------------------------------------------------------------------
# STEP 3: CALIBRATE β PER STRATUM FROM BASELINE
# ----------------------------------------------------------------------------
# Back-calculates stratum-specific transmission rates so the model
# exactly reproduces the observed new_infections_per_year at baseline.
#
# FOI model per stratum s:
#   infections_s = β_s × (n_unsuppressed / total_pop) × n_susceptible_s
#
# We split observed infections proportionally across strata using the
# assumed relative risks (rr_high vs 1 for general), then solve for β_s.
#
# This means:
#   - β is always country-calibrated (no assumption about "true" β needed)
#   - The RR between strata is set by rr_high, but absolute scale is data-driven
#   - If rr_high = 1, reverts to homogeneous model

calibrate_beta <- function(context, populations, strata, strata_params) {
  
  observed_infections <- context$new_infections_per_year
  
  # Infectious pressure term (same for all strata — shared pool assumption)
  infectious_pressure <- strata$n_unsuppressed / populations$total
  
  # Expected infections per stratum BEFORE calibration (proportional weights)
  # General stratum is split into female + circ male + uncirc male
  # Uncirc males have higher susceptibility (vmmc_risk_reduction captures the delta)
  
  # Susceptibility weights relative to general female = 1.0
  # Uncircumcised male: same as female (1.0) — VMMC will reduce this
  # Circumcised male: already reduced by vmmc_risk_reduction
  w_high          <- strata_params$rr_high
  w_gen_female    <- 1.0
  w_gen_male_uncirc <- 1.0                                       # pre-VMMC, same as female
  w_gen_male_circ   <- 1.0 - strata_params$vmmc_risk_reduction  # already circumcised
  
  # Weighted susceptible counts → proportional infection allocation
  weighted_high          <- w_high           * strata$n_high_risk
  weighted_gen_female    <- w_gen_female     * strata$n_general_female
  weighted_gen_male_unc  <- w_gen_male_uncirc * strata$n_general_male_uncirc
  weighted_gen_male_circ <- w_gen_male_circ  * strata$n_general_male_circ
  
  total_weight <- weighted_high + weighted_gen_female + weighted_gen_male_unc + weighted_gen_male_circ
  
  # Fraction of infections attributable to each stratum at baseline
  frac_high          <- weighted_high          / total_weight
  frac_gen_female    <- weighted_gen_female    / total_weight
  frac_gen_male_unc  <- weighted_gen_male_unc  / total_weight
  frac_gen_male_circ <- weighted_gen_male_circ / total_weight
  
  # Baseline infections per stratum
  inf_high          <- observed_infections * frac_high
  inf_gen_female    <- observed_infections * frac_gen_female
  inf_gen_male_unc  <- observed_infections * frac_gen_male_unc
  inf_gen_male_circ <- observed_infections * frac_gen_male_circ
  
  # Back-calculate β for each stratum:
  #   infections_s = β_s × infectious_pressure × n_susceptible_s
  #   β_s = infections_s / (infectious_pressure × n_susceptible_s)
  
  safe_beta <- function(inf, pressure, n) {
    if (is.null(n) || n == 0 || pressure == 0) return(0)
    inf / (pressure * n)
  }
  
  list(
    beta_high          = safe_beta(inf_high,          infectious_pressure, strata$n_high_risk),
    beta_gen_female    = safe_beta(inf_gen_female,    infectious_pressure, strata$n_general_female),
    beta_gen_male_unc  = safe_beta(inf_gen_male_unc,  infectious_pressure, strata$n_general_male_uncirc),
    beta_gen_male_circ = safe_beta(inf_gen_male_circ, infectious_pressure, strata$n_general_male_circ),
    
    # Store these for diagnostics / sanity checks
    frac_high          = frac_high,
    frac_gen           = 1 - frac_high,
    baseline_infections_check = observed_infections  # useful for debugging
  )
}


# ----------------------------------------------------------------------------
# STEP 4: COMPUTE PREVENTION COVERAGE ADJUSTMENTS
# ----------------------------------------------------------------------------
# Each prevention intervention reduces the effective susceptible pool in
# one or more strata. Returns a named list of coverage-weighted risk
# reductions (0 = no protection, 1 = full protection) per stratum.
#
# NOTE on VMMC:
#   VMMC converts uncircumcised men → effectively circumcised (lower β).
#   We model this as: n_uncirc_effective = n_uncirc × (1 - newly_circumcised_frac)
#   rather than as a coverage multiplier, to keep the β calibration clean.
#
# NOTE on stacking:
#   When multiple interventions cover the same stratum, we use multiplicative
#   stacking: residual_risk = Π(1 - coverage_i × efficacy_i)
#   This avoids > 100% protection and is the standard approach.

compute_prevention_adjustments <- function(scenario_interventions, strata, populations, strata_params) {
  
  # Helper: clip coverage to [0, 1]
  clip <- function(x) max(0, min(1, x))
  
  # ---- High-risk stratum ----
  # Interventions: PrEP (oral + lenacapavir), condoms, KP/STI testing (linkage effect)
  # Note: PrEP coverage input is in absolute people; convert to proportion of stratum
  
  prep_oral_coverage_high     <- clip(
    (scenario_interventions$prep_oral %||% 0) / max(strata$n_high_risk, 1)
  )
  prep_len_coverage_high      <- clip(
    (scenario_interventions$prep_lenacapavir %||% 0) / max(strata$n_high_risk, 1)
  )
  # Condoms: split coverage across strata proportionally to stratum size
  condom_coverage_high        <- clip(
    (scenario_interventions$condoms %||% 0) * strata_params$prop_high_risk / max(strata$n_high_risk, 1)
  )
  
  # Efficacies (already loaded from intervention_params in main app)
  # Passed in here for modularity — see INTEGRATION NOTE below
  eff_prep_oral  <- 0.99   # UPDATE: pull from intervention_params$efficacy
  eff_prep_len   <- 1.00   # UPDATE: PURPOSE trials (near-perfect)
  eff_condom     <- 0.80   # UPDATE: consistent use efficacy
  
  # Multiplicative stacking for high-risk
  residual_high <- (1 - prep_oral_coverage_high * eff_prep_oral) *
                   (1 - prep_len_coverage_high  * eff_prep_len)  *
                   (1 - condom_coverage_high     * eff_condom)
  
  protection_high <- 1 - residual_high
  
  
  # ---- General stratum: female ----
  # Interventions: condoms, PEP (marginal), ANC-linked
  condom_coverage_gen_f <- clip(
    (scenario_interventions$condoms %||% 0) * strata_params$prop_general *
      (1 - strata_params$prop_male_general) / max(strata$n_general_female, 1)
  )
  pep_coverage_gen_f    <- clip(
    (scenario_interventions$pep %||% 0) * 0.5 / max(strata$n_general_female, 1)  # 50% female
  )
  
  eff_pep <- 0.80  # UPDATE: pull from intervention_params
  
  residual_gen_female <- (1 - condom_coverage_gen_f * eff_condom) *
                         (1 - pep_coverage_gen_f    * eff_pep)
  protection_gen_female <- 1 - residual_gen_female
  
  
  # ---- General stratum: uncircumcised males ----
  # Interventions: condoms, PEP, VMMC (special — see below)
  condom_coverage_gen_mu <- clip(
    (scenario_interventions$condoms %||% 0) * strata_params$prop_general *
      strata_params$prop_male_general * (1 - strata_params$circ_prevalence) /
      max(strata$n_general_male_uncirc, 1)
  )
  pep_coverage_gen_mu <- clip(
    (scenario_interventions$pep %||% 0) * 0.5 / max(strata$n_general_male_uncirc, 1)
  )
  
  residual_gen_male_unc <- (1 - condom_coverage_gen_mu * eff_condom) *
                           (1 - pep_coverage_gen_mu    * eff_pep)
  protection_gen_male_unc <- 1 - residual_gen_male_unc
  
  
  # ---- VMMC: special handling ----
  # VMMC does not reduce risk for already-covered men — it converts
  # uncircumcised men to the lower-risk circumcised category.
  # We track what fraction of uncircumcised men have been newly circumcised
  # in the scenario, and use it to reweight the uncirc/circ split.
  #
  # newly_circumcised = vmmc_scenario (absolute people in scenario)
  # but capped at the eligible (uncircumcised) pool
  
  newly_circumcised   <- min(scenario_interventions$vmmc %||% 0, strata$n_general_male_uncirc)
  vmmc_coverage_frac  <- clip(newly_circumcised / max(strata$n_general_male_uncirc, 1))
  
  # After VMMC: effective uncirc pool shrinks, circ pool grows
  # This is returned separately so FOI calculation can reweight these sub-strata
  
  list(
    protection_high          = protection_high,
    protection_gen_female    = protection_gen_female,
    protection_gen_male_unc  = protection_gen_male_unc,
    vmmc_coverage_frac       = vmmc_coverage_frac   # fraction of uncirc men now circumcised
  )
}

# Null-coalescing helper (in case not already in your environment)
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b


# ----------------------------------------------------------------------------
# STEP 5: MAIN FOI FUNCTION — ESTIMATE NEW INFECTIONS UNDER A SCENARIO
# ----------------------------------------------------------------------------
# This is the function you call from calculate_impact().
#
# Inputs:
#   context               — reactive context list (prevalence, deaths, etc.)
#   populations           — output of calculate_populations()
#   scenario_interventions — named list of scenario values (same structure as
#                            baseline/target in calculate_impact)
#   suppression_delta     — change in number of virally suppressed people
#                           (output from the treatment arm of calculate_impact)
#   strata_params         — optional override; defaults via define_strata_params()
#
# Returns:
#   List with new_infections (total) and stratum breakdown for diagnostics

estimate_new_infections_foi <- function(context,
                                        populations,
                                        scenario_interventions,
                                        suppression_delta = 0,
                                        strata_params = NULL) {
  
  # Step 1: Get stratum parameters
  if (is.null(strata_params)) {
    strata_params <- define_strata_params(context)
  }
  
  # Step 2: Partition populations
  strata <- partition_into_strata(populations, strata_params)
  
  # Step 3: Calibrate β from observed baseline
  betas <- calibrate_beta(context, populations, strata, strata_params)
  
  # Step 4: Adjust unsuppressed pool for treatment-side changes
  # suppression_delta > 0 means more people suppressed → fewer infectious
  n_unsuppressed_scenario <- max(0, strata$n_unsuppressed - suppression_delta)
  infectious_pressure_scenario <- n_unsuppressed_scenario / populations$total
  
  # Step 5: Compute prevention adjustments
  prev_adj <- compute_prevention_adjustments(
    scenario_interventions, strata, populations, strata_params
  )
  
  # Step 6: Adjust uncirc/circ split based on VMMC coverage in scenario
  # Newly circumcised men move from uncirc → circ susceptibility
  n_newly_circ      <- prev_adj$vmmc_coverage_frac * strata$n_general_male_uncirc
  n_uncirc_eff      <- strata$n_general_male_uncirc - n_newly_circ  # shrinks
  n_circ_eff        <- strata$n_general_male_circ   + n_newly_circ  # grows
  
  # Step 7: Compute infections per stratum
  #   infections_s = β_s × infectious_pressure × susceptible_s × (1 - protection_s)
  
  infections_high <- betas$beta_high *
    infectious_pressure_scenario *
    strata$n_high_risk *
    (1 - prev_adj$protection_high)
  
  infections_gen_female <- betas$beta_gen_female *
    infectious_pressure_scenario *
    strata$n_general_female *
    (1 - prev_adj$protection_gen_female)
  
  infections_gen_male_unc <- betas$beta_gen_male_unc *
    infectious_pressure_scenario *
    n_uncirc_eff *
    (1 - prev_adj$protection_gen_male_unc)
  
  # Newly circumcised men use the circ beta (lower)
  infections_gen_male_circ <- betas$beta_gen_male_circ *
    infectious_pressure_scenario *
    n_circ_eff
  # (no additional prevention adjustment for circ men — their lower β is the protection)
  
  total_new_infections <- infections_high +
    infections_gen_female +
    infections_gen_male_unc +
    infections_gen_male_circ
  
  # Clamp to non-negative
  total_new_infections <- max(0, total_new_infections)
  
  # Step 8: Return results with stratum breakdown
  list(
    new_infections            = round(total_new_infections),
    infections_averted        = round(max(0, context$new_infections_per_year - total_new_infections)),
    
    # Stratum breakdown (useful for diagnostics / charts)
    by_stratum = list(
      high_risk      = round(infections_high),
      gen_female     = round(infections_gen_female),
      gen_male_uncirc = round(infections_gen_male_unc),
      gen_male_circ  = round(infections_gen_male_circ)
    ),
    
    # Calibration diagnostics
    diagnostics = list(
      beta_high          = betas$beta_high,
      beta_gen_female    = betas$beta_gen_female,
      beta_gen_male_unc  = betas$beta_gen_male_unc,
      beta_gen_male_circ = betas$beta_gen_male_circ,
      frac_infections_high_risk = betas$frac_high,
      n_unsuppressed_baseline   = strata$n_unsuppressed,
      n_unsuppressed_scenario   = n_unsuppressed_scenario,
      infectious_pressure_baseline = strata$n_unsuppressed / populations$total,
      infectious_pressure_scenario = infectious_pressure_scenario,
      vmmc_newly_circumcised    = round(n_newly_circ)
    )
  )
}


# ============================================================================
# STEP 6: CALIBRATION VALIDATION
# ============================================================================
#
# WHAT THIS DOES:
#   Takes the calibrated βs and checks them against biologically plausible
#   bounds derived from the per-act transmission literature scaled to
#   annual partnership rates. Flags any stratum where β is implausible,
#   and explains *why* in plain language so you can fix the upstream inputs.
#
# BIOLOGICAL GROUNDING:
#   Annual per-partnership transmission probability ≈
#     per-act probability × acts per year × (1 - condom use × efficacy)
#
#   Per-act HIV transmission probabilities (receptive, no ART, no condom):
#     - Male-to-female vaginal:     ~0.001 - 0.003
#     - Female-to-male vaginal:     ~0.0003 - 0.001
#     - Receptive anal (MSM/FSW):   ~0.01  - 0.03
#
#   Acts per year in a regular partnership: ~50-100
#   → Annual per-partnership probability range:
#       General population:  ~0.05 - 0.20   (cumulative over ~100 acts)
#       High-risk/KP:        ~0.20 - 0.80   (more partners, higher per-act risk)
#
#   In the FOI model, β is NOT a per-act probability — it is a macro-level
#   rate that maps infectious_pressure (proportion of population unsuppressed)
#   × susceptible count → annual infections. Its plausible range therefore
#   depends on epidemic context:
#
#     β_general:   typically 0.01 - 0.30  for generalized SSA epidemics
#     β_high_risk: typically 0.10 - 2.00  (can exceed 1 as it's a rate, not prob)
#
#   Values outside these ranges suggest a data inconsistency upstream —
#   usually one of:
#     (a) new_infections_per_year is misspecified (wrong year, wrong source)
#     (b) hiv_prevalence is too low/high relative to incidence
#     (c) percent_suppressed is inconsistent with incidence
#     (d) prop_high_risk or rr_high is implausible for this country
#
# RETURNS:
#   A list with:
#     $valid        — TRUE/FALSE overall pass
#     $flags        — character vector of specific warnings (empty if valid)
#     $beta_table   — data.frame with β values, bounds, and pass/fail per stratum
#     $incidence_check — observed vs implied incidence rate as a sanity check
#     $narrative    — plain-English summary suitable for a UI warning panel

validate_calibration <- function(context, populations, betas, strata, strata_params) {
  
  flags   <- character(0)
  
  # ------------------------------------------------------------------
  # 1. Define plausible bounds per stratum
  # ------------------------------------------------------------------
  bounds <- list(
    beta_high = list(
      lower = 0.05,   # CALIBRATE: minimum plausible for KP/high-risk
      upper = 3.00,   # CALIBRATE: maximum — very high epidemic intensity
      label = "High-risk (KP)"
    ),
    beta_gen_female = list(
      lower = 0.005,  # CALIBRATE: minimum for general female
      upper = 0.50,   # CALIBRATE: maximum for general female
      label = "General (female)"
    ),
    beta_gen_male_unc = list(
      lower = 0.003,  # CALIBRATE: lower than female (male acquisition lower per-act)
      upper = 0.40,
      label = "General (uncircumcised male)"
    ),
    beta_gen_male_circ = list(
      lower = 0.001,
      upper = 0.20,   # CALIBRATE: circumcised males, lowest risk
      label = "General (circumcised male)"
    )
  )
  
  # ------------------------------------------------------------------
  # 2. Check each β against bounds
  # ------------------------------------------------------------------
  beta_values <- list(
    beta_high          = betas$beta_high,
    beta_gen_female    = betas$beta_gen_female,
    beta_gen_male_unc  = betas$beta_gen_male_unc,
    beta_gen_male_circ = betas$beta_gen_male_circ
  )
  
  beta_table <- do.call(rbind, lapply(names(beta_values), function(key) {
    b     <- beta_values[[key]]
    bound <- bounds[[key]]
    pass  <- !is.na(b) && b >= bound$lower && b <= bound$upper
    
    if (!pass) {
      direction <- if (!is.na(b) && b < bound$lower) "too low" else "too high"
      flags <<- c(flags, sprintf(
        "%s: β = %.4f (%s; plausible range %.3f–%.2f)",
        bound$label, b, direction, bound$lower, bound$upper
      ))
    }
    
    data.frame(
      stratum    = bound$label,
      beta       = round(b, 5),
      lower      = bound$lower,
      upper      = bound$upper,
      pass       = pass,
      stringsAsFactors = FALSE
    )
  }))
  
  # ------------------------------------------------------------------
  # 3. Incidence rate check
  # ------------------------------------------------------------------
  # Observed incidence = new infections / HIV-negative sexually active pop
  # Plausible range for SSA: ~0.001 (0.1%) to 0.05 (5%) per year
  # Above 3% is a very high-incidence setting; above 5% is almost certainly
  # a data error or very localised subpopulation estimate.
  
  obs_incidence <- context$new_infections_per_year /
    max(populations$hiv_negative, 1)
  
  incidence_pct <- round(obs_incidence * 100, 3)
  
  if (obs_incidence > 0.05) {
    flags <- c(flags, sprintf(
      "Implied annual incidence = %.2f%% among HIV-negative — unusually high (>5%%). Check new_infections_per_year and prevalence inputs.",
      incidence_pct
    ))
  } else if (obs_incidence < 0.001) {
    flags <- c(flags, sprintf(
      "Implied annual incidence = %.3f%% among HIV-negative — very low (<0.1%%). Model may underestimate prevention impact.",
      incidence_pct
    ))
  }
  
  # ------------------------------------------------------------------
  # 4. Infections-vs-unsuppressed consistency check
  # ------------------------------------------------------------------
  # If n_unsuppressed is very small relative to new_infections,
  # β has to be enormous to compensate — a red flag.
  # Rule of thumb: new_infections should not exceed ~20% of unsuppressed
  # (each unsuppressed person cannot plausibly cause >1 infection/year on average
  #  in a population-level model)
  
  ratio_inf_to_unsup <- context$new_infections_per_year / max(strata$n_unsuppressed, 1)
  
  if (ratio_inf_to_unsup > 0.5) {
    flags <- c(flags, sprintf(
      "new_infections / unsuppressed_PLHIV = %.2f — implies implausibly high per-person transmission. Check suppression rate or infection counts.",
      ratio_inf_to_unsup
    ))
  }
  
  # ------------------------------------------------------------------
  # 5. High-risk fraction contribution check
  # ------------------------------------------------------------------
  # If rr_high is large and prop_high_risk is non-trivial,
  # the high-risk stratum can dominate infections.
  # Flag if >80% of calibrated infections come from <10% of population.
  
  if (betas$frac_high > 0.80 && strata_params$prop_high_risk < 0.10) {
    flags <- c(flags, sprintf(
      "%.0f%% of infections attributed to high-risk stratum (%.0f%% of population). Consider increasing prop_high_risk or reducing rr_high.",
      betas$frac_high * 100, strata_params$prop_high_risk * 100
    ))
  }
  
  # ------------------------------------------------------------------
  # 6. Build narrative summary
  # ------------------------------------------------------------------
  n_flags <- length(flags)
  
  narrative <- if (n_flags == 0) {
    sprintf(
      paste0(
        "Calibration passed. Implied annual incidence: %.3f%% among HIV-negative adults. ",
        "High-risk stratum accounts for %.0f%% of baseline infections. ",
        "All stratum-level transmission rates fall within biologically plausible bounds."
      ),
      incidence_pct,
      betas$frac_high * 100
    )
  } else {
    sprintf(
      paste0(
        "%d calibration warning(s) detected. Implied incidence: %.3f%%. ",
        "Review flagged parameters before interpreting scenario results. ",
        "Most common causes: mismatched data years, prevalence/incidence inconsistency, ",
        "or implausible suppression rates relative to reported infections."
      ),
      n_flags,
      incidence_pct
    )
  }
  
  # ------------------------------------------------------------------
  # 7. Return structured result
  # ------------------------------------------------------------------
  list(
    valid             = (n_flags == 0),
    flags             = flags,
    beta_table        = beta_table,
    incidence_check   = list(
      observed_rate_pct   = incidence_pct,
      new_infections      = context$new_infections_per_year,
      hiv_negative_pop    = round(populations$hiv_negative),
      n_unsuppressed      = round(strata$n_unsuppressed),
      ratio_inf_unsup     = round(ratio_inf_to_unsup, 3)
    ),
    narrative         = narrative
  )
}


# ----------------------------------------------------------------------------
# CONVENIENCE WRAPPER: run FOI + validation together
# ----------------------------------------------------------------------------
# Use this in calculate_impact() or in a renderUI() diagnostics panel.
# Returns the full foi_result with a $validation slot appended.

estimate_and_validate <- function(context,
                                  populations,
                                  scenario_interventions,
                                  suppression_delta = 0,
                                  strata_params = NULL) {
  
  if (is.null(strata_params)) strata_params <- define_strata_params(context)
  
  strata    <- partition_into_strata(populations, strata_params)
  betas     <- calibrate_beta(context, populations, strata, strata_params)
  
  # Run validation against BASELINE betas (calibration is always baseline)
  validation <- validate_calibration(context, populations, betas, strata, strata_params)
  
  # Run FOI for scenario
  foi_result <- estimate_new_infections_foi(
    context                = context,
    populations            = populations,
    scenario_interventions = scenario_interventions,
    suppression_delta      = suppression_delta,
    strata_params          = strata_params
  )
  
  # Append validation to result
  foi_result$validation <- validation
  foi_result
}


# ----------------------------------------------------------------------------
# SHINY UI HELPER: render a calibration warning panel
# ----------------------------------------------------------------------------
# Drop this into your server as:
#   output$calibration_check <- renderUI({ render_calibration_panel(foi_result) })
# And add uiOutput("calibration_check") somewhere in your Results tab.

render_calibration_panel <- function(foi_result) {
  
  v <- foi_result$validation
  
  # Colour and icon based on pass/fail
  panel_class  <- if (v$valid) "alert alert-success" else "alert alert-warning"
  icon_html    <- if (v$valid) "\u2713 " else "\u26A0\uFE0F "  # ✓ or ⚠️
  
  # Beta table as HTML rows
  beta_rows <- lapply(1:nrow(v$beta_table), function(i) {
    row   <- v$beta_table[i, ]
    color <- if (row$pass) "green" else "red"
    tags$tr(
      tags$td(row$stratum),
      tags$td(style = paste0("color:", color, "; font-weight:bold;"),
              formatC(row$beta, format = "f", digits = 5)),
      tags$td(paste0(row$lower, " – ", row$upper)),
      tags$td(style = paste0("color:", color),
              if (row$pass) "\u2713 OK" else "\u2717 FLAG")
    )
  })
  
  tagList(
    div(class = panel_class,
        strong(paste0(icon_html, "Calibration Check")),
        p(v$narrative)
    ),
    
    if (!v$valid) {
      div(class = "alert alert-warning",
          strong("Specific warnings:"),
          tags$ul(lapply(v$flags, tags$li))
      )
    },
    
    # Compact beta table
    tags$table(
      class = "table table-sm table-bordered",
      style = "font-size: 0.85em; margin-top: 10px;",
      tags$thead(tags$tr(
        tags$th("Stratum"),
        tags$th("\u03B2 (calibrated)"),   # β
        tags$th("Plausible range"),
        tags$th("Status")
      )),
      tags$tbody(beta_rows)
    ),
    
    # Incidence summary line
    div(style = "font-size: 0.85em; color: #555; margin-top: 8px;",
        sprintf(
          "Implied incidence: %.3f%% | Unsuppressed PLHIV: %s | Infections/unsuppressed ratio: %.3f",
          v$incidence_check$observed_rate_pct,
          format(v$incidence_check$n_unsuppressed, big.mark = ","),
          v$incidence_check$ratio_inf_unsup
        )
    )
  )
}


# ============================================================================
# INTEGRATION NOTE — how to wire this into calculate_impact()
# ============================================================================
#
# In calculate_impact(), replace this block:
#
#   } else if ("adult_infections" %in% intervention$outcomes) {
#     incidence_rate <- context$new_infections_per_year / populations$hiv_negative
#     infections_averted <- infections_averted +
#       sign * number_reached * incidence_rate * intervention$efficacy
#     ...
#   }
#
# With a call to estimate_new_infections_foi() AFTER the loop:
#
#   # After processing all interventions, compute new infections via FOI
#   foi_result <- estimate_new_infections_foi(
#     context                = context,
#     populations            = populations,
#     scenario_interventions = target,          # the scenario being evaluated
#     suppression_delta      = additional_suppressed  # from treatment arm above
#   )
#
#   new_infections      <- foi_result$new_infections
#   infections_averted  <- foi_result$infections_averted
#
# And remove the per-intervention infections_averted accumulation for
# adult_infections — the FOI function now handles all of that holistically.
#
# The prevention interventions (prep_oral, vmmc, condoms, pep) still go
# through calculate_impact() for their COST calculation — just not for
# their infections_averted count. That now comes from FOI.
#
# ============================================================================
#
# PARAMETERS TO ADD TO country CSV (optional country-level overrides):
#   circ_prevalence      — male circumcision prevalence (0-1), default 0.20
#   prop_high_risk       — fraction of HIV-neg sexually active who are KP/high-risk
#   rr_high              — relative transmission risk, high vs general
#
# ============================================================================
