# ============================================================================
# test_12_clinical_visit.R
# ----------------------------------------------------------------------------
# Tests for the clinical_visit_12month intervention (annual vs 6-monthly
# clinical visits for stable ART clients) inside calculate_scenario_outcomes().
#
# WHAT THIS INTERVENTION DOES (as implemented in Mock-Up_logic_V3.R):
#   - EFFECT (deferred, post-loop, ~line 2274): raises viral suppression by
#     moving people out of the unsuppressed-ESTABLISHED pool. Because the model
#     equates "stable" with "suppressed" (prop_on_art_stable_diff = 0), there is
#     no stable-specific unsuppressed pool; the effect acts on n_est_treated_base.
#       *** THIS FILE ASSERTS THE X-EQUIVALENT EFFECT BLOCK ***
#           additional_suppressed += n_established_on_art * cov_frac * target_pp
#       where `efficacy` (from the Excel sheet) is a TARGET percentage-point
#       gain (0.01 = +1pp), applied to the established headcount so the pp gain
#       is constant across countries. If you revert to the Y block
#           additional_suppressed += n_est_treated_base * cov_frac * efficacy
#       the effect formula and 12.1's expected numbers change (under Y the shift
#       is n_est_treated_base * efficacy). Ask for updated expected values before
#       reverting rather than re-pointing the path.
#   - COST (in-loop, ~line 1806): charged on STABLE clients enrolled
#       clinical_visit_cost_adjustment += number_reached * art_cost_standard * unit_cost
#     where number_reached = on_art_stable * coverage, unit_cost is a FRACTION of
#     art_cost_standard (negative = saving). Lands in art_provision_cost, NOT
#     total_intervention_cost.
#
# TEST-INPUT PARAMETER VALUES:
#   efficacy and unit_cost below (0.10, -0.05, etc.) are ARBITRARY TEST INPUTS
#   injected via with_intervention_groups(). They are NOT sourced parameter
#   estimates and make no claim about the real intervention. Each expected
#   OUTPUT is derived from these inputs via the formulas above, so the tests
#   check the logic's arithmetic, not any real-world magnitude. Real efficacy/
#   unit_cost come from the Excel intervention_params sheet at deploy time.
#
# FIXTURE (shared with test_07 / test_08, prop_on_art_stable_diff = 0):
#   on_art                = 45,000 * 0.80        = 36,000
#   on_art_stable         = 36,000 * 0.90        = 32,400   (== suppressed pop)
#   n_established_on_art  = 34,070.4             (from test_07 derivation)
#   pct_supp_frac         = 0.90
#   n_est_supp_base       = 34,070.4 * 0.90      = 30,663.36
#   n_est_treated_base    = 34,070.4 * 0.10      =  3,407.04
#
# REQUIRES: the logic file sourced by helpers.R must be a V3 (or later) file
#   that defines clinical_visit_12month. run_all_tests.R and helpers.R currently
#   default HIV_LOGIC_PATH to a V2 file; point it at Mock-Up_logic_V3.R or these
#   tests skip. skip_if_no_cv12() below makes that explicit rather than failing
#   with a confusing NULL-subscript error.
# ============================================================================

source("helpers.R")

# --- Same deterministic mortality + cascade params as test_08 --------------
LIVE_MORT_RATES <- list(
  untreated_undiagnosed = 0.012,
  new_art_initiations    = 0.006,
  treated                = 0.0051,
  suppressed             = 0.0051,
  ahd_new                = 0.144,
  ahd_established        = 0.028,
  ahd_untreated          = 0.254,
  prop_ahd = list(
    undiagnosed         = 0.154,
    diagnosed_not_art   = 0.209,
    new_initiations     = 0.209,
    established_treated = 0.295,
    established_supp    = 0.043
  )
)

# All-zero mortality: used by the EFFECT test so the treated->supp shift passes
# through to end_suppressed 1:1 (no AHD-blended mortality differential between
# the established_treated and established_supp groups to muddy the derivation).
ZERO_MORT_RATES <- modifyList(LIVE_MORT_RATES, list(
  untreated_undiagnosed = 0, new_art_initiations = 0,
  treated = 0, suppressed = 0,
  ahd_new = 0, ahd_established = 0, ahd_untreated = 0
))

LIVE_PARAMS_CASCADE <- list(
  sexually_active_frac            = 0.85,
  ltfu_rate_stable                = 0.044,
  ltfu_rate_unstable               = 0.14,
  spontaneous_reengagement_rate    = 0,
  retention_suppression_rate       = 0.41,
  tracking_reengagement_supp       = 0.9,
  prop_on_art_stable_diff          = 0,
  testing_reengagement_cap_frac    = 0.45,
  testing_art_init_supp            = 0.9,
  prop_retest_default              = 0.59,
  new_diagnoses_cap_prop           = 0.95,
  average_linkage_cap              = 0.93,
  pmtct_cascade_supp_discount      = 0.9,
  eid_supp_rate                    = 0.38,
  prop_cd4_ahd                     = 0.4
)

zero_interventions <- function() {
  setNames(as.list(rep(0, length(default_baseline_interventions))),
           names(default_baseline_interventions))
}

# Skip guard: fail loudly-but-clearly if the sourced logic file predates the
# intervention (e.g. HIV_LOGIC_PATH still points at V2).
skip_if_no_cv12 <- function() {
  cv <- tryCatch(
    intervention_groups$treatment_monitoring$interventions$clinical_visit_12month,
    error = function(e) NULL)
  if (is.null(cv)) {
    testthat::skip(paste0(
      "clinical_visit_12month not found in intervention_groups. Point ",
      "HIV_LOGIC_PATH at Mock-Up_logic_V3.R (see run_all_tests.R line ~46)."))
  }
}

# Inject known test efficacy/unit_cost for clinical_visit_12month by swapping
# the whole treatment_monitoring group (restored automatically at test exit).
override_cv12 <- function(efficacy, unit_cost, envir = parent.frame()) {
  ig_new <- intervention_groups
  ig_new$treatment_monitoring$interventions$clinical_visit_12month$efficacy  <- efficacy
  ig_new$treatment_monitoring$interventions$clinical_visit_12month$unit_cost <- unit_cost
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring),
                           envir = envir)
}

# Override mortality + LTFU globals (mirrors test_08's override_cascade_globals,
# with a settable mortality table so the effect test can zero it).
override_cascade_globals <- function(mort = LIVE_MORT_RATES, envir = parent.frame()) {
  snap <- list(
    s = ANNUAL_LTFU_RATE_STABLE, u = ANNUAL_LTFU_RATE_UNSTABLE,
    sp = ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE, rs = RETENTION_SUPPRESSION_RATE,
    cd4 = CD4_AHD_TARGETING_YIELD, use = USE_MORTALITY_CALIBRATION,
    mort = MORTALITY_RATES
  )
  assign("ANNUAL_LTFU_RATE_STABLE",              0.044,  envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE",            0.14,   envir = .GlobalEnv)
  assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", 0,      envir = .GlobalEnv)
  assign("RETENTION_SUPPRESSION_RATE",           0.41,   envir = .GlobalEnv)
  assign("CD4_AHD_TARGETING_YIELD",              0.4,    envir = .GlobalEnv)
  assign("USE_MORTALITY_CALIBRATION",            FALSE,  envir = .GlobalEnv)
  assign("MORTALITY_RATES",                      mort,   envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute({
            assign("ANNUAL_LTFU_RATE_STABLE",              SNAP_S,    envir = .GlobalEnv)
            assign("ANNUAL_LTFU_RATE_UNSTABLE",            SNAP_U,    envir = .GlobalEnv)
            assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", SNAP_SP,   envir = .GlobalEnv)
            assign("RETENTION_SUPPRESSION_RATE",           SNAP_RS,   envir = .GlobalEnv)
            assign("CD4_AHD_TARGETING_YIELD",              SNAP_CD4,  envir = .GlobalEnv)
            assign("USE_MORTALITY_CALIBRATION",            SNAP_USE,  envir = .GlobalEnv)
            assign("MORTALITY_RATES",                      SNAP_MORT, envir = .GlobalEnv)
          }, list(SNAP_S = snap$s, SNAP_U = snap$u, SNAP_SP = snap$sp,
                  SNAP_RS = snap$rs, SNAP_CD4 = snap$cd4,
                  SNAP_USE = snap$use, SNAP_MORT = snap$mort)),
          add = TRUE),
          envir = envir)
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# 12.1 EFFECT: constant +target_pp on established suppression (X-equivalent)
# ---------------------------------------------------------------------------
# WHAT: At 100% coverage the intervention adds
#         n_established_on_art * 1.0 * target_pp = 34,070.4 * 0.01 = 340.704
#       to additional_suppressed. That shift (340.704 < n_est_treated_base =
#       3,407.04, so the cap does NOT bind) moves 340.704 people from the
#       unsuppressed-established pool into suppressed. With mortality zeroed it
#       passes through to end_suppressed unchanged.
# WHY:  Pins the X-equivalent formula (headcount * pp) AND its defining
#       property: a CONSTANT +1pp on established suppression regardless of the
#       country's baseline suppression. Also confirms it's a pure reshuffle
#       WITHIN on-ART (end_on_art must not move).
# HOW:  Compare coverage-100 vs coverage-0 (all else equal, both is_baseline).
#         d(end_suppressed)          = 340.704            -> ~341 after rounding
#         established suppression     = 90.00% -> 91.00%   (+1.00pp)
#         d(end_on_art)              = 0                  (treated<->supp reshuffle)
#       target_pp = 0.01 is chosen so the cap does NOT bind. Note target_pp =
#       0.10 would SATURATE (34,070.4*0.10 = 3,407.04 = n_est_treated_base) — a
#       fixture coincidence to avoid when picking a test value under X.
# NOTE (Y revert): under the Y block the shift is n_est_treated_base * efficacy,
#       so the same +1pp effect would require efficacy = 0.10, not 0.01.
# ---------------------------------------------------------------------------
test_that("clinical_visit_12month adds a constant +target_pp to established suppression (X)", {
  skip_if_no_cv12()
  with_hiv_params(LIVE_PARAMS_CASCADE)
  override_cascade_globals(mort = ZERO_MORT_RATES)   # isolate shift from mortality
  override_cv12(efficacy = 0.01, unit_cost = 0)      # TEST target_pp = +1pp

  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)

  base_interv <- zero_interventions()
  cv_interv   <- zero_interventions()
  cv_interv$clinical_visit_12month <- 100

  r_base <- calculate_scenario_outcomes(
    ctx, base_interv, pops, is_baseline = TRUE, baseline_interventions = base_interv)
  r_cv   <- calculate_scenario_outcomes(
    ctx, cv_interv,   pops, is_baseline = TRUE, baseline_interventions = cv_interv)

  
  # Sanity: zeroed-mortality baseline end_suppressed = n_est_supp_base = 30,663.36
  expect_lte(abs(r_base$end_suppressed - 30663), 2)

  # Effect: +340.704 suppressed (rounds to ~341)
  expect_lte(abs((r_cv$end_suppressed - r_base$end_suppressed) - 341), 2)
  #print(r_cv$end_suppressed)
  # Defining X property: established suppression rises by a constant +1pp
  supp_frac_base <- r_base$end_suppressed / r_base$end_on_art
  supp_frac_cv   <- r_cv$end_suppressed   / r_cv$end_on_art
  expect_lte(abs((supp_frac_cv - supp_frac_base) - 0.01), 0.001)

  # Pure reshuffle within on-ART: end_on_art unchanged
  expect_lte(abs(r_cv$end_on_art - r_base$end_on_art), 2)
})

# ---------------------------------------------------------------------------
# 12.2 COST: charged on stable clients, into art_provision_cost only
# ---------------------------------------------------------------------------
# WHAT: With unit_cost = -0.05 (5% of art_cost_standard saved per enrolled
#       stable client) and art_cost_standard = 200 injected into context:
#         d(art_provision_cost) = on_art_stable * unit_cost * art_cost_standard
#                               = 32,400 * (-0.05) * 200 = -324,000
# WHY:  Pins (a) the cost denominator = stable clients (32,400), (b) the DSD
#       fractional convention (unit_cost * art_cost_standard), and (c) that the
#       cost lands in art_provision_cost, NOT total_intervention_cost.
# HOW:  efficacy = 0 so there is NO suppression shift -> end_on_art identical
#       between runs -> the end_on_art*art_cost_unit term cancels and the whole
#       difference is the clinical-visit cost adjustment.
# ---------------------------------------------------------------------------
test_that("clinical_visit_12month cost = on_art_stable * unit_cost * art_cost_standard", {
  skip_if_no_cv12()
  with_hiv_params(LIVE_PARAMS_CASCADE)
  override_cascade_globals()                         # LIVE mortality is fine here
  override_cv12(efficacy = 0, unit_cost = -0.05)     # arbitrary TEST unit_cost

  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list(),
                               art_cost_standard = 200)   # injected TEST cost
  pops <- calculate_populations(ctx)

  base_interv <- zero_interventions()
  cv_interv   <- zero_interventions()
  cv_interv$clinical_visit_12month <- 100

  r_base <- calculate_scenario_outcomes(
    ctx, base_interv, pops, is_baseline = TRUE, baseline_interventions = base_interv)
  r_cv   <- calculate_scenario_outcomes(
    ctx, cv_interv,   pops, is_baseline = TRUE, baseline_interventions = cv_interv)

  # -324,000 saving in ART provision cost
  expect_lte(abs((r_cv$art_provision_cost - r_base$art_provision_cost) - (-324000)), 2)

  # Cost must NOT touch total_intervention_cost (it uses `next` before that line)
  expect_lte(abs(r_cv$total_intervention_cost - r_base$total_intervention_cost), 2)
})

# ---------------------------------------------------------------------------
# 12.3 INVARIANT: cascade monotonicity holds at full coverage + max efficacy
# ---------------------------------------------------------------------------
# WHAT: efficacy = 1.0, coverage = 100 tries to convert the ENTIRE
#       unsuppressed-established pool. The min(intervention_supp_shift,
#       n_est_treated_base) cap (line ~2321) must stop end_suppressed exceeding
#       end_on_art.
# WHY:  Same class of guard as test 8.6, but exercised through THIS lever
#       (8.6 leaves clinical_visit_12month at 0 and never tests it).
# HOW:  Assert end_suppressed <= end_on_art <= end_diagnosed <= end_plhiv.
# ---------------------------------------------------------------------------
test_that("cascade ordering holds with clinical_visit_12month at max coverage/efficacy", {
  skip_if_no_cv12()
  with_hiv_params(LIVE_PARAMS_CASCADE)
  override_cascade_globals()
  override_cv12(efficacy = 1.0, unit_cost = 0)       # stress the shift cap

  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$clinical_visit_12month <- 100

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv)

  expect_lte(result$end_suppressed, result$end_on_art)
  expect_lte(result$end_on_art,     result$end_diagnosed)
  expect_lte(result$end_diagnosed,  result$end_plhiv)
})

# ---------------------------------------------------------------------------
# 12.4 NO-OP: at coverage 0 the intervention is inert regardless of params
# ---------------------------------------------------------------------------
# WHAT: With coverage 0, neither the (in-loop) cost branch nor the (post-loop)
#       effect term contributes, even when efficacy/unit_cost are large. Two
#       coverage-0 runs — one with zero params, one with large params — must be
#       identical.
# WHY:  Directly guards the "does adding this key disturb the baseline cascade?"
#       concern. The effect block runs unconditionally (outside the loop), so
#       this pins that cov_frac = 0 zeroes it and the cost branch is skipped.
# HOW:  Compare end_suppressed and art_provision_cost across the two runs.
# ---------------------------------------------------------------------------
test_that("clinical_visit_12month contributes nothing at coverage 0", {
  skip_if_no_cv12()
  with_hiv_params(LIVE_PARAMS_CASCADE)
  override_cascade_globals()

  ctx  <- make_fixture_context(test_yield = 0.05, prior_year_tests = NULL,
                               yield_multipliers = list(), art_cost_standard = 200)
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()          # clinical_visit_12month = 0

  # Run A: zero params
  override_cv12(efficacy = 0, unit_cost = 0)
  r_zero <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv)

  # Run B: large params, but still coverage 0 -> must match Run A
  override_cv12(efficacy = 0.5, unit_cost = -0.5)
  r_big  <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv)

  expect_lte(abs(r_big$end_suppressed    - r_zero$end_suppressed),    1)
  expect_lte(abs(r_big$art_provision_cost - r_zero$art_provision_cost), 1)
})
