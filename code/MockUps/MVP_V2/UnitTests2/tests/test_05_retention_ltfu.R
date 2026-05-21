# ============================================================================
# test_05_retention_ltfu.R
# ----------------------------------------------------------------------------
# Tests for LTFU prevention and re-engagement logic inside
# calculate_scenario_outcomes():
#
#   - DSD interventions (mmd_3/6/12, community_pickup) are additive in their
#     contribution to ltfu_retained_frac (they're mutually exclusive on the
#     same stable patient by UI constraint)
#   - tracking_tracing is DEFERRED in the intervention loop and applied
#     against (prevalent_ltfu + ltfu_new_effective) AFTER prevention resolves
#   - ltfu_prevented applies only to ltfu_new_stable (current code only has
#     DSD interventions, all on_art_stable)
#   - spontaneous re-engagement uses the GROSS pool, not the residual after
#     testing/tracking (prevents perverse scale-down behaviour)
#   - testing_reengagement_cap binds only testing-driven flow, not tracking
#     or spontaneous
#   - DSD costs apply to ALL stable clients reached (not just retained)
#   - Tracking cost applies to the deferred pool
#
# Parameter values used (locked via with_hiv_params):
#   ltfu_rate_stable                 = 0.044
#   ltfu_rate_unstable               = 0.14
#   spontaneous_reengagement_rate    = 0       (your live value, currently disabled)
#   retention_suppression_rate       = 0.41
#   tracking_reengagement_supp       = 0.9
#   prop_on_art_stable_diff          = 0
#   testing_reengagement_cap_frac    = 0.45
#   testing_art_init_supp            = 0.9
# ============================================================================

source("helpers.R")  # rename test_helpers.R -> helpers.R per earlier discussion;
                     # if you kept it as test_helpers.R, change this line back.

# Live hiv_params values, locked here for reproducibility
LIVE_PARAMS_RETENTION <- list(
  sexually_active_frac           = 0.85,   # your live value
  ltfu_rate_stable               = 0.044,
  ltfu_rate_unstable              = 0.14,
  spontaneous_reengagement_rate   = 0,
  retention_suppression_rate      = 0.41,
  tracking_reengagement_supp      = 0.9,
  prop_on_art_stable_diff         = 0,
  testing_reengagement_cap_frac   = 0.45,
  testing_art_init_supp           = 0.9,
  prop_retest_default             = 0.59,
  new_diagnoses_cap_prop          = 0.95,
  average_linkage_cap             = 0.93,
  pmtct_cascade_supp_discount     = 0.9
)

# Helper: zero out every intervention except the one(s) the test exercises
zero_interventions <- function() {
  setNames(as.list(rep(0, length(default_baseline_interventions))),
           names(default_baseline_interventions))
}

# Helper: build a retention-clean fixture context that produces deterministic
# year-start LTFU populations. With:
#   plhiv = 50,000, diagnosed = 90%, on_art = 80%, suppressed = 90%, stable_diff = 0
#     on_art        = 50,000 × 0.9 × 0.8 = 36,000
#     on_art_stable = 36,000 × ((90 + 0)/100) = 32,400
#     on_art_unstable = 36,000 - 32,400 = 3,600
#   ltfu (prevalent) = diagnosed - on_art = 45,000 - 36,000 = 9,000
#   ltfu_new_stable   = 32,400 × 0.044 = 1,425.6
#   ltfu_new_unstable = 3,600  × 0.14  = 504
#   ltfu_new (gross)  = 1,425.6 + 504  = 1,929.6
# These numbers anchor every test below.
retention_context <- function(...) {
  make_fixture_context(...)
}

# Helper: set the LTFU rate globals (top-level constants in the source file)
# to match the parameter overrides. Restores on exit of the test_that block.
override_ltfu_rates <- function(stable = 0.044, unstable = 0.14,
                                spontaneous = 0,
                                retention_supp = 0.41,
                                envir = parent.frame()) {
  snap <- list(
    s   = ANNUAL_LTFU_RATE_STABLE,
    u   = ANNUAL_LTFU_RATE_UNSTABLE,
    sp  = ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE,
    rs  = RETENTION_SUPPRESSION_RATE
  )
  assign("ANNUAL_LTFU_RATE_STABLE",          stable,        envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE",        unstable,      envir = .GlobalEnv)
  assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE", spontaneous, envir = .GlobalEnv)
  assign("RETENTION_SUPPRESSION_RATE",       retention_supp, envir = .GlobalEnv)
  do.call("on.exit",
          list(substitute({
            assign("ANNUAL_LTFU_RATE_STABLE",                SNAP_S,  envir = .GlobalEnv)
            assign("ANNUAL_LTFU_RATE_UNSTABLE",              SNAP_U,  envir = .GlobalEnv)
            assign("ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE",   SNAP_SP, envir = .GlobalEnv)
            assign("RETENTION_SUPPRESSION_RATE",             SNAP_RS, envir = .GlobalEnv)
          }, list(SNAP_S = snap$s, SNAP_U = snap$u, SNAP_SP = snap$sp, SNAP_RS = snap$rs)),
          add = TRUE),
          envir = envir)
  invisible(NULL)
}

# Helper: override one retention intervention's params; preserves the rest.
# Routes each intervention to its correct group: tracking_tracing lives in
# retention_support; DSD interventions (mmd_3/6/12, community_pickup) live
# in treatment_monitoring.
override_retention <- function(int_key, efficacy, unit_cost) {
  ig_new <- intervention_groups
  if (int_key == "tracking_tracing") {
    ig_new$retention_support$interventions[[int_key]]$efficacy  <- efficacy
    ig_new$retention_support$interventions[[int_key]]$unit_cost <- unit_cost
  } else {
    # DSD interventions live in treatment_monitoring
    ig_new$treatment_monitoring$interventions[[int_key]]$efficacy  <- efficacy
    ig_new$treatment_monitoring$interventions[[int_key]]$unit_cost <- unit_cost
  }
  ig_new
}

# ---------------------------------------------------------------------------
# 5.1 Year-start LTFU flow split (verifies test_helpers fixture)
# ---------------------------------------------------------------------------
# WHAT: With ltfu_rate_stable = 0.044, ltfu_rate_unstable = 0.14, the gross
#       incident LTFU flow at year-start is determined by the stability mix
#       of on_art.
# WHY:  All downstream retention tests depend on these baseline values being
#       what I claim they are. Lock them down with the LIVE rates.
# HOW:  Fixture: on_art = 36,000; on_art_stable = 32,400; on_art_unstable = 3,600.
#         ltfu_new_stable   = 32,400 × 0.044 = 1,425.6
#         ltfu_new_unstable = 3,600  × 0.14  = 504
#         ltfu_new (gross)  = 1,929.6
# ---------------------------------------------------------------------------
test_that("year-start LTFU flow uses live ltfu_rate_stable/unstable values", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  pops <- calculate_populations(retention_context())

  expect_close(pops$on_art_stable,        32400)
  expect_close(pops$ltfu_new_stable,      32400 * 0.044)   # 1425.6
  expect_close(pops$ltfu_new_unstable,    3600  * 0.14)    # 504
  expect_close(pops$ltfu_new,             32400 * 0.044 + 3600 * 0.14)
  expect_close(pops$ltfu,                 9000)            # prevalent
})

# ---------------------------------------------------------------------------
# 5.2 Single DSD intervention: additive contribution to ltfu_retained_frac
# ---------------------------------------------------------------------------
# WHAT: With one DSD at 50% coverage and efficacy 0.10:
#       coverage_frac      = (32,400 × 0.50) / 32,400 = 0.50
#       ltfu_retained_frac = 0 + 0.50 × 0.10 = 0.05
#       ltfu_prevented     = ltfu_new_stable × 0.05 = 1,425.6 × 0.05 = 71.28
#       After round() in the return list: 71.
# WHY:  Locks the additive formula for a single DSD term. If the test fails,
#       either the coverage_frac denominator or the addition rule changed.
# HOW:  Override mmd_3month efficacy = 0.10, unit_cost = 0. Set mmd_3month
#       coverage to 50.
# ---------------------------------------------------------------------------
test_that("single DSD intervention contributes additively to ltfu_prevented", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  ig_new <- override_retention("mmd_3month", efficacy = 0.10, unit_cost = 0)
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring))

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$mmd_3month <- 50    # 50% coverage of stable

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # Expected ltfu_prevented = round(1,425.6 × 0.05) = round(71.28) = 71
  expect_lte(abs(result$ltfu_prevented - 71), 1)
})

# ---------------------------------------------------------------------------
# 5.3 Two DSD interventions sum additively
# ---------------------------------------------------------------------------
# WHAT: With mmd_3month (cov 40%, eff 0.10) + mmd_6month (cov 30%, eff 0.20):
#       ltfu_retained_frac = 0 + 0.40 × 0.10 + 0.30 × 0.20 = 0.04 + 0.06 = 0.10
#       ltfu_prevented     = 1,425.6 × 0.10 = 142.56 -> 143 (round)
# WHY:  Verifies the additive rule across multiple DSD lines (since they're
#       UI-enforced mutually exclusive on the same person, no double-count
#       protection needed).
# ---------------------------------------------------------------------------
test_that("two DSD interventions contribute additively to ltfu_retained_frac", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  ig_new <- intervention_groups
  ig_new$treatment_monitoring$interventions$mmd_3month$efficacy  <- 0.10
  ig_new$treatment_monitoring$interventions$mmd_3month$unit_cost <- 0
  ig_new$treatment_monitoring$interventions$mmd_6month$efficacy  <- 0.20
  ig_new$treatment_monitoring$interventions$mmd_6month$unit_cost <- 0
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring))

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$mmd_3month <- 40
  interv$mmd_6month <- 30

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # Expected: 1,425.6 × 0.10 = 142.56 -> round to 143
  expect_lte(abs(result$ltfu_prevented - 143), 1)
})

# ---------------------------------------------------------------------------
# 5.4 ltfu_retained_frac is capped at 1.0
# ---------------------------------------------------------------------------
# WHAT: If sum of (cov × eff) terms > 1.0, the code caps at 1 (line 1714).
# WHY:  Otherwise ltfu_prevented could exceed ltfu_new_stable, going negative
#       on stable_ltfu and corrupting downstream cascades.
# HOW:  Single DSD at 100% coverage × efficacy 2.0 -> raw frac = 2.0.
#       After cap: 1.0. ltfu_prevented = 1,425.6 × 1.0 = 1,425.6 -> 1426.
# ---------------------------------------------------------------------------
test_that("ltfu_retained_frac is capped at 1.0", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  ig_new <- override_retention("mmd_3month", efficacy = 2.0, unit_cost = 0)
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring))

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$mmd_3month <- 100   # 100% coverage

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # Capped to 1,425.6 -> 1426 (round)
  expect_lte(abs(result$ltfu_prevented - 1426), 1)
})

# ---------------------------------------------------------------------------
# 5.5 ltfu_prevented applies only to stable LTFU; unstable_ltfu unaffected
# ---------------------------------------------------------------------------
# WHAT: Lines 1720-1722: ltfu_prevented = ltfu_new_stable × ltfu_retained_frac.
#       ltfu_prevented_unstable is HARD-CODED to 0 because the current
#       intervention set has no on_art_unstable eligible interventions.
# WHY:  unsuppressed_ltfu output should equal full ltfu_new_unstable, never
#       reduced by DSD coverage.
# HOW:  Same fixture as 5.2. ltfu_new_unstable = 504. With DSD coverage
#       reducing only stable flow, unsuppressed_ltfu (= unstable_ltfu) = 504.
# ---------------------------------------------------------------------------
test_that("DSD interventions do not reduce unstable LTFU", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  ig_new <- override_retention("mmd_3month", efficacy = 0.10, unit_cost = 0)
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring))

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$mmd_3month <- 50

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # ltfu_new_unstable = 3,600 × 0.14 = 504. Unaffected by DSD coverage.
  expect_close(result$unsuppressed_ltfu, 504)
})

# ---------------------------------------------------------------------------
# 5.6 DSD cost applies to ALL stable clients reached (not just retained)
# ---------------------------------------------------------------------------
# WHAT: total_intervention_cost gets number_reached × unit_cost (line 1667-68),
#       where number_reached is the full coverage × eligible (here 50% × 32,400
#       = 16,200), regardless of retained frac.
# WHY:  Captures that you pay for delivering DSD to all enrolled stable
#       clients, not just those who would otherwise have been LTFU.
# HOW:  Override mmd_3month$unit_cost = 12 (arbitrary). Coverage 50% of 32,400.
#         expected cost = 16,200 × 12 = 194,400
#       With NO testing interventions, all other costs = 0.
# ---------------------------------------------------------------------------
test_that("DSD cost = coverage × eligible × unit_cost (full reach)", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  ig_new <- override_retention("mmd_3month", efficacy = 0.10, unit_cost = 12)
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring))

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$mmd_3month <- 50

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # 32,400 × 0.50 = 16,200; × 12 = 194,400
  expect_close(result$total_intervention_cost, 194400)
})

# ---------------------------------------------------------------------------
# 5.7 Tracking is deferred and applied against full LTFU pool
# ---------------------------------------------------------------------------
# WHAT: tracking_tracing is deferred in the loop (lines 1615-1620) and applied
#       AFTER ltfu_new_effective resolves (lines 1749-1752).
#       tracking_reached = (ltfu + ltfu_new_effective) × coverage
#       ltfu_reengaged   += tracking_reached × efficacy
# WHY:  The deferral exists so tracking acts against the right denominator
#       (prevalent stock + net incident after prevention).
# HOW:  No DSD, so ltfu_new_effective = ltfu_new = 1,929.6.
#       total_ltfu_pool = 9,000 + 1,929.6 = 10,929.6
#       Override tracking_tracing: efficacy = 0.50, unit_cost = 0.
#       tracking_tracing coverage = 40 (%).
#         tracking_reached = 10,929.6 × 0.40 = 4,371.84
#         ltfu_reengaged   = 4,371.84 × 0.50 = 2,185.92 (+ 0 spontaneous)
#       Expected: result$ltfu_reengaged ≈ 2,186 (within rounding ±1)
# ---------------------------------------------------------------------------
test_that("tracking_tracing operates on (prevalent + net incident) LTFU pool", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  # Override tracking_tracing params
  ig_new <- intervention_groups
  ig_new$retention_support$interventions$tracking_tracing$efficacy  <- 0.50
  ig_new$retention_support$interventions$tracking_tracing$unit_cost <- 0
  with_intervention_groups(list(retention_support = ig_new$retention_support))

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$tracking_tracing <- 40

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # 10,929.6 × 0.40 × 0.50 = 2,185.92 (+ 0 spontaneous since rate = 0)
  expect_lte(abs(result$ltfu_reengaged - 2186), 1)
})

# ---------------------------------------------------------------------------
# 5.8 Tracking cost applies to the full deferred pool
# ---------------------------------------------------------------------------
# WHAT: total_intervention_cost += tracking_reached × unit_cost (line 1751-52).
#       tracking_reached = total_ltfu_pool × coverage (NOT × efficacy).
# WHY:  You pay to attempt tracking, not to succeed.
# HOW:  Same as 5.7 but unit_cost = 15.
#         tracking_reached = 10,929.6 × 0.40 = 4,371.84
#         expected cost    = 4,371.84 × 15   = 65,577.6 -> 65,578 (round at end)
# ---------------------------------------------------------------------------
test_that("tracking cost = (pool × coverage) × unit_cost (pre-efficacy)", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  ig_new <- intervention_groups
  ig_new$retention_support$interventions$tracking_tracing$efficacy  <- 0.50
  ig_new$retention_support$interventions$tracking_tracing$unit_cost <- 15
  with_intervention_groups(list(retention_support = ig_new$retention_support))

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$tracking_tracing <- 40

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # 10,929.6 × 0.40 × 15 = 65,577.6 -> 65,578 after round
  expect_lte(abs(result$total_intervention_cost - 65578), 2)
})

# ---------------------------------------------------------------------------
# 5.9 Spontaneous re-engagement uses GROSS pool, not residual after testing
# ---------------------------------------------------------------------------
# WHAT: spontaneous_reengaged = total_ltfu_pool × ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE.
#       It's computed BEFORE the testing cap is applied to other flows, against
#       the gross pool. This prevents the perverse outcome where scaling
#       testing DOWN inflates spontaneous re-engagement.
# WHY:  Conceptually spontaneous return is a background epidemiological flow,
#       not a residual.
# HOW:  Override ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE = 0.10 (your live is 0).
#       No DSD, no tracking, no testing.
#         total_ltfu_pool      = 9,000 + 1,929.6 = 10,929.6
#         spontaneous_reengaged = 10,929.6 × 0.10 = 1,092.96
#       Expected: result$ltfu_reengaged ≈ 1,093 (round ±1)
#
#       (NOTE: Your live param is 0, which means in real runs ltfu_reengaged
#        comes only from tracking. This test exercises the formula assuming
#        you'd switch the rate on.)
# ---------------------------------------------------------------------------
test_that("spontaneous re-engagement = gross pool × rate", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  # Override JUST the spontaneous rate (overriding the on.exit-restored value)
  override_ltfu_rates(spontaneous = 0.10)

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()   # no tracking, no testing, no DSD

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # 10,929.6 × 0.10 = 1,092.96
  expect_lte(abs(result$ltfu_reengaged - 1093), 1)
})

# ---------------------------------------------------------------------------
# 5.10 Live spontaneous rate (= 0) produces zero spontaneous flow
# ---------------------------------------------------------------------------
# WHAT: With ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE = 0 (your live value), no
#       spontaneous re-engagement should occur, regardless of pool size.
# WHY:  Verifies that the "off" setting is genuinely off — that the formula
#       doesn't accidentally pull in a default. Also serves as a literal
#       documentation point that this flow is disabled in current config.
# HOW:  Standard LIVE params (rate = 0), no tracking. ltfu_reengaged should
#       be exactly 0.
# ---------------------------------------------------------------------------
test_that("zero spontaneous rate produces zero spontaneous flow", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()  # spontaneous = 0 by default

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()   # no tracking, no testing, no DSD

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  expect_close(result$ltfu_reengaged, 0)
})

# ---------------------------------------------------------------------------
# 5.11 DSD coverage on stable does NOT bleed into unstable LTFU prevention
# ---------------------------------------------------------------------------
# WHAT: When mmd_3month coverage is 100%, ltfu_new_unstable (504) should still
#       fully flow into unsuppressed_ltfu output — DSD only prevents stable
#       LTFU.
# WHY:  Lines 1637-1642: explicit comment that using on_art as the denominator
#       (instead of on_art_stable) would let DSD coverage of stable patients
#       spuriously reduce unstable LTFU. This test pins down that behaviour.
# HOW:  Same setup as 5.4 (efficacy 2.0 to force cap = 1.0).
#       stable_ltfu = 1,425.6 - 1,425.6 = 0; unstable_ltfu = 504.
# ---------------------------------------------------------------------------
test_that("100% DSD coverage on stable does not reduce unstable LTFU", {
  with_hiv_params(LIVE_PARAMS_RETENTION)
  override_ltfu_rates()

  ig_new <- override_retention("mmd_3month", efficacy = 2.0, unit_cost = 0)
  with_intervention_groups(list(treatment_monitoring = ig_new$treatment_monitoring))

  ctx  <- retention_context(test_yield = 0.05, prior_year_tests = NULL,
                            yield_multipliers = list())
  pops <- calculate_populations(ctx)

  interv <- zero_interventions()
  interv$mmd_3month <- 100

  result <- calculate_scenario_outcomes(
    ctx, interv, pops, is_baseline = TRUE, baseline_interventions = interv
  )

  # ltfu_new_unstable = 3,600 × 0.14 = 504, unchanged
  expect_close(result$unsuppressed_ltfu, 504)
  # ltfu_prevented_stable saturates the stable flow
  expect_lte(abs(result$suppressed_ltfu), 1)   # ~0 (within round)
})
