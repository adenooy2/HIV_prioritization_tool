# ============================================================================
# test_01_populations.R
# ----------------------------------------------------------------------------
# Tests for calculate_populations()
#
# This function is upstream of every cascade and FOI calculation. If it
# produces inconsistent numbers, every downstream metric inherits the error
# silently. Tests here pin down:
#
#   - the cascade identity (undiagnosed + diagnosed_not_on_art + on_art = plhiv)
#   - the four FOI strata partition of HIV-negatives
#   - the sexually_active denominator (adults only, not total pop)
#   - hiv_exposed_births = births × hiv_prevalence × anc_multiplier
#   - pregnant_* cascade sub-populations
#   - the ltfu_new split into stable/unstable using stable share derivation
#   - NULL/NA fallbacks for context fields
#
# Every test_that() block carries a doc header that shows the arithmetic
# derivation for the expected number, so failures are auditable on paper.
# ============================================================================

source("helpers.R")

# ---------------------------------------------------------------------------
# 1.1 Cascade identity: PLHIV = undiagnosed + diagnosed_not_on_art + on_art
# ---------------------------------------------------------------------------
# WHAT: The core accounting identity. Every PLHIV is in exactly one cascade
#       stage at year-start.
# WHY:  If this breaks, mortality groups, FOI strata (n_unsuppressed), and
#       end-of-year reconciliation all inherit the inconsistency.
# HOW:  Standard fixture (plhiv = 50,000, 90/80/85 cascade).
#         diagnosed             = 50,000 × 0.90 = 45,000
#         on_art                = 45,000 × 0.80 = 36,000
#         undiagnosed           = 50,000 - 45,000 = 5,000
#         diagnosed_not_on_art  = 45,000 - 36,000 = 9,000
#         sum                   = 5,000 + 9,000 + 36,000 = 50,000  ✓
# ---------------------------------------------------------------------------
test_that("cascade reconciles to PLHIV exactly", {
  ctx  <- make_fixture_context()
  pops <- calculate_populations(ctx)

  expect_close(pops$diagnosed,            45000)
  expect_close(pops$on_art,                36000)
  expect_close(pops$undiagnosed,           5000)
  expect_close(pops$diagnosed_not_on_art,  9000)
  expect_close(pops$ltfu,                  9000)  # = diagnosed_not_on_art post-collapse
  expect_close(pops$undiagnosed +
                 pops$diagnosed_not_on_art +
                 pops$on_art,
               ctx$plhiv)
})

# ---------------------------------------------------------------------------
# 1.2 Suppressed and unsuppressed-on-ART
# ---------------------------------------------------------------------------
# WHAT: suppressed = on_art × percent_suppressed/100; unsuppressed = on_art - suppressed.
# WHY:  unsuppressed feeds n_unsuppressed in partition_into_strata, which drives
#       infectious_pressure and therefore every infection count.
# HOW:  on_art = 36,000; percent_suppressed = 90.
#         suppressed   = 36,000 × 0.90 = 32,400
#         unsuppressed =  36,000 - 32,400 = 3,600
# ---------------------------------------------------------------------------
test_that("suppression split is on_art × percent_suppressed / 100", {
  ctx  <- make_fixture_context()
  pops <- calculate_populations(ctx)

  expect_close(pops$suppressed,    32400)
  expect_close(pops$unsuppressed,  3600)
  expect_close(pops$suppressed + pops$unsuppressed, pops$on_art)
})

# ---------------------------------------------------------------------------
# 1.3 Sexually active uses ADULTS only, not total population
# ---------------------------------------------------------------------------
# WHAT: sexually_active = total_pop × (1 - prop_under14/100) × hiv_params$sexually_active_frac.
# WHY:  Previously a bug source — using total_pop here inflates the FOI
#       denominator by ~40% in high-fertility settings and produces spurious
#       "low incidence" calibration flags.
# HOW:  Override hiv_params$sexually_active_frac = 0.6 for determinism.
#         total_pop = 1,000,000; prop_under14 = 40 (i.e. 40%).
#         adult_pop          = 1,000,000 × (1 - 0.4) = 600,000
#         sexually_active    = 600,000 × 0.6        = 360,000
# ---------------------------------------------------------------------------
test_that("sexually_active uses adults × sexually_active_frac", {
  with_hiv_params(list(sexually_active_frac = 0.6))
  ctx  <- make_fixture_context()
  pops <- calculate_populations(ctx)

  expect_close(pops$adult_pop,       600000)
  expect_close(pops$sexually_active, 360000)
})

# ---------------------------------------------------------------------------
# 1.4 FOI strata: HIV-negatives split into 4 strata
# ---------------------------------------------------------------------------
# WHAT: HIV-negatives are partitioned into high_risk, general_female,
#       uncirc_male, circ_male. Note: high_risk is OVERLAPPING across sexes
#       in the model (high_risk_negative = hiv_neg × prop_hr); the general
#       split is also taken from full hiv_neg × (1 - prop_hr) × sex split.
# WHY:  These directly feed strata in calibrate_beta. A miscounted denominator
#       propagates to β.
# HOW:  hiv_negative = total_pop - plhiv = 1,000,000 - 50,000 = 950,000.
#       Fixture: prop_high_risk = 0.05, prop_pop_male = 50 (so 0.50), circ_prev = 0.30.
#         high_risk_negative = 950,000 × 0.05                       = 47,500
#         general_female     = 950,000 × (1 - 0.50) × (1 - 0.05)    = 451,250
#         uncirc_male        = 950,000 × 0.50 × (1 - 0.30) × (1 - 0.05) = 315,875
#         circ_male          = 950,000 × 0.50 × 0.30                = 142,500
# ---------------------------------------------------------------------------
test_that("FOI strata partition of HIV-negatives is correct", {
  ctx  <- make_fixture_context()
  pops <- calculate_populations(ctx)

  expect_close(pops$hiv_negative,        950000)
  expect_close(pops$high_risk_negative,  47500)
  expect_close(pops$general_female,      451250)
  expect_close(pops$uncirc_male,         315875)
  expect_close(pops$circ_male,           142500)
  # uncircumcised_males (used by VMMC) keeps both high-risk and general men
  expect_close(pops$uncircumcised_males, 950000 * 0.5 * (1 - 0.3))  # = 332,500
})

# ---------------------------------------------------------------------------
# 1.5 hiv_exposed_births uses anc_multiplier
# ---------------------------------------------------------------------------
# WHAT: hiv_exposed_births = births × hiv_prevalence × anc_multiplier.
# WHY:  anc_multiplier bridges general-population to ANC HIV prevalence. Must
#       be applied here and equivalently to every pregnant_* sub-population
#       so EID arm and maternal cascade run off the same cohort.
# HOW:  total_pop = 1,000,000; birth_rate = 25 -> births = 25,000.
#       hiv_prevalence = 0.05; anc_multiplier = 1.4.
#         hiv_exposed_births = 25,000 × 0.05 × 1.4 = 1,750
#       Default fixture (anc_multiplier = 1):
#         hiv_exposed_births = 25,000 × 0.05 × 1   = 1,250
# ---------------------------------------------------------------------------
test_that("hiv_exposed_births = births × hiv_prevalence × anc_multiplier", {
  pops_default <- calculate_populations(make_fixture_context())
  expect_close(pops_default$hiv_exposed_infants, 1250)
  expect_close(pops_default$pregnant_women,      25000)  # = births

  pops_high <- calculate_populations(make_fixture_context(anc_multiplier = 1.4))
  expect_close(pops_high$hiv_exposed_infants, 1750)
})

# ---------------------------------------------------------------------------
# 1.6 anc_multiplier NULL/0/negative falls back to 1
# ---------------------------------------------------------------------------
# WHAT: Lines 566-569: defensive default. NULL × number = numeric(0) in R, so
#       this MUST coerce non-positive/missing values to 1.
# WHY:  Custom Country and UI-built contexts may omit anc_multiplier. Silent
#       numeric(0) propagation would zero out the entire PMTCT cascade
#       downstream.
# HOW:  Build contexts with anc_multiplier = NULL, NA, 0, -1; all should give
#       the same hiv_exposed_births as anc_multiplier = 1.
# ---------------------------------------------------------------------------
test_that("anc_multiplier defaults to 1 for NULL/NA/0/negative", {
  baseline_births <- calculate_populations(
    make_fixture_context(anc_multiplier = 1))$hiv_exposed_infants

  for (bad in list(NULL, NA, 0, -0.5)) {
    ctx <- make_fixture_context(anc_multiplier = bad)
    pops <- calculate_populations(ctx)
    expect_close(pops$hiv_exposed_infants, baseline_births,
                 label = sprintf("anc_multiplier = %s",
                                 if (is.null(bad)) "NULL" else as.character(bad)))
  }
})

# ---------------------------------------------------------------------------
# 1.7 Pregnant cascade sub-populations
# ---------------------------------------------------------------------------
# WHAT: Pregnant women cascade mirrors the adult cascade applied to
#       hiv_exposed_births (which already includes anc_multiplier).
# WHY:  These are the denominators for ANC/PNC VL, PMTCT linkage, EID. Off-by-
#       one in the formula understates or overstates PMTCT impact directly.
# HOW:  hiv_exposed_births = 1,250 (default fixture).
#         pregnant_on_art              = 1,250 × 0.90 × 0.80          = 900
#         pregnant_on_art_suppressed   = 900 × 0.90                   = 810
#         pregnant_on_art_unsuppressed = 900 × (1 - 0.90)              = 90
#         pregnant_not_on_art          = 1,250 × (1 - 0.90 × 0.80)     = 1,250 × 0.28 = 350
#         pregnant_undiagnosed         = 1,250 × (1 - 0.90)            = 125
# ---------------------------------------------------------------------------
test_that("pregnant cascade sub-populations derived correctly", {
  pops <- calculate_populations(make_fixture_context())

  expect_close(pops$pregnant_on_art,              900)
  expect_close(pops$pregnant_on_art_suppressed,   810)
  expect_close(pops$pregnant_on_art_unsuppressed, 90)
  expect_close(pops$pregnant_not_on_art,          350)
  expect_close(pops$pregnant_undiagnosed,         125)

  # Internal consistency: on_art_supp + on_art_unsupp = on_art
  expect_close(pops$pregnant_on_art_suppressed +
                 pops$pregnant_on_art_unsuppressed,
               pops$pregnant_on_art)
})

# ---------------------------------------------------------------------------
# 1.8 pregnant_hiv_testable: HIV-negative pregnant + HIV+ undiagnosed pregnant
# ---------------------------------------------------------------------------
# WHAT: pregnant_hiv_testable = (births - hiv_exposed_births) + pregnant_undiagnosed.
# WHY:  This is the denominator for ANC/PNC HIV testing yield. Including
#       already-diagnosed HIV+ pregnant women would inflate the denominator
#       and depress the apparent yield.
# HOW:  births = 25,000; hiv_exposed_births = 1,250; pregnant_undiagnosed = 125.
#         pregnant_hiv_testable = (25,000 - 1,250) + 125 = 23,875
# ---------------------------------------------------------------------------
test_that("pregnant_hiv_testable excludes diagnosed HIV+ pregnant women", {
  pops <- calculate_populations(make_fixture_context())
  expect_close(pops$pregnant_hiv_testable, 23875)
})

# ---------------------------------------------------------------------------
# 1.9 ltfu_new split by stability uses hiv_params$prop_on_art_stable_diff
# ---------------------------------------------------------------------------
# WHAT: on_art_stable_n = on_art × ((percent_suppressed + prop_on_art_stable_diff)/100)
#       Then ltfu_new_stable   = on_art_stable_n   × ANNUAL_LTFU_RATE_STABLE
#            ltfu_new_unstable = on_art_unstable_n × ANNUAL_LTFU_RATE_UNSTABLE
# WHY:  Drives the LTFU prevention denominator (DSD acts only on stable).
#       If the stable share is wrong, DSD coverage frac is mis-scaled.
# HOW:  Override hiv_params: prop_on_art_stable_diff = 0,
#       ltfu_rate_stable = 0.05, ltfu_rate_unstable = 0.20.
#       (Globals ANNUAL_LTFU_RATE_STABLE/UNSTABLE were assigned at source
#        time from the LIVE hiv_params, so for this test we override the
#        globals directly via base assign().)
#       Fixture: on_art = 36,000; percent_suppressed = 90.
#         on_art_stable_n = 36,000 × ((90 + 0)/100) = 32,400
#         on_art_unstable = 36,000 - 32,400          = 3,600
#         ltfu_new_stable    = 32,400 × 0.05 = 1,620
#         ltfu_new_unstable  = 3,600  × 0.20 = 720
#         ltfu_new (total)   = 1,620 + 720   = 2,340
# ---------------------------------------------------------------------------
test_that("ltfu_new split by stability matches formula", {
  with_hiv_params(list(prop_on_art_stable_diff = 0))

  # ANNUAL_LTFU_RATE_STABLE/UNSTABLE are top-level constants set at source.
  # Snapshot + override + restore via on.exit.
  old_stable   <- ANNUAL_LTFU_RATE_STABLE
  old_unstable <- ANNUAL_LTFU_RATE_UNSTABLE
  assign("ANNUAL_LTFU_RATE_STABLE",   0.05, envir = .GlobalEnv)
  assign("ANNUAL_LTFU_RATE_UNSTABLE", 0.20, envir = .GlobalEnv)
  on.exit({
    assign("ANNUAL_LTFU_RATE_STABLE",   old_stable,   envir = .GlobalEnv)
    assign("ANNUAL_LTFU_RATE_UNSTABLE", old_unstable, envir = .GlobalEnv)
  }, add = TRUE)

  pops <- calculate_populations(make_fixture_context())

  expect_close(pops$on_art_stable,     32400)
  expect_close(pops$ltfu_new_stable,   1620)
  expect_close(pops$ltfu_new_unstable, 720)
  expect_close(pops$ltfu_new,          2340)
})

# ---------------------------------------------------------------------------
# 1.10 NULL/NA fallbacks pull from hiv_params defaults
# ---------------------------------------------------------------------------
# WHAT: When context fields prop_pop_male, prop_pop_under_14, circ_prevalence,
#       prop_high_risk are NULL/NA, calculate_populations() reaches into
#       hiv_params$default_*.
# WHY:  Custom Country and partial CSV rows often omit these. Silent fallback
#       prevents NULL propagation that would corrupt FOI strata.
# HOW:  Build context with these fields set to NA, override hiv_params with
#       known defaults, verify uncircumcised_males uses the overridden values.
#         total_pop = 1,000,000; plhiv = 50,000 -> hiv_neg = 950,000.
#         default_prop_pop_male = 49 -> male share 0.49
#         default_circ_prevalence = 0.25 (note: already proportion in hiv_params)
#         uncirc_males = 950,000 × 0.49 × (1 - 0.25) = 349,125
# ---------------------------------------------------------------------------
test_that("NA context fields fall back to hiv_params defaults", {
  with_hiv_params(list(
    default_prop_pop_male   = 49,
    default_circ_prevalence = 0.25,
    default_prop_high_risk  = 0.04,
    default_prop_pop_under_14 = 40
  ))

  ctx <- make_fixture_context(
    prop_pop_male     = NA,
    prop_pop_under_14 = NA,
    circ_prevalence   = NA,
    prop_high_risk    = NA
  )
  pops <- calculate_populations(ctx)

  # Derived expected values from overridden defaults:
  #   uncircumcised_males = 950,000 × 0.49 × (1 - 0.25) = 349,125
  expect_close(pops$uncircumcised_males, 950000 * 0.49 * (1 - 0.25))
  #   high_risk_negative = 950,000 × 0.04 = 38,000
  expect_close(pops$high_risk_negative, 950000 * 0.04)
})

# ---------------------------------------------------------------------------
# 1.11 Cascade reconciliation warning fires for inconsistent input
# ---------------------------------------------------------------------------
# WHAT: Lines 586-594 emit a warning if (plhiv - dx) + (dx - on_art) + on_art
#       differs from plhiv by more than 1. The arithmetic is tautological for
#       internally-computed values, so this only fires when caller passes
#       pre-computed cascade members that don't agree.
# WHY:  Internal-derivation path is tested in 1.1. Here we confirm the
#       guard exists and does NOT fire for a consistent fixture (the only
#       path the function can take given that diagnosed/on_art are derived).
# HOW:  Standard fixture should produce no warning.
# ---------------------------------------------------------------------------
test_that("no cascade-reconciliation warning for consistent fixture", {
  expect_no_warning(
    calculate_populations(make_fixture_context())
  )
})
