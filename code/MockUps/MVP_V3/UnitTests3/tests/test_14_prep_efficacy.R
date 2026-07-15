# ============================================================================
# test_14_prep_efficacy.R
# ----------------------------------------------------------------------------
# MIGRATION TEST — TEMPORARY. Self-retires at step 4.
#
# Purpose: prove derive_prep_efficacy() reproduces the hand-calculated numbers
# currently sitting in the intervention_params 'efficacy' cells, before those
# cells are deleted.
#
#   Oral : eff_adherent x person_years_on_prep
#            FSW/AGYW/General  0.74 x 0.20 = 0.148
#            MSM               0.74 x 0.35 = 0.259
#   LEN  : eff_adherent x shot_coverage_years x (1 + second_shot_return_rate)
#            all groups        1.00 x 0.5 x (1 + 0.50) = 0.75
#
# DELIBERATE EXCEPTION to the README's "fixture injection, not live params"
# rule (README ~line 503). This is a DATA test, not a logic test: its whole
# job is to check the live SharePoint sheet. It is expected to fail if someone
# edits the sheet without updating the other side. That is the feature.
# Unit tests for the derivation itself belong in the 14.2 block below and use
# no live data.
#
# Once the eight flat 'efficacy' rows are deleted (step 4), the 14.1 block
# skips itself and this file can be deleted along with them.
# ============================================================================

source("helpers.R")

PREP_KEYS <- c("prep_oral_fsw", "prep_oral_msm", "prep_oral_agyw",
               "prep_oral_general",
               "prep_lenacapavir_fsw", "prep_lenacapavir_msm",
               "prep_lenacapavir_agyw", "prep_lenacapavir_general")

# Safe single-cell accessor. Returns NA_real_ if the column doesn't exist at
# all (rows not yet added), if it's not coercible, or if the subset returned
# anything other than exactly one value (i.e. the spread() fan-out).
cell <- function(row, col) {
  if (!col %in% names(row)) return(NA_real_)
  v <- suppressWarnings(as.numeric(row[[col]]))
  if (length(v) != 1) return(NA_real_)
  v
}

# ---------------------------------------------------------------------------
# 14.1 Derived efficacy reproduces the legacy hand-calculated cell
# ---------------------------------------------------------------------------
for (.key in PREP_KEYS) {
  local({
    key <- .key
    test_that(sprintf("14.1 %s: derivation reproduces the legacy efficacy cell", key), {
      
      row <- subset(intervention_params, intervention_key == key)
      
      # Guard: the spread() fan-out. If category/intervention spelling drifted
      # across the rows for this key, spread() emits >1 half-NA row and every
      # subset(...)$col downstream returns a vector. Fail loudly, don't skip.
      expect_equal(nrow(row), 1,
                   label = sprintf("nrow(intervention_params[key=='%s']) — >1 means
                                    inconsistent category/intervention spelling
                                    across this key's rows", key))
      
      sheet_eff <- cell(row, "efficacy")
      
      # Post-step-4: flat efficacy row deleted. Nothing left to compare against;
      # this test has served its purpose.
      if (is.na(sheet_eff)) {
        skip(sprintf("%s: no flat 'efficacy' row — step 4 complete, test retired", key))
      }
      
      eff_adh    <- cell(row, "eff_adherent")
      py_oral    <- cell(row, "person_years_on_prep")
      ret_rate   <- cell(row, "second_shot_return_rate")
      shot_years <- cell(row, "shot_coverage_years")
      
      # ANTI-VACUOUS GUARD. Without this the test compares prep_eff()'s
      # fallback to the very cell it fell back to, and passes while proving
      # nothing. A missing/mistyped parameter_type MUST fail here.
      expect_false(is.na(eff_adh),
                   label = sprintf("%s: eff_adherent row missing from sheet
                                    (mistyped parameter_type?)", key))
      expect_true(!is.na(py_oral) || !is.na(ret_rate),
                  label = sprintf("%s: needs person_years_on_prep (oral) or
                                   second_shot_return_rate (LEN)", key))
      
      # Recompute by hand from the sheet — deliberately NOT via
      # derive_prep_efficacy(), so a bug in that function can't hide itself.
      expected <- if (!is.na(ret_rate)) {
        eff_adh * (if (is.na(shot_years)) 0.5 else shot_years) * (1 + ret_rate)
      } else {
        eff_adh * py_oral
      }
      
      # (a) the legacy cell matches the sub-assumptions it was calculated from.
      #     Fails if the cell was rounded (0.15 vs 0.148) or has drifted.
      expect_close(expected, sheet_eff, tolerance = 1e-6,
                   label = sprintf("%s: hand-calc from sub-assumptions (%.6f) vs
                                    legacy efficacy cell (%.6f)",
                                   key, expected, sheet_eff))
      
      # (b) build_intervention_groups() actually wired prep_eff() in.
      derived <- intervention_groups$prevention$interventions[[key]]$efficacy
      expect_close(derived, expected, tolerance = 1e-6,
                   label = sprintf("%s: intervention_groups efficacy (%.6f) vs
                                    hand-calc (%.6f) — wiring bug in prep_eff()",
                                   key, derived, expected))
    })
  })
}

# ---------------------------------------------------------------------------
# 14.2 derive_prep_efficacy() unit behaviour — no live data
# ---------------------------------------------------------------------------
test_that("14.2 derive_prep_efficacy: oral route", {
  expect_close(derive_prep_efficacy(0.74, person_years = 0.20), 0.148)
  expect_close(derive_prep_efficacy(0.74, person_years = 0.35), 0.259)
  expect_equal(derive_prep_efficacy(0.74, person_years = 0), 0)
})

test_that("14.2 derive_prep_efficacy: LEN route", {
  # 1.00 x 0.5 x (1 + 0.50) = 0.75
  expect_close(derive_prep_efficacy(1.00, return_rate = 0.50), 0.75)
  # return_rate = 1 -> both shots land -> py = 0.5 x 2 = 1.0 -> eff_adherent
  expect_close(derive_prep_efficacy(0.90, return_rate = 1.00), 0.90)
  # return_rate = 0 -> shot 1 only -> py = 0.5
  expect_close(derive_prep_efficacy(1.00, return_rate = 0.00), 0.50)
})

test_that("14.2 derive_prep_efficacy: guards", {
  # return_rate wins when both routes' inputs are present (documented order)
  expect_close(derive_prep_efficacy(1.00, person_years = 0.20, return_rate = 0.50), 0.75)
  # dimensional slip: months entered instead of a fraction
  expect_error(derive_prep_efficacy(0.74, person_years = 2.4), "outside \\[0,1\\]")
  expect_error(derive_prep_efficacy(1.2,  person_years = 0.20), "outside \\[0,1\\]")
  # missing inputs + fallback -> warn and fall back
  expect_warning(v <- derive_prep_efficacy(fallback = 0.148, key = "x"), "falling back")
  expect_close(v, 0.148)
  # missing inputs + no fallback -> hard error
  expect_error(derive_prep_efficacy(key = "x"), "neither derivation inputs")
})