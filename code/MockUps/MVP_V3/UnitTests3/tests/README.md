# HIV Intervention Impact Calculator — Test Suite

This folder contains an automated test suite for `Mock-Up_logic_V2.R`. It pins
down the model's calculations with exact-number assertions wherever possible,
so that future refactors trigger failures when behaviour changes rather than
letting errors propagate silently into country results.

There are 11 thematic test files plus a shared helper module and an
orchestrator. On a clean run the suite produces roughly 215+ passing
assertions across about 110 `test_that()` blocks.

---

## Table of contents

1. [Quick start](#quick-start)
2. [Folder contents](#folder-contents)
3. [Per-file summary](#per-file-summary)
4. [Findings surfaced while building the suite](#findings-surfaced-while-building-the-suite)
5. [Design rationale](#design-rationale)
6. [Adding new tests](#adding-new-tests)
7. [Debugging a failure](#debugging-a-failure)
8. [Known gaps and follow-ups](#known-gaps-and-follow-ups)

---

## Quick start

### Requirements

- R 4.x with the `testthat` package installed
- Internet access at run time (the suite sources `Mock-Up_logic_V2.R`, which
  downloads intervention parameters and country data from SharePoint)
- The two paths at the top of `run_all_tests.R` set to wherever your copy of
  the logic file and the test folder live

### Running

From an R session:

```r
source("run_all_tests.R")
```

Or from the command line:

```bash
Rscript run_all_tests.R
```

The orchestrator uses `testthat::test_dir(".")` with the `summary` reporter,
prints per-file results inline, and ends with a one-line tally:

```
Final summary: PASS 215 | FAIL 0 | WARN 0
```

For CI use, the script exits with status 1 if any test failed (non-interactive
sessions only).

### What the suite does NOT need

- It does not need a Shiny session, the UI, or any country-specific CSV setup
  beyond what's loaded by sourcing the logic file.
- It does not need to know your local R package paths beyond `testthat`.
- It does not modify any source files. Every global override is reverted by
  `on.exit()` at the end of the `test_that()` block.

---

## Folder contents

```
tests/
├── helpers.R                  # Shared setup, fixtures, override helpers
├── run_all_tests.R            # Orchestrator + final tally
├── test_01_populations.R
├── test_02_strata_foi.R
├── test_03_testing.R
├── test_04_prevention.R
├── test_05_retention_ltfu.R
├── test_06_mtct_infant.R
├── test_07_mortality.R
├── test_08_cascade_end.R
├── test_09_costs.R
├── test_10_scenario_diff.R
└── test_11_integration.R
```

### `helpers.R`

The setup module, sourced first by every test file. Provides:

**Fixture builders.** `make_fixture_context(...)` returns a context list with
deliberately round numbers so every downstream calculation can be derived on
paper (population = 1,000,000; prevalence = 5%; cascade 90/80/85; births 25/1000;
etc.). Override any field by name. `make_fixture_interventions(...)` builds a
parallel zeros-by-default intervention list with sensible efficacy defaults.

**Scoped global overrides.** `with_hiv_params(list(...))`, `with_mortality_rates(...)`,
and `with_intervention_groups(...)` each snapshot the relevant global, install
a modified copy via `modifyList()`, and register an `on.exit()` restore on
the calling frame. Tests call these inline; restoration is automatic at end
of `test_that()`.

**Custom expectations.** `expect_close(a, b)` is `expect_equal()` with a
default tolerance of 1e-6 (for float comparisons). `expect_within_pct(a, b,
pct = 1)` checks that `|a - b| / |b| < pct%` — used for the FOI roundtrip
and similar where exact equality isn't realistic.

### `run_all_tests.R`

Hard-codes two local paths (logic file location and tests folder), then calls
`test_dir(".")` and prints a summary. Edit the two `Sys.setenv` /
`setwd` lines if you move the project.

---

## Per-file summary

### `test_01_populations.R` — `calculate_populations()`

Covers the upstream function that builds every downstream cascade and FOI
input. Key assertions:

- **Cascade identity.** `undiagnosed + diagnosed_not_art + on_art = plhiv` to
  the integer (with the standard 90/80/85 fixture: 5,000 + 9,000 + 36,000 = 50,000).
- **Suppression split.** `suppressed = on_art × percent_suppressed/100` and
  `unsuppressed = on_art - suppressed`.
- **Sexually active uses adults.** `sexually_active = total_pop × (1 -
  prop_under14/100) × sexually_active_frac`. The previous bug source
  (using `total_pop` instead) is pinned.
- **FOI strata of HIV-negatives.** 4 strata sum to `hiv_negative` (950,000):
  high-risk = 47,500; general female = 451,250; uncirc male = 315,875;
  circ male = 142,500.
- **ANC multiplier.** `hiv_exposed_births = births × hiv_prevalence ×
  anc_multiplier`. NULL/NA/0/negative all fall back to 1.
- **Pregnant cascade.** Mirrors the adult cascade against `hiv_exposed_births`.
- **`pregnant_hiv_testable`.** Excludes already-diagnosed HIV+ pregnant women
  from the testing denominator.
- **LTFU split by stability.** Uses live `prop_on_art_stable_diff` plus
  `ANNUAL_LTFU_RATE_STABLE/UNSTABLE`.
- **NA fallbacks.** When context fields are NA, `hiv_params$default_*` is used.

### `test_02_strata_foi.R` — Force-of-infection module

The most important file in the suite. Covers:

- **`define_strata_params()`** unit conversions (% → proportion).
- **`partition_into_strata()`** splits `sexually_active_negative` into four
  strata; sum identity verified.
- **Baseline FOI roundtrip (no `baseline_prev_adj`).** β is calibrated so the
  model reproduces `new_infections_per_year`. The roundtrip test verifies the
  output is within 1% of input. **This is the central correctness check for
  the whole simulator.**
- **Baseline FOI roundtrip with `baseline_prev_adj`.** Same property in the
  "biological β" path where prevention is applied symmetrically at calibration
  and FOI eval.
- **PrEP-only protection isolation** (multiplicative residual = 1 - cov × eff).
- **Condom demand-weighting.** Per-person coverage uses sex-acts denominator,
  not headcount: `condom_cov_X = total_condoms × use_rate_X / total_acts`.
- **VMMC denominator.** `vmmc_coverage_frac` uses the *general* uncirc-male
  pool (`n_general_male_uncirc`), not `populations$uncircumcised_males`.
- **Monotonicity.** Scaling PrEP up monotonically reduces infections.
- **Suppression delta.** Positive `suppression_delta` reduces infections
  proportionally.
- **By-stratum sum identity.** The 4 by-stratum infection counts sum to total
  (within ±4 rounding).
- **Zero observed infections** → all β = 0 and 0 new infections.

### `test_03_testing.R` — Testing intervention block

Calls `calculate_scenario_outcomes()` because the testing logic is woven
through it. Covers:

- **`prop_new_dx + prop_reeng = 1`** (indirect identity check).
- **Yield dilution factor = 1.0** below `prior_year_tests` threshold.
- **Half-yield formula above threshold:** `factor = (threshold + (total -
  threshold) × 0.5) / total`. Verified at 20,000 tests vs 10,000 threshold
  → factor 0.75.
- **Index testing exempt from dilution** (`modality_dilution = 1.0`
  regardless of total volume).
- **Index testing capped at 2 × `new_infections_per_year`.**
- **Country `yield_multipliers`** scale the base yield linearly.
- **Cost split.** Unit cost × all tests + linkage cost × linked patients.
- **ANC HIV testing routes into PMTCT cascade** (bypasses general yield path).
- **`testing_reengagement_cap`** binds when retest volume exceeds LTFU pool.
- **`new_diagnoses_cap`** binds at `undiagnosed × cap_prop`.

### `test_04_prevention.R` — Stratum prevention and cost loop

Fills gaps left by test_02 (which covered protection arithmetic but not the
cost loop or PEP allocation in detail):

- **PEP-only allocation in general female.** Per-person coverage = `pep × 0.5
  / n_general_female`.
- **PEP excludes high-risk stratum.** High-risk gets PrEP and condoms only.
- **PrEP + condom multiplicative stacking** in high-risk. Verified that
  protection = 1 - (1 - prep_cov × eff)(1 - condom_cov × eff).
- **VMMC alone leaves `protection_*` = 0.** VMMC moves people between
  strata; it isn't a protection multiplier.
- **PEP allocation sums to 1.5 × supply.** Each of 3 general sub-strata
  gets `pep × 0.5 / n_substratum` allocated. See findings.
- **PEP coverage clips at 1.0** when supply exceeds stratum size.
- **Condom cost uses raw `intervention_value`**, not `number_reached`
  (special-cased in the cost loop because condoms are billed per unit, not
  per person reached).
- **PrEP cost capped at `high_risk_negative`** pool size.
- **VMMC cost capped at `uncircumcised_males`** pool size.
- **Strata partition sum identity** holds at live `sexually_active_frac`.

### `test_05_retention_ltfu.R` — DSD, tracking, spontaneous re-engagement

Covers the LTFU prevention and re-engagement mechanics:

- **Year-start LTFU flow** uses live `ltfu_rate_stable = 0.044` and
  `ltfu_rate_unstable = 0.14`. Anchors all other test 5 derivations.
- **Single DSD additive contribution.** `ltfu_prevented = ltfu_new_stable ×
  coverage_frac × efficacy`.
- **Two DSD interventions sum additively.** Mutually exclusive on the same
  person by UI constraint, so no double-count protection needed.
- **`ltfu_retained_frac` capped at 1.0.** Pathological combinations of
  coverage × efficacy can't push retention above 100%.
- **DSD doesn't reduce unstable LTFU.** `unsuppressed_ltfu` remains at
  `ltfu_new_unstable` regardless of DSD coverage (DSD acts only on stable).
- **DSD cost uses full coverage × eligible × unit cost.** You pay for
  delivering DSD to every enrolled stable client, not just those you'd
  otherwise have lost.
- **Tracking is deferred and applied to `prevalent + net_incident LTFU`.**
  This is the correct denominator after prevention resolves.
- **Tracking cost** = `(pool × coverage) × unit_cost` (pre-efficacy).
- **Spontaneous re-engagement uses gross pool.** Prevents perverse outcome
  where scaling testing *down* would inflate spontaneous flow.
- **Live spontaneous rate = 0** produces zero spontaneous flow (current
  configuration choice — see findings).
- **100% DSD coverage on stable doesn't bleed into unstable.**

### `test_06_mtct_infant.R` — MTCT cascade and infant mortality

Covers the entire vertical-transmission pathway end-to-end:

- **Baseline infant infections** from 3-route MTCT formula: 810 × 0.0033 +
  90 × 0.037 + 350 × 0.2 = 76.003 → 76.
- **ANC VL testing shifts unsuppressed → suppressed.** With 100% × eff=1.0:
  shifts all 90 unsuppressed → suppressed → end_inf = 73.
- **PNC VL applied after ANC VL** (no double-count, capped at remaining).
- **PMTCT linkage.** Newly-diagnosed pregnant women → ART at
  `anc_hiv_testing$linkage_rate`; suppressed at `pct_supp × pmtct_supp_discount`.
- **Infant prophylaxis at 100% × eff=1** reduces infections to 0.
- **Infant prophylaxis applies efficacy ONCE** (single-application formula
  — see findings, "Bugs found and fixed during build").
- **EID diagnosed** = `reached × actual_yield × efficacy`, where
  `actual_yield = end_infant_inf / hiv_exposed_infants`.
- **EID cost split.** Test cost × all reached + linkage cost × infected
  diagnosed only.
- **Baseline infant mortality** = `end_inf × untreated_rate` (all untreated).
- **Full EID coverage** routes infants to ART and dramatically reduces
  deaths via the cascade mortality structure.
- **PMTCT new diagnoses capped at `pregnant_undiagnosed`** (125) regardless
  of testing efficacy.
- **ANC VL shift capped at `pregnant_on_art_unsuppressed`** (90) regardless
  of efficacy.

### `test_07_mortality.R` — 5-group deaths, AHD package, calibration

Covers the adult mortality calculation block:

- **Per-group deaths formula** matches `calc_deaths(n, base, ahd, prop_ahd) =
  n × ((1 - prop_ahd) × base + prop_ahd × ahd)` for each cascade group.
  Derivations pinned with your live MORTALITY_RATES.
- **`total_hiv_deaths_before_interventions`** sums the 5 components.
- **`mortality_calibration_factor` = 1.0** when `USE_MORTALITY_CALIBRATION =
  FALSE` (your live config).
- **Calibration factor scales** when toggle is ON.
- **Calibration anchors adult deaths to target** when ON: with
  `aids_deaths_per_year = 2500` and `birth_rate = 0`, sum of 5 deaths_*
  components = 2500.
- **No-AHD-package baseline** mortality reflects full untouched AHD rates.
- **AHD package halves AHD mortality** when fully covered with 0.5 efficacy.
  Applied to BOTH `eff_ahd_rate_new_init` and `eff_ahd_rate_established`
  (see findings).
- **`deaths_averted`** counts only on-treatment delta (untreated groups
  don't contribute because no current intervention reduces their rates).
- **Partial CD4 coverage gates AHD package proportionally.** CD4 = 0%
  produces zero AHD package effect even at AHD = 100%.

### `test_08_cascade_end.R` — End-of-year cascade reconciliation

Covers the post-mortality cascade derivation:

- **`end_suppressed = remaining_est_supp + remaining_new_supp`.**
- **`end_on_art = remaining_est_treated + remaining_est_supp + remaining_new_init`.**
- **`end_diagnosed = remaining_diagnosed_not_art + end_on_art`.**
- **`end_plhiv` includes new infections** (added to remaining_undiagnosed
  after mortality).
- **Cascade monotonicity:** `end_suppressed ≤ end_on_art ≤ end_diagnosed ≤
  end_plhiv`. Both at baseline and under aggressive interventions.
- **More testing → more new initiations and higher `end_on_art`.**
- **Mortality cannot push groups negative** even under pathological
  calibration (factor ~86).
- **`end_plhiv > end_diagnosed`** at baseline because new infections enter
  the undiagnosed pool.
- **`end_total_infections = end_new_infections + end_infant_infections`.**
- **DSD coverage at 100% raises `end_on_art`** above no-DSD baseline.

### `test_09_costs.R` — Cost branches not covered upstream

The earlier files exercise the main cost lines (testing/condom/PrEP/VMMC/DSD/
tracking/EID), so test_09 fills the remaining branches:

- **EAC cost.** Uses `eac_reach = on_art × vl_cov_frac × unsuppressed_rate ×
  eac_cov_frac` × unit_cost. Layered VL/EAC coverage product.
- **PMTCT linkage cost** at line 1699 (post-cascade): `pmtct_cascade_linked_art
  × anc_hiv_testing$linkage_cost`.
- **CD4 testing cost** = `n_cd4_tested × unit_cost`. Gated by `art_initiations > 0`.
- **AHD package cost** = `n_ahd_pkg_reached × unit_cost`.
- **ANC HIV testing combined cost** = unit cost × all reached + linkage cost
  × PMTCT linked.
- **Multi-intervention cost additivity.** PrEP + condom costs sum independently
  to PrEP cost + condom cost (no state shared between cost lines).
- **`art_provision_cost = end_on_art × 200`** (within rounding — see findings).
- **`total_cost = total_intervention_cost + art_provision_cost`** (within
  rounding).
- **Zero interventions → `total_intervention_cost = 0`.**
- **PNC VL testing cost** = `number_reached × unit_cost`.

### `test_10_scenario_diff.R` — Scenario difference function (unit)

Pure tests on `calculate_scenario_difference()` using synthetic baseline /
scenario lists constructed inline. No simulator calls — these run in
milliseconds:

- **Cascade diffs preserve sign.** Positive when scenario > baseline,
  negative when scenario < baseline.
- **`additional_infections_averted` is positive when scenario reduces
  infections.** Formula: `-(scenario_inf - baseline_inf + scenario_inf_inf
  - baseline_inf_inf)`.
- **`additional_infections_averted` is negative when scenario increases
  infections.** Sign convention works in both directions.
- **`additional_deaths_averted = -(scenario_deaths - baseline_deaths)`.**
- **`scale_up_cost = max(0, scenario_int_cost - baseline_int_cost)`.**
  Display helper — never negative.
- **`scale_down_savings = max(0, baseline_int_cost - scenario_int_cost)`.**
  The mirror.
- **`diff_intervention_cost` keeps raw sign** (not max-floored).
- **Identical inputs → all-zero diffs.**
- **`diff_total_cost = diff_intervention_cost + diff_art_provision_cost`.**
- **Argument order is `(scenario, baseline)`.** Swapping inverts every sign.
- **Each diff field reads from its matching input pair.** Defensive test
  against typo'd field names.

### `test_11_integration.R` — End-to-end integration

Runs the full simulator twice (baseline + scenario) and feeds real result
lists into `calculate_scenario_difference()`. This catches field-name drift
between the two functions — a class of bug test_10 can't see because its
synthetic dicts have whatever keys the test author specifies.

- **All 21 expected diff fields are non-NULL and numeric** when fed real
  simulator output.
- **Self-comparison** (real result vs itself) yields all-zero diffs.
- **Scale-up scenario** (more testing) produces coherent positive signs:
  more diagnoses, more linkages, lower deaths, positive intervention cost,
  positive `scale_up_cost`, zero `scale_down_savings`.
- **Scale-down scenario** inverts those signs and routes the cost gap to
  `scale_down_savings`.
- **Cost identity** `diff_total = diff_intervention + diff_art_provision`
  holds (within rounding) for real outputs.

---

## Findings surfaced while building the suite

Six things came to light while writing the tests. Some are model assumptions
worth being explicit about; some were genuine bugs you'd already fixed before
this build started; one is a display-layer quirk.

### 1. β bounds in `validate_calibration` were too tight (FIXED)

`validate_calibration()` flagged β values as "implausibly high" whenever the
model was run on a generalised SSA epidemic with realistic incidence. The
original bounds were:

```
beta_high          : 0.05 – 3.00
beta_gen_female    : 0.005 – 0.50
beta_gen_male_unc  : 0.003 – 0.40
beta_gen_male_circ : 0.001 – 0.20
```

Working backwards from the calibration formula `β ≈ stratum_incidence /
(n_unsuppressed / total_pop)` and empirical SSA incidence data (FSW median
4.3/100py, peaks to 15+; general female 0.2 – 4.9/100py in high-burden
settings), legitimate high-burden epidemics produce β values that exceed
those bounds. Widened to:

```
beta_high          : 0.05 – 5.00
beta_gen_female    : 0.005 – 1.50
beta_gen_male_unc  : 0.003 – 1.20
beta_gen_male_circ : 0.001 – 0.60
```

Source: Joshi et al. 2023 (FSW); Joshi et al. 2021 (general SSA review,
F:M IRR 1.47); South Africa cohort 2012-2017; Mozambique 2018; Tanzania
Sauti cohort. Derivations are in `Mock-Up_logic_V2.R` lines 1175-1186.

### 2. PEP supply allocation sums to ~150% of supply (DOCUMENTED)

In `compute_prevention_adjustments()` (lines 1046-1068), each of three
general sub-strata (female, male_uncirc, male_circ) receives `pep × 0.5 /
n_substratum` allocated. Total people protected across the three strata
equals `1.5 × pep_supply`. Locked by test 4.5 in current behaviour.

This may be intentional (the 0.5 factor representing a per-act probability
of needing PEP rather than a per-person split), or it may be an unintended
over-allocation. The source has no comment either way. **If you change this
formula in future, test 4.5 will fail and force a deliberate update.**

### 3. AHD package efficacy reduces *established* AHD mortality too (DOCUMENTED)

The AHD package is conceptually designed for newly-initiating patients with
low CD4 counts. But the source code (lines 1996-2004) applies the package's
efficacy reduction to BOTH `eff_ahd_rate_new_init` AND
`eff_ahd_rate_established`. So with full CD4 + AHD package coverage and
0.50 efficacy, established AHD deaths drop by 50% — even though established
patients typically wouldn't be re-CD4-tested as part of the new-initiation
flow that the AHD package targets.

Test 7.8 locks current behaviour. If this is intentional (continued AHD
management on long-term care), no action needed. If it's a mis-attribution,
the fix is to remove the AHD efficacy reduction from `eff_ahd_rate_established`
at line 2003-04 and update test 7.8.

### 4. `art_provision_cost` and `end_on_art` round independently (DISPLAY QUIRK)

In `calculate_scenario_outcomes()`:
- Line 2360: `art_provision_cost <- end_on_art × 200` (unrounded `end_on_art`)
- Line 2555: `end_on_art = round(end_on_art)` in return list
- Line 2575: `art_provision_cost = round(art_provision_cost)` in return list

Both are rounded for display, but from their unrounded internal values. So
in the return list:

```
result$art_provision_cost  ≠  result$end_on_art × 200    (off by up to ±$200)
```

Not a bug, but worth being aware of when comparing the two fields downstream.
If you want them strictly consistent, round `end_on_art` first and then
compute `art_provision_cost = end_on_art_rounded × 200`.

Test 9.7 allows ±$200 tolerance to accommodate this.

### 5. Spontaneous re-engagement is currently disabled (CONFIG CHOICE)

`hiv_params$spontaneous_reengagement_rate = 0` in your live config, even
though the source comment cites empirical literature supporting 10-17%
silent transfer. The code mechanism works (verified by test 5.9 with an
override to 0.10); it's just turned off in production. Test 5.10 locks the
zero behaviour, test 5.9 verifies the formula activates correctly when
the rate is non-zero.

If you turn this on, expect `ltfu_reengaged` to roughly double at baseline
(tracking would still dominate at typical coverage levels).

### 6. Bugs found and fixed during build

Two genuine bugs surfaced and were resolved:

**Infant prophylaxis double-efficacy** (FIXED before tests ran):
Earlier source had efficacy multiplied at line 1551 (when building
`infant_prophy_cov_frac`) AND at line 2301-02 (in the MTCT cascade). Net
effect: `reduction = baseline × eff²` instead of `baseline × eff`. You
fixed this before the test_06 run; test 6.6 now verifies the
single-application behaviour.

**Test group-name bugs in test_05 and test_08**: tests were writing DSD
intervention overrides to a non-existent `treatment` group instead of
the real `treatment_monitoring` group. Tests passed initially through
an `intervention_groups` flatten-and-overwrite mechanism that I haven't
fully diagnosed, but they were structurally wrong. Both files were corrected
to use `treatment_monitoring`.

---

## Design rationale

### Why one test file per theme rather than one mega-file

The model has clear functional boundaries (populations, FOI, testing, prevention,
retention, MTCT, mortality, cascade end, costs, diff). Thematic files mean:

- **Failure isolation.** When one file errors at source-time (e.g. a missing
  helper, a NULL eligibility), the others still run. Mega-file: one early
  `stop()` masks everything downstream.
- **Re-running just one area while debugging.** During this build, I re-ran
  test_07 dozens of times in isolation. With a 2,000-line monolith that
  would have meant sourcing the entire suite each iteration.
- **Diff-readability when changes are made.** Touching mortality logic only
  affects test_07. Reviewers see a clean per-file scope.

### Why fixture injection rather than running against live params

Three options were on the table at the start:

1. **Inject fixtures** (refactor light, deterministic numbers)
2. **Use live params** (no refactor, compute expected at test time from same params)
3. **Hybrid** (extract small pure helpers, integration on the big function)

We picked option 1. Reasons:

- **Reproducibility.** Tests don't depend on the contents of an Excel file
  that may change between runs.
- **Exact-number assertions are possible.** Live-params tests can only verify
  "the calculation matches my recomputation of the same formula" — they catch
  typos and refactor breaks but not formula errors.
- **Documentable derivations.** Every `test_that` block carries a WHAT/WHY/HOW
  header showing the arithmetic. Future readers can audit on paper.

The cost of option 1 is the `with_hiv_params()` / `override_*_globals()`
helpers, which snapshot and restore globals to keep tests hermetic. Each
test file's per-test setup is 5-10 lines of override boilerplate. Acceptable.

### Why no source refactor

`calculate_scenario_outcomes()` is ~1,300 lines and reads many globals. The
cleanest unit-test approach would be to extract its inner blocks (the dilution
factor calculation, the cascade allocation, the calc_deaths helper) into
small named functions that take params as arguments. That would let us write
pure unit tests on each block.

We chose **not** to refactor in this build because:

- The user explicitly asked for tests, not a refactor.
- The big function has worked in production for many country runs; rewriting
  it risks introducing regressions that the tests can't catch yet (catch-22).
- The integration-style tests (test_03 through test_11) still pin down
  behaviour usefully without needing the refactor.

If you later refactor, the suite will help: any test that currently passes
should still pass after the refactor. Differences are the alarm.

### Why exact numbers wherever possible, not just directional checks

The user's original request was for exact-number comparisons. Their words:
"I want to try and compare direct numbers where possible rather than just
the correct direction of movement."

Directional tests (`expect_gt`, `expect_lt`) catch sign-flip bugs but miss
magnitude errors. If a refactor halves all PrEP impact, a directional test
still passes ("PrEP scale-up still reduces infections"), but exact-number
tests fail loudly.

Soft directional tests are used only where the interaction graph is too dense
to derive an exact number cleanly (e.g. test 7.9 deaths_averted, test 8.7
art_initiations vs end_on_art). Each such test carries a docstring noting
that the assertion is intentionally soft.

### Why some tolerance bands are wide

Most return-list values are rounded to integers. When a calculation passes
through multiple `round()` calls (e.g. testing → linkage → cap → ART
initiation → mortality → cascade end), accumulated rounding can drift by a
few units. Tolerance bands are calibrated per test:

- **`tolerance = 1e-6`** for pure-float internal values (β, protection fractions).
- **`±1`** for single-round integer outputs.
- **`±2 to ±5`** for multi-round outputs (death totals, cascade end values).
- **Percentage tolerance** for the FOI roundtrip (1% — accommodates one
  round() at the model level).
- **`±$200`** for the `art_provision_cost ↔ end_on_art` identity (independent
  rounding of two related values).

Where a test fails by an amount close to but exceeding tolerance, that's a
prompt to investigate the rounding chain — not necessarily a bug.

### Why a separate integration file (test_11)

Test_10 uses synthetic baseline/scenario lists. That isolates failures to
`calculate_scenario_difference()` itself but doesn't catch field-name drift
between the two functions: if `calculate_scenario_outcomes()` renames
`end_diagnosed` to `end_diagnoses`, test_10 passes (synthetic dict has the
old key) but the live tool breaks.

Test_11 runs the simulator twice and feeds the real return lists into the
diff function. Field-name drift surfaces as a NULL field in the diff output,
which test 11.1 explicitly checks against a hard-coded list of expected keys.

Trade-off: test_11 is slow (each test runs the simulator twice). Keep that
set small and focused on integration concerns, not model logic. Model
logic belongs in tests 01-09.

---

## Adding new tests

### Within an existing thematic file

Just add a new `test_that("name", { ... })` block. Follow the convention:

```r
# ---------------------------------------------------------------------------
# X.Y Short title
# ---------------------------------------------------------------------------
# WHAT: One-sentence description of what the test verifies.
# WHY:  Why this matters / what regression it catches.
# HOW:  Derivation of expected values, showing arithmetic.
#         expected = ...
# ---------------------------------------------------------------------------
test_that("short_title", {
  with_hiv_params(LIVE_PARAMS_X)
  override_X_globals()

  # ... setup ...

  result <- calculate_scenario_outcomes(...)

  expect_close(result$field, expected_value)
})
```

### New thematic file

Save as `test_XX_<theme>.R` in this folder. `test_dir()` picks it up
automatically by filename pattern. Add a comment to `run_all_tests.R`'s
"CURRENT THEMATIC FILES" list for human readability.

Every file should:

1. `source("helpers.R")` at the top.
2. Define a `LIVE_PARAMS_X` list with the relevant `hiv_params` values you'll
   need (copy from another file, adjust).
3. Define an `override_X_globals()` helper if you need to override
   source-time constants (LTFU rates, MORTALITY_RATES, etc.). Always
   register an `on.exit()` restore.
4. Use the WHAT/WHY/HOW docstring pattern for each test.

### When you change `Mock-Up_logic_V2.R`

If your change is intentional behaviour: update the expected values in any
test that fails. If your change is a refactor that shouldn't alter behaviour:
the tests should all still pass; any failure is the alarm.

When updating expected values, also update the HOW section of the docstring
so future readers can audit the new derivation.

---

## Debugging a failure

`testthat` failure messages tell you:

1. **Which test file and line** the failure happened on.
2. **The actual vs expected values.**
3. **The expression that was evaluated.**

Standard debugging flow:

1. **Re-read the test's WHAT/WHY/HOW docstring.** What's it pinning down?
2. **Compute the expected value by hand** from the fixture inputs shown in HOW.
3. **Compare** to the actual return value. If actual matches your hand
   calculation, the test's expected value is stale — update it. If actual
   doesn't match the formula in HOW, the source has drifted — investigate.
4. **For multi-round drift failures**, check whether the difference is one or
   two `round()` units. If yes, widen the tolerance. If no, the formula has
   changed.
5. **For NULL-field errors**, you're likely overriding the wrong group in
   `intervention_groups`. Check `Mock-Up_logic_V2.R` lines 215-489 for the
   group definitions: `prevention`, `testing`, `treatment_monitoring`,
   `retention_support`, `advanced_disease`. Verify your override targets the
   right group.

### Quick diagnostic snippet

To inspect what's actually being returned mid-test, add this temporarily:

```r
result <- calculate_scenario_outcomes(...)
cat("Field:", result$field_name, "\n")
str(result, max.level = 1)  # top-level field list
```

Remove before committing.

---

## Known gaps and follow-ups

Things the suite does NOT currently cover, in rough order of risk:

1. **Custom Country preset builder.** `build_country_presets()` and the
   data-loading layer (line 2626-2628) are exercised only by sourcing the
   file at suite startup. No tests verify that a malformed country row
   produces sensible defaults rather than NULL propagation.

2. **`validate_calibration` trigger logic.** The β-bound *values* are
   exercised through normal test runs (warnings would surface). But the
   trigger conditions — the incidence-range check, the
   infections-to-unsuppressed ratio check, the frac_high concentration
   check — aren't directly tested. If a future refactor changes when these
   fire, the tests won't catch it.

3. **Shiny UI layer.** Out of scope for unit tests but worth a small set of
   smoke tests (does the app start? does a baseline run produce a non-empty
   chart?) before any major release.

4. **End-to-end multi-year projection.** The tool currently models a single
   year. If you extend to multi-year, the cascade hand-off (end-of-year-N
   becomes start-of-year-N+1) needs its own thematic file.

5. **Numerical stability under extreme inputs.** Tests use deliberate clean
   numbers. Pathological inputs (negative populations, suppression > 100%,
   zero adult population) aren't comprehensively exercised. The model has
   defensive guards in many places, but not all — worth a separate "stress
   test" file if you want full coverage.

6. **Test_05 modifyList behaviour.** During this build, test_05 passed
   initially despite a structural bug in which group it targeted. Root cause
   not fully diagnosed before the fix was applied. If you see similar
   passing-by-accident behaviour in future, lean toward investigating rather
   than declaring victory.

7. **FSW/MSM/AGYW PrEP re-stratification (IN PROGRESS, 2026-07).** The single
   `high_risk` stratum has been split into separate `fsw` and `msm` strata,
   and a new `agyw` (women 15-24) stratum has been carved out of what was
   `general_female`. PrEP is now targeted directly per group (real head
   counts, no more `prep_high_risk_fold` allocation heuristic) — confirmed
   with Alex that FSW/AGYW/MSM fully replace the old single PrEP total, with
   no residual general/untargeted PrEP bucket, for both oral PrEP and
   lenacapavir.
   - **Sourced:** `prop_fsw`/`prop_msm` are derived via `estimate_kp_props()`
     from Stevens et al. 2024 (*Lancet Glob Health* 12:e1400-12) country-level
     `pse_prop`/`prevalence` data. Two known approximations are documented
     in-code: (1) the source's 15-49 denominator is approximated by this
     model's 15+ "adult" population, which overstates `prop_fsw`/`prop_msm`
     by an unquantified amount; (2) the cross-country medians used as a last-
     resort fallback (n=39) are not country-specific.
   - **NOT sourced (placeholder values, clearly flagged in-code):**
     `rr_fsw`/`rr_msm` (need an incidence-based relative-risk estimate, not
     a prevalence ratio — candidate source: Jones et al. 2024, cited in
     Stevens et al.'s own reference list, not yet reviewed), `prop_agyw` and
     `rr_agyw` (need DHS/PHIA age-structure and AGYW-specific incidence data
     — nothing reviewed yet), and per-group PrEP efficacy (temporarily
     reuses the old blended `prep_oral`/`prep_lenacapavir` efficacy for all
     three groups pending trial-specific values — candidates: iPrEx,
     Partners PrEP, FEM-PrEP/VOICE for oral; PURPOSE 1/2 for lenacapavir).
   - **Test impact (rewrites needed, not yet done):** `test_02_strata_foi.R`
     2.1/2.2 (strata structure), 2.5-2.8 (`compute_prevention_adjustments`
     assumes single `prep_oral`), 2.11 (`by_stratum` key names changed from
     `high_risk` to `fsw`/`msm`/`agyw`). `test_04_prevention.R` 4.3 (tests the
     k-fold heuristic being removed — needs replacing, not patching), 4.8/
     4.8b/4.8c (cost caps), 4.10 (partition sum). `test_09_costs.R` PrEP+
     condom additivity tests reference the old `prep_oral` key directly.
     `test_01_populations.R` is unaffected — the separate (and apparently
     unused/dead) `high_risk_negative`/`general_female`/`uncirc_male`/
     `circ_male` fields in `calculate_populations()` were deliberately left
     untouched to avoid unrelated test churn; worth confirming they're
     genuinely unused before a future cleanup.
   - **UI status:** the country-calibration plumbing (`country_calibration()`
     capture and the `context()` reactive) has been updated so the new
     per-country fields actually reach the logic layer. The *input* side has
     not: the old single `baseline_prep_oral`/`scenario1_prep_oral`/etc.
     numeric fields are still in the UI but are now inert — they no longer
     map to any live intervention, so PrEP impact will show as zero in every
     scenario until the 18 new group-specific numeric inputs (3 groups × 2
     products × 3 scenarios) replace them.

8. **Infectious pressure is population-wide, not stratified by source group
   (PRE-EXISTING, not changed by item 7 above).** `estimate_new_infections_foi()`
   computes one pooled `infectious_pressure = n_unsuppressed / total_population`
   and applies it identically to every stratum, scaled only by that stratum's
   own β and protection term. There is no tracking of *which* stratum an
   unsuppressed person belongs to, and no assortative mixing (KP members
   partnering disproportionately within-group). Concretely: (a) HIV-positive
   FSW/MSM/AGYW are not modelled as a distinct sub-population anywhere —
   `calculate_populations()`'s cascade fields (`diagnosed`/`on_art`/
   `suppressed`/`unsuppressed`) are population-wide aggregates with no
   risk-group tag; (b) `suppression_delta` reduces the same shared pool
   proportionally across all strata (see item 2.9 in test_02), so a
   suppression intervention targeted at one group does not preferentially
   protect that group's own partners. This is a proportionate-mixing
   simplification, not a bug, and predates the FSW/MSM/AGYW re-stratification
   (the old `high_risk` stratum had the identical limitation). It matters
   because Stevens et al. 2024 (the source for item 7's `prop_fsw`/`prop_msm`)
   explicitly cites network effects — KP-focused treatment scale-up reducing
   onward transmission to non-KP partners (Stone et al. 2021; Mishra et al.
   2014) — as a reason KP programming has outsized epidemic impact beyond
   simple population proportions. A proportionate-mixing model structurally
   cannot produce that result. Fixing it properly would mean tagging cascade
   state by risk group and introducing sourced assortative-mixing parameters
   — a materially larger change than the current re-stratification, not
   scoped here.
