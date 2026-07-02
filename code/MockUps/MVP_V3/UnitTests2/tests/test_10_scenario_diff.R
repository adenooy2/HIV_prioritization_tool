# ============================================================================
# test_10_scenario_diff.R
# ----------------------------------------------------------------------------
# Tests for calculate_scenario_difference():
#
#   A pure transformation function: takes two result lists (scenario and
#   baseline) and returns a list of differences. No external state, no
#   complex logic — just subtraction with two sign-flips for "averted"
#   quantities and two max(0, ...) display helpers.
#
# Tests here construct synthetic baseline / scenario lists directly rather
# than running calculate_scenario_outcomes() twice. This makes the assertions
# independent of all the upstream calibration / cascade machinery — failures
# in test_10 indicate problems with the diff function itself, not the model.
#
#   - All `diff_*` fields = scenario - baseline (sign and magnitude)
#   - additional_infections_averted = -(scenario_inf + scenario_inf_inf -
#                                        baseline_inf - baseline_inf_inf)
#   - additional_deaths_averted     = -(scenario_deaths - baseline_deaths)
#   - scale_up_cost      = max(0, scenario_int_cost - baseline_int_cost)
#   - scale_down_savings = max(0, baseline_int_cost - scenario_int_cost)
# ============================================================================

source("helpers.R")

# Builder: synthetic result list with all fields the diff function reads.
# Easy to copy and modify per test.
fake_result <- function(
  end_diagnosed         = 45000,
  end_on_art            = 36000,
  end_suppressed        = 32400,
  tests_performed       = 100000,
  positive_tests        = 5000,
  new_diagnoses         = 1500,
  art_initiations       = 1200,
  ltfu_new_effective    = 1900,
  ltfu_prevented        = 100,
  ltfu_reengaged        = 800,
  end_new_infections    = 5000,
  end_infant_infections = 76,
  end_total_infections  = 5076,
  end_deaths            = 2500,
  total_intervention_cost = 10000,
  art_provision_cost      = 7200000,
  total_cost              = 7210000
) {
  list(
    end_diagnosed = end_diagnosed,
    end_on_art = end_on_art,
    end_suppressed = end_suppressed,
    tests_performed = tests_performed,
    positive_tests = positive_tests,
    new_diagnoses = new_diagnoses,
    art_initiations = art_initiations,
    ltfu_new_effective = ltfu_new_effective,
    ltfu_prevented = ltfu_prevented,
    ltfu_reengaged = ltfu_reengaged,
    end_new_infections = end_new_infections,
    end_infant_infections = end_infant_infections,
    end_total_infections = end_total_infections,
    end_deaths = end_deaths,
    total_intervention_cost = total_intervention_cost,
    art_provision_cost = art_provision_cost,
    total_cost = total_cost
  )
}

# ---------------------------------------------------------------------------
# 10.1 Cascade diffs: scenario - baseline (sign preserved when scenario > baseline)
# ---------------------------------------------------------------------------
# WHAT: diff_diagnosed = scenario$end_diagnosed - baseline$end_diagnosed.
#       Positive value means scenario has MORE diagnosed than baseline.
# WHY: Sign convention matters for UI display. Scenario better than baseline
#      on diagnosed → positive diff.
# HOW: scenario end_diagnosed = 46,000; baseline = 45,000. Expected diff = +1,000.
# ---------------------------------------------------------------------------
test_that("diff_diagnosed = scenario - baseline with correct sign", {
  baseline <- fake_result(end_diagnosed = 45000)
  scenario <- fake_result(end_diagnosed = 46000)
  d <- calculate_scenario_difference(scenario, baseline)
  expect_close(d$diff_diagnosed, 1000)
})

# ---------------------------------------------------------------------------
# 10.2 Cascade diffs: negative when scenario < baseline
# ---------------------------------------------------------------------------
# WHAT: diff_on_art negative when scenario reduces ART coverage.
# WHY: Scale-down scenarios produce negative diffs; sign-flip bug here would
#      flip every cost-effectiveness ratio.
# HOW: scenario end_on_art = 35,000; baseline = 36,000. Expected diff = -1,000.
# ---------------------------------------------------------------------------
test_that("diff_on_art is negative when scenario < baseline", {
  baseline <- fake_result(end_on_art = 36000)
  scenario <- fake_result(end_on_art = 35000)
  d <- calculate_scenario_difference(scenario, baseline)
  expect_close(d$diff_on_art, -1000)
})

# ---------------------------------------------------------------------------
# 10.3 additional_infections_averted: sign flipped from (scenario - baseline)
# ---------------------------------------------------------------------------
# WHAT: From line 2608-09:
#       additional_infections_averted = -((scenario_inf - baseline_inf) +
#                                          (scenario_inf_inf - baseline_inf_inf))
#       = baseline_inf + baseline_inf_inf - scenario_inf - scenario_inf_inf
#       i.e. positive when scenario has FEWER infections than baseline.
# WHY: This is THE most important sign to get right. The whole tool is built
#      around showing "how many additional infections did this scenario avert".
#      A flip here turns avert into cause-of.
# HOW: baseline adult = 5,000, infant = 76. Scenario adult = 4,000, infant = 50.
#      additional_inf_averted = -((4000 - 5000) + (50 - 76))
#                             = -(-1000 + -26)
#                             = -(-1026)
#                             = 1026  (positive = scenario AVERTED 1,026 infections)
# ---------------------------------------------------------------------------
test_that("additional_infections_averted is positive when scenario reduces infections", {
  baseline <- fake_result(end_new_infections = 5000, end_infant_infections = 76)
  scenario <- fake_result(end_new_infections = 4000, end_infant_infections = 50)
  d <- calculate_scenario_difference(scenario, baseline)
  expect_close(d$additional_infections_averted, 1026)
})

# ---------------------------------------------------------------------------
# 10.4 additional_infections_averted: negative when scenario INCREASES infections
# ---------------------------------------------------------------------------
# WHAT: If scenario causes more infections (e.g. budget cuts), this value
#       goes negative.
# WHY: Confirms the sign convention works in both directions.
# HOW: scenario adult = 6,000 (worse than baseline 5,000). Infant same (76).
#      additional_inf_averted = -((6000 - 5000) + 0) = -1000
# ---------------------------------------------------------------------------
test_that("additional_infections_averted is negative when scenario adds infections", {
  baseline <- fake_result(end_new_infections = 5000, end_infant_infections = 76)
  scenario <- fake_result(end_new_infections = 6000, end_infant_infections = 76)
  d <- calculate_scenario_difference(scenario, baseline)
  expect_close(d$additional_infections_averted, -1000)
})

# ---------------------------------------------------------------------------
# 10.5 additional_deaths_averted: sign flipped from (scenario - baseline)
# ---------------------------------------------------------------------------
# WHAT: From line 2610: additional_deaths_averted = -(scenario_deaths - baseline_deaths)
#                                                 = baseline_deaths - scenario_deaths.
#       Positive when scenario reduces deaths.
# HOW: scenario deaths = 2,200; baseline = 2,500. Expected = -(-300) = 300.
# ---------------------------------------------------------------------------
test_that("additional_deaths_averted is positive when scenario reduces deaths", {
  baseline <- fake_result(end_deaths = 2500)
  scenario <- fake_result(end_deaths = 2200)
  d <- calculate_scenario_difference(scenario, baseline)
  expect_close(d$additional_deaths_averted, 300)
})

# ---------------------------------------------------------------------------
# 10.6 scale_up_cost = max(0, scenario - baseline) (intervention costs only)
# ---------------------------------------------------------------------------
# WHAT: scale_up_cost > 0 means scenario spends MORE than baseline. Used for
#       UI display of "how much more we'd spend".
# WHY: max(0, ...) means scale_up_cost can never be negative. If scenario
#      spends LESS, scale_up_cost = 0 and scale_down_savings carries the value.
# HOW: scenario int_cost = 15,000; baseline = 10,000.
#      scale_up_cost = max(0, 15000 - 10000) = 5,000
#      scale_down_savings = max(0, 10000 - 15000) = 0
# ---------------------------------------------------------------------------
test_that("scale_up_cost shows positive scenario-baseline gap; scale_down_savings = 0", {
  baseline <- fake_result(total_intervention_cost = 10000)
  scenario <- fake_result(total_intervention_cost = 15000)
  d <- calculate_scenario_difference(scenario, baseline)
  expect_close(d$scale_up_cost,      5000)
  expect_close(d$scale_down_savings, 0)
})

# ---------------------------------------------------------------------------
# 10.7 scale_down_savings = max(0, baseline - scenario) (intervention costs only)
# ---------------------------------------------------------------------------
# WHAT: Mirror of 10.6. When scenario spends LESS than baseline, savings
#       carries the difference and scale_up_cost = 0.
# HOW: scenario int_cost = 8,000; baseline = 10,000.
#      scale_up_cost      = max(0, 8000 - 10000) = 0
#      scale_down_savings = max(0, 10000 - 8000) = 2,000
# ---------------------------------------------------------------------------
test_that("scale_down_savings shows positive baseline-scenario gap; scale_up_cost = 0", {
  baseline <- fake_result(total_intervention_cost = 10000)
  scenario <- fake_result(total_intervention_cost = 8000)
  d <- calculate_scenario_difference(scenario, baseline)
  expect_close(d$scale_up_cost,      0)
  expect_close(d$scale_down_savings, 2000)
})

# ---------------------------------------------------------------------------
# 10.8 diff_intervention_cost preserves sign (not max-floored)
# ---------------------------------------------------------------------------
# WHAT: diff_intervention_cost = scenario - baseline. Unlike scale_up/down,
#       this carries the raw sign.
# WHY: scale_up/down are display helpers; the underlying diff must keep the
#      sign so downstream calculations (e.g. cost per infection averted) work.
# HOW: Same as 10.7. Expected diff_intervention_cost = 8000 - 10000 = -2,000.
# ---------------------------------------------------------------------------
test_that("diff_intervention_cost keeps sign (negative when scenario < baseline)", {
  baseline <- fake_result(total_intervention_cost = 10000)
  scenario <- fake_result(total_intervention_cost = 8000)
  d <- calculate_scenario_difference(scenario, baseline)
  expect_close(d$diff_intervention_cost, -2000)
})

# ---------------------------------------------------------------------------
# 10.9 Identical baseline and scenario produce all zeros
# ---------------------------------------------------------------------------
# WHAT: If scenario == baseline (no change), every diff field should be 0
#       and both scale_up/down should be 0.
# WHY: Edge case — pre-scenario comparison ("what if I do nothing different")
#      must produce a clean zero state, not noise.
# HOW: Use the same fake_result twice.
# ---------------------------------------------------------------------------
test_that("identical scenario and baseline yield all-zero differences", {
  same <- fake_result()
  d <- calculate_scenario_difference(same, same)

  expect_close(d$diff_diagnosed,   0)
  expect_close(d$diff_on_art,      0)
  expect_close(d$diff_suppressed,  0)
  expect_close(d$diff_new_infections,    0)
  expect_close(d$diff_infant_infections, 0)
  expect_close(d$diff_total_infections,  0)
  expect_close(d$diff_deaths,            0)
  expect_close(d$additional_infections_averted, 0)
  expect_close(d$additional_deaths_averted,     0)
  expect_close(d$diff_intervention_cost,  0)
  expect_close(d$diff_art_provision_cost, 0)
  expect_close(d$diff_total_cost,         0)
  expect_close(d$scale_up_cost,           0)
  expect_close(d$scale_down_savings,      0)
})

# ---------------------------------------------------------------------------
# 10.10 diff_total_cost reflects both intervention and ART provision deltas
# ---------------------------------------------------------------------------
# WHAT: diff_total_cost = scenario_total - baseline_total. Should equal
#       diff_intervention_cost + diff_art_provision_cost (since total = sum
#       in the source result list).
# WHY: Cross-check that the two sub-components add to the whole. If they
#      don't, either total was computed differently in the result list or
#      the diff function is missing a term.
# HOW: scenario: int_cost = 12,000, art = 7,300,000, total = 7,312,000.
#      baseline: int_cost = 10,000, art = 7,200,000, total = 7,210,000.
#      diff_int_cost = 2,000; diff_art = 100,000; diff_total = 102,000.
# ---------------------------------------------------------------------------
test_that("diff_total_cost = diff_intervention_cost + diff_art_provision_cost", {
  baseline <- fake_result(total_intervention_cost = 10000,
                          art_provision_cost      = 7200000,
                          total_cost              = 7210000)
  scenario <- fake_result(total_intervention_cost = 12000,
                          art_provision_cost      = 7300000,
                          total_cost              = 7312000)
  d <- calculate_scenario_difference(scenario, baseline)

  expect_close(d$diff_total_cost, 102000)
  expect_close(d$diff_total_cost,
               d$diff_intervention_cost + d$diff_art_provision_cost)
})

# ---------------------------------------------------------------------------
# 10.11 Argument order: calculate_scenario_difference(scenario, baseline)
# ---------------------------------------------------------------------------
# WHAT: Swapping arguments inverts every sign.
# WHY: This is mostly a documentation test — the function signature is
#      (scenario, baseline), not (baseline, scenario). Get it wrong in caller
#      code and every value flips. Lock the convention here.
# HOW: Build a scenario with more diagnosed than baseline. Pass (scenario,
#      baseline) → diff_diagnosed > 0. Pass (baseline, scenario) → diff < 0.
# ---------------------------------------------------------------------------
test_that("argument order is (scenario, baseline); swapping inverts signs", {
  baseline <- fake_result(end_diagnosed = 45000)
  scenario <- fake_result(end_diagnosed = 46000)

  correct  <- calculate_scenario_difference(scenario, baseline)
  swapped  <- calculate_scenario_difference(baseline, scenario)

  expect_close(correct$diff_diagnosed, 1000)
  expect_close(swapped$diff_diagnosed, -1000)
})

# ---------------------------------------------------------------------------
# 10.12 All 4 cascade-related diff fields propagate from inputs
# ---------------------------------------------------------------------------
# WHAT: Sanity that the diff function reads from EACH scenario field and
#       doesn't silently fall back to baseline if a field is missing.
# WHY: If a wrong key were used (e.g. typo "end_diagnosed_" with trailing
#      underscore), the field would be NULL and the diff would be NA or 0
#      depending on R coercion. Bury such bugs by checking each.
# HOW: Set every scenario field to baseline + 100. Every diff should = 100.
# ---------------------------------------------------------------------------
test_that("every diff field is computed from its matching input pair", {
  baseline <- fake_result()
  scenario <- fake_result(
    end_diagnosed         = baseline$end_diagnosed         + 100,
    end_on_art            = baseline$end_on_art            + 100,
    end_suppressed        = baseline$end_suppressed        + 100,
    tests_performed       = baseline$tests_performed       + 100,
    positive_tests        = baseline$positive_tests        + 100,
    new_diagnoses         = baseline$new_diagnoses         + 100,
    art_initiations       = baseline$art_initiations       + 100,
    ltfu_new_effective    = baseline$ltfu_new_effective    + 100,
    ltfu_prevented        = baseline$ltfu_prevented        + 100,
    ltfu_reengaged        = baseline$ltfu_reengaged        + 100,
    end_new_infections    = baseline$end_new_infections    + 100,
    end_infant_infections = baseline$end_infant_infections + 100,
    end_total_infections  = baseline$end_total_infections  + 100,
    end_deaths            = baseline$end_deaths            + 100,
    total_intervention_cost = baseline$total_intervention_cost + 100,
    art_provision_cost      = baseline$art_provision_cost      + 100,
    total_cost              = baseline$total_cost              + 100
  )
  d <- calculate_scenario_difference(scenario, baseline)

  expect_close(d$diff_diagnosed,           100)
  expect_close(d$diff_on_art,              100)
  expect_close(d$diff_suppressed,          100)
  expect_close(d$diff_tests_performed,     100)
  expect_close(d$diff_positive_tests,      100)
  expect_close(d$diff_new_diagnoses,       100)
  expect_close(d$diff_art_initiations,     100)
  expect_close(d$diff_ltfu_new_effective,  100)
  expect_close(d$diff_ltfu_prevented,      100)
  expect_close(d$diff_ltfu_reengaged,      100)
  expect_close(d$diff_new_infections,      100)
  expect_close(d$diff_infant_infections,   100)
  expect_close(d$diff_total_infections,    100)
  expect_close(d$diff_deaths,              100)
  expect_close(d$diff_intervention_cost,   100)
  expect_close(d$diff_art_provision_cost,  100)
  expect_close(d$diff_total_cost,          100)
})
