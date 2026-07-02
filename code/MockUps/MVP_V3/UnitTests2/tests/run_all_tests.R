# ============================================================================
# run_all_tests.R
# ----------------------------------------------------------------------------
# Orchestrator for the HIV Intervention Impact Calculator test suite.
#
# USAGE:
#   From the tests/ directory:
#       Rscript run_all_tests.R
#   Or interactively:
#       source("run_all_tests.R")     # working directory is set below
#
# OUTPUT:
#   testthat's default reporter shows per-test results inline plus a final
#   summary (e.g. [ FAIL 0 | WARN 0 | SKIP 0 | PASS 42 ]). Exit code reflects
#   pass/fail for CI integration.
#
# REQUIREMENTS:
#   - testthat installed
#   - Internet access (helpers.R sources Mock-Up_logic_V2.R, which hits
#     SharePoint at load time)
#
# CURRENT THEMATIC FILES:
#   test_01_populations.R     - calculate_populations
#   test_02_strata_foi.R      - FOI module (strata, calibration, scenarios)
#   test_03_testing.R         - testing intervention block
#   test_04_prevention.R      - PEP allocation, stacking, cost loop special cases
#   test_05_retention_ltfu.R  - DSD additive, tracking deferred, spontaneous
#   test_06_mtct_infant.R     - MTCT cascade, PMTCT linkage, EID, infant mortality
#   test_07_mortality.R       - 5-group deaths, AHD package, calibration toggle
#   test_08_cascade_end.R     - End-of-year cascade reconciliation
#   test_09_costs.R           - Cost branches not covered upstream (EAC, CD4, AHD, ART, ANC)
#   test_10_scenario_diff.R   - Scenario-vs-baseline difference function
#   test_11_integration.R     - End-to-end: real simulator output -> diff function
#
# ADDING A NEW THEMATIC FILE:
#   Save as test_XX_<theme>.R in this directory; test_dir() picks it up
#   automatically by filename pattern.
# ============================================================================

# ---------------------------------------------------------------------------
# Local environment setup
# ---------------------------------------------------------------------------
# Paths are hard-codedlocal. If running on a different
# machine, edit both paths or set HIV_LOGIC_PATH externally and comment out
# the Sys.setenv() line.
Sys.setenv(HIV_LOGIC_PATH = "/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/Mock-Up_logic_V2.R")
setwd("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/UnitTests2/tests/")

suppressPackageStartupMessages(library(testthat))

# test_dir runs every file matching ^test.*\.R$ in the given directory using
# its own context, reports per-file progress, and returns a results object.
results <- test_dir(".", reporter = "summary", stop_on_failure = FALSE)

# Extract summary counts for CI exit code
results_df <- as.data.frame(results)
n_failed   <- sum(results_df$failed)
n_warned   <- sum(results_df$warning)
n_passed   <- sum(results_df$nb) - n_failed - n_warned

cat(sprintf("\nFinal summary: PASS %d | FAIL %d | WARN %d\n",
            n_passed, n_failed, n_warned))

# Exit with non-zero status if anything failed (useful for CI)
if (!interactive() && n_failed > 0) {
  quit(status = 1)
}
