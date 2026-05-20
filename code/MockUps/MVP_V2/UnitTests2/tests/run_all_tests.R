# ============================================================================
# run_all_tests.R
# ----------------------------------------------------------------------------
# Orchestrator for the HIV Intervention Impact Calculator test suite.
#
# USAGE:
#   From the tests/ directory:
#       Rscript run_all_tests.R
#   Or interactively:
#       setwd("tests"); source("run_all_tests.R")
#
# OUTPUT:
#   testthat's default reporter shows per-test results inline plus a final
#   summary (e.g. [ FAIL 0 | WARN 0 | SKIP 0 | PASS 42 ]). Exit code reflects
#   pass/fail for CI integration.
#
# REQUIREMENTS:
#   - testthat installed
#   - Internet access (test_helpers.R sources Mock-Up_logic_V2.R, which hits
#     SharePoint at load time)
#   - HIV_LOGIC_PATH environment variable can override the default logic file
#     location (defaults to /mnt/user-data/uploads/Mock-Up_logic_V2.R)
#
# ADDING A NEW THEMATIC FILE:
#   Save as test_XX_<theme>.R in this directory; test_dir() picks it up
#   automatically by filename pattern.
# ============================================================================
Sys.setenv(HIV_LOGIC_PATH = "/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/code/MockUps/MVP_V2/Mock-Up_logic_V2.R")  # adjust to your actual path
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
