# =============================================================================
# Pull median (and mean) duration of breastfeeding from the DHS API
# -----------------------------------------------------------------------------
# Source : The DHS Program STATcompiler API
#          https://api.dhsprogram.com/#/api-indicators.cfm
#          https://api.dhsprogram.com/#/api-data.cfm
# License: DHS aggregate indicator data are free to use. No API key required
#          for aggregate (non-microdata) endpoints.
#
# Output : data/dhs_bf_duration.csv  -- one row per (country, survey, indicator)
#
# Caveats (READ BEFORE MODELLING):
#   1. Median/mean duration are CURRENT-STATUS life-table estimates from
#      children under 3 years at survey, NOT retrospective completed durations.
#   2. DHS-8 (~2022 onward) dropped these indicators from the standard tabulation
#      plan. Expect missing values for the most recent surveys.
#      Source: https://dhsprogram.com/data/Guide-to-DHS-Statistics/Breastfeeding_and_Complementary_Feeding.htm
#   3. DHS-7 changed how months-since-birth are computed vs earlier phases.
#      Estimates are close but not identical -- flag for time-series use.
#   4. DHS is LMIC-only. No high-income country coverage.
#   5. Survey vintage varies widely. Always carry SurveyYear alongside the value.
# =============================================================================

# ---- Packages ---------------------------------------------------------------
# install.packages(c("httr2", "jsonlite", "dplyr", "tidyr", "readr", "purrr"))
suppressPackageStartupMessages({
  library(httr2)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(readr)
  library(purrr)
})

API_ROOT <- "https://api.dhsprogram.com/rest/dhs"

# ---- Helper: paged GET ------------------------------------------------------
# The DHS API paginates large result sets (default 100/page).
# We loop until RecordsReturned < perPage.
dhs_get_all <- function(path, query = list(), per_page = 5000) {
  results <- list()
  page <- 1
  repeat {
    q <- c(query, list(perPage = per_page, page = page, f = "json"))
    resp <- request(paste0(API_ROOT, path)) |>
      req_url_query(!!!q) |>
      req_retry(max_tries = 4, backoff = ~ 2^.x) |>
      req_perform()
    parsed <- resp_body_json(resp, simplifyVector = TRUE)
    if (is.null(parsed$Data) || length(parsed$Data) == 0) break
    results[[page]] <- as_tibble(parsed$Data)
    # Stop when we've fetched everything
    total <- parsed$RecordCount %||% nrow(results[[page]])
    fetched <- sum(vapply(results, nrow, integer(1)))
    if (fetched >= total) break
    page <- page + 1
  }
  if (length(results) == 0) tibble() else bind_rows(results)
}

# httr2 uses %||% from rlang; define a fallback in case
`%||%` <- function(a, b) if (is.null(a)) b else a

# ---- Step 1: find the exact indicator IDs ------------------------------------
# We search the indicator catalogue for "Median duration" and "Mean duration"
# of breastfeeding rather than hard-coding IDs that may change.
message("Step 1/3: Looking up breastfeeding-duration indicator IDs...")

all_indicators <- dhs_get_all(
  "/indicators",
  query = list(returnFields = "IndicatorId,Label,Definition,Level1,Level2")
)

bf_duration_ind <- all_indicators |>
  filter(
    grepl("breastfeed", Label, ignore.case = TRUE),
    grepl("duration",   Label, ignore.case = TRUE),
    grepl("median|mean", Label, ignore.case = TRUE)
  ) |>
  select(IndicatorId, Label) |>
  distinct()

print(bf_duration_ind)
# Inspect the printed table -- you may want to narrow it further, e.g.
# keep only "any breastfeeding", "exclusive breastfeeding", "predominant".
# For now, take all matches:
ind_ids <- bf_duration_ind$IndicatorId
stopifnot(length(ind_ids) > 0)

# ---- Step 2: pull the data --------------------------------------------------
# The /data endpoint accepts a comma-separated list of indicator IDs.
# breakdown=national restricts to the country-level aggregate (no subgroups).
message("Step 2/3: Pulling data for ", length(ind_ids), " indicators...")

bf_data <- dhs_get_all(
  "/data",
  query = list(
    indicatorIds = paste(ind_ids, collapse = ","),
    breakdown    = "national",
    returnFields = paste(
      "Indicator", "IndicatorId", "Value", "Precision",
      "CountryName", "DHS_CountryCode",
      "SurveyId", "SurveyYear", "SurveyType",
      "CILow", "CIHigh", "DenominatorUnweighted", "DenominatorWeighted",
      sep = ","
    )
  )
)

message("Rows fetched: ", nrow(bf_data))

# ---- Step 3: tidy + write ---------------------------------------------------
bf_tidy <- bf_data |>
  mutate(
    Value = as.numeric(Value),
    SurveyYear = as.integer(SurveyYear)
  ) |>
  # Map IndicatorId -> a short variable name for easier reshaping
  mutate(
    var = case_when(
      grepl("median",   Indicator, ignore.case = TRUE) &
        grepl("any",     Indicator, ignore.case = TRUE)              ~ "median_any_bf",
      grepl("median",   Indicator, ignore.case = TRUE) &
        grepl("exclusive", Indicator, ignore.case = TRUE)            ~ "median_excl_bf",
      grepl("median",   Indicator, ignore.case = TRUE) &
        grepl("predominant", Indicator, ignore.case = TRUE)          ~ "median_predom_bf",
      grepl("mean",     Indicator, ignore.case = TRUE) &
        grepl("any",     Indicator, ignore.case = TRUE)              ~ "mean_any_bf",
      TRUE                                                            ~ Indicator
    )
  ) |>
  arrange(CountryName, SurveyYear, var)

dir.create("data", showWarnings = FALSE)
write_csv(bf_tidy, "data/dhs_bf_duration_long.csv")

# Wide format (one row per survey) for quick inspection / modelling
bf_wide <- bf_tidy |>
  select(CountryName, DHS_CountryCode, SurveyId, SurveyYear, SurveyType,
         var, Value) |>
  pivot_wider(names_from = var, values_from = Value)

write_csv(bf_wide, "data/dhs_bf_duration_wide.csv")

message("Step 3/3: Wrote data/dhs_bf_duration_long.csv (",
        nrow(bf_tidy), " rows) and data/dhs_bf_duration_wide.csv (",
        nrow(bf_wide), " rows).")

# ---- Quick sanity check -----------------------------------------------------
# Look at the most recent survey per country, median of any breastfeeding
recent_any <- bf_wide |>
  filter(!is.na(median_any_bf)) |>
  group_by(CountryName) |>
  slice_max(SurveyYear, n = 1, with_ties = FALSE) |>
  ungroup() |>
  arrange(desc(median_any_bf)) |>
  select(CountryName, SurveyYear, median_any_bf, median_excl_bf)

print(head(recent_any, 15))
cat("\nCountries with at least one observation:", n_distinct(bf_wide$CountryName), "\n")
cat("Year range:", min(bf_wide$SurveyYear), "-", max(bf_wide$SurveyYear), "\n")


recent_any$CountryName[recent_any$CountryName=="Cote d'Ivoire"]="Côte d'Ivoire"

recent_any$CountryName[recent_any$CountryName=="Tanzania"]="United Republic of Tanzania"

sub_countries2=c("Botswana","Côte d'Ivoire","Eswatini","Ghana","Kenya","Lesotho","Malawi","Mozambique","Nigeria","South Africa","United Republic of Tanzania","Uganda","Zambia","Zimbabwe")

recent_any_sub=recent_any %>% filter(CountryName%in%sub_countries2)
recent_any_sub$bf_duration_months=round(recent_any_sub$median_any_bf)
recent_any_sub=recent_any_sub %>% select(country=CountryName,bf_duration_months)

write.csv(recent_any_sub,"/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/bf_duration.csv")



