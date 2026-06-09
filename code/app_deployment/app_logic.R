# ============================================================================
# HIV Intervention Impact Calculator - Logic (NEW VERSION)
# ============================================================================
# This tool allows users to model the health and cost impacts of scaling
# HIV interventions up or down across prevention, testing, and treatment.
# 
# KEY CHANGE: All scenarios (baseline, scenario 1, scenario 2) are calculated
# as independent absolute outcomes, then differences are computed.
# ============================================================================
rm(list=ls())
library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(httr)
library(readr)
library(readxl)
options(tier.mort_diag = FALSE)

# Null-coalescing helper used throughout FOI module.
# Returns `a` if it is non-NULL, non-empty, and not entirely NA; else `b`.
# Robust to length-0 vectors (numeric(0), logical(0)) and length-N vectors
# with NAs, which the naive `!is.null(a) && !is.na(a)` form chokes on with
# "missing value where TRUE/FALSE needed" or length-coercion errors.
`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ============================================================================
# LOAD DATA
# ============================================================================
# Load country data
response <- GET("https://1drv.ms/x/c/2ae90f5cbd0fd171/IQBCFFlfF2AaRLcGuaCvNAcJAbE-8Ak2_gDyNJnL0GQu8Ag?e=k5dAU1&download=1")
country_data_csv <- content(response, as = "parsed", type = "text/csv")

# Load country-level baseline intervention volumes
# Replace the URL below with the actual share link for the baseline CSV
baseline_response <- GET("https://1drv.ms/x/c/2ae90f5cbd0fd171/IQAnibhIen_1TbiM3pkMXOTzAW2NkrUyv3KueNCHV1Tu_sI?e=96oyQi&download=1")
baseline_data_csv <- content(baseline_response, as = "parsed", type = "text/csv") 


# Load intervention parameters from Excel
load_intervention_params <- function(){
  sharepoint_url_interventions <- "https://bushare-my.sharepoint.com/:x:/g/personal/brooken_bu_edu/IQDkEN28uBz4Q6HD1Ydfa-mKASlPto-TuBhjDXChgC-eFbs?e=WuMKZs&download=1"
  
  temp_file_int <- tempfile(fileext = ".xlsx")
  download.file(sharepoint_url_interventions, temp_file_int, mode = "wb", method = "libcurl")
  
  intervention_params <- read_excel(temp_file_int, col_names = FALSE)
  
  colnames(intervention_params) <- as.character(intervention_params[2, ])
  intervention_params <- intervention_params[-1, ]
  intervention_params <- intervention_params[-1, ]
  
  intervention_params <- intervention_params %>% 
    select(category, intervention, intervention_key, parameter_type, current_value) %>% 
    spread(parameter_type, current_value)
  
  intervention_params$efficacy <- as.numeric(intervention_params$efficacy)
  intervention_params$unit_cost <- as.numeric(intervention_params$unit_cost)
  intervention_params$linkage_cost <- as.numeric(intervention_params$linkage_cost)
  intervention_params$linkage_rate <- as.numeric(intervention_params$linkage_rate)
  #intervention_params$multiplier <- as.numeric(intervention_params$multiplier)
  
  return(intervention_params)
}

# ============================================================================
# LOAD MODEL PARAMETERS FROM EXCEL
# Reads the flat key/value sheet 'params_for_loading' and returns a named list.
# Falls back to hardcoded defaults if the download fails.
# ============================================================================
load_hiv_model_params <- function() {
  sharepoint_url_params <- "https://bushare-my.sharepoint.com/:x:/g/personal/brooken_bu_edu/IQDkEN28uBz4Q6HD1Ydfa-mKASlPto-TuBhjDXChgC-eFbs?e=WuMKZs&download=1"
  
  temp_file_params <- tempfile(fileext = ".xlsx")
  download.file(sharepoint_url_params, temp_file_params,
                mode = "wb", method = "libcurl")
  df <- read_excel(temp_file_params, sheet = "general_values")
  out <- as.list(setNames(as.numeric(df$value), df$key))
  out[!is.na(names(out)) & nzchar(names(out))]
  
  
}

# Global parameter store — loaded once at app startup
hiv_params <- load_hiv_model_params()

# At the top of Mock-Up_logic_V2.R (just once, near other globals around line 92)
.last_diag_country <- new.env(parent = emptyenv())


# ============================================================================
# MORTALITY RATES BY CASCADE STAGE (UPDATE THESE BASED ON LITERATURE)
# ============================================================================
MORTALITY_RATES <- list(
  untreated_undiagnosed = hiv_params$mortality_untreated_undiagnosed,  # Average 350-500
  new_art_initiations   = hiv_params$mortality_new_art_initiations,  # first year on ART (pre-stabilisation)
  treated               = hiv_params$mortality_treated, # established on ART, not virally suppressed
  suppressed            = hiv_params$mortality_suppressed, # established on ART, virally suppressed
  ahd_untreated         = hiv_params$mortality_ahd_untreated,  # AHD (CD4<200) among undiagnosed / diagnosed not on ART
  # AHD on-ART mortality split by ART duration. Year-1 AHD mortality is
  # dramatically higher than long-term AHD mortality (per Thembisa 5.0
  # Table 3.1: ~0.14 in year 1, ~0.02 for months 7+ cohort-weighted).
  # A single "ahd_treated" value cannot capture both; previously a 0.05
  # compromise was used, which overstated established and understated new.
  # Falls back to mortality_ahd if the new keys aren't yet in the CSV.
  ahd_new               = if (!is.null(hiv_params$mortality_ahd_new) &&
                              !is.na(hiv_params$mortality_ahd_new))
    hiv_params$mortality_ahd_new
  else hiv_params$mortality_ahd,
  ahd_established       = if (!is.null(hiv_params$mortality_ahd_established) &&
                              !is.na(hiv_params$mortality_ahd_established))
    hiv_params$mortality_ahd_established
  else hiv_params$mortality_ahd,
  prop_ahd = list(
    undiagnosed        = hiv_params$prop_ahd_undiagnosed,
    diagnosed_not_art  = hiv_params$prop_ahd_diagnosed_not_art,
    new_initiations    = hiv_params$prop_ahd_new_initiations,
    established_treated= hiv_params$prop_ahd_established_treated,
    established_supp   = hiv_params$prop_ahd_established_supp
  )
)

# ============================================================================
# MORTALITY CALIBRATION TOGGLE
# ----------------------------------------------------------------------------
# When TRUE, model deaths are scaled at baseline so that total HIV deaths
# match the country's UNAIDS aids_deaths_per_year value. The same factor
# is then applied to all scenarios, preserving the relative impact of
# interventions while anchoring absolute deaths to country reality.
#
# When FALSE, model uses raw literature-based per-cascade mortality rates;
# total deaths reflect SSA-average rates, not country specifics.
#
# Set FALSE to compare uncalibrated outputs, run a model audit, or revert
# to pure literature behaviour without removing any code.
# ============================================================================
USE_MORTALITY_CALIBRATION <- FALSE

# ============================================================================
# LTFU RATES BY STABILITY STATUS (UPDATE THESE BASED ON LITERATURE)
# Stable patients: established, clinically stable

# ============================================================================
ANNUAL_LTFU_RATE_STABLE   <- hiv_params$ltfu_rate_stable  
ANNUAL_LTFU_RATE_UNSTABLE <- hiv_params$ltfu_rate_unstable

# ============================================================================
# SPONTANEOUS RE-ENGAGEMENT RATE 
# Annual proportion of the LTFU pool that returns to care without any explicit
# intervention. Captures silent transfers (patients classified
# as LTFU at one facility but receiving care elsewhere) and self-initiated
# re-engagement.
# ============================================================================
ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE =hiv_params$spontaneous_reengagement_rate


# Proportion of *retained* unsuppressed patients who achieve viral suppression
# as a direct result of the improved adherence that prevented their dropout.
# i.e. the intervention both keeps them in care AND improves their adherence enough
# to suppress.
RETENTION_SUPPRESSION_RATE <- hiv_params$retention_suppression_rate

ART_COST_STANDARD <- hiv_params$art_cost_standard %||% 200
# ============================================================================
# MTCT RATES BY MATERNAL ART/SUPPRESSION STATUS
# Single cumulative rates covering pregnancy, delivery, and breastfeeding.
# Sourced directly from hiv_params (Excel general_values).
# NVP efficacy is adjusted dynamically in the MTCT block using
# bf_duration_months and nvp_prophylaxis_duration_months.
# ============================================================================
MTCT_RATES <- list(
  on_art_suppressed    = hiv_params$mtct_on_art_suppressed,
  on_art_unsuppressed  = hiv_params$mtct_on_art_unsuppressed,
  not_on_art           = hiv_params$mtct_not_on_art
)

# ============================================================================
# INFANT HIV MORTALITY RATES BY TREATMENT STATUS 
# Annual mortality risk for HIV-infected infants by treatment/suppression status.
# ============================================================================
INFANT_MORTALITY_RATES <- list(
  untreated     = hiv_params$infant_mort_untreated,   
  on_art        = hiv_params$infant_mort_on_art,  
  suppressed    = hiv_params$infant_mort_suppressed   
)

# ============================================================================
# CONDOM BEHAVIOURAL PARAMETERS (UPDATE THESE BASED ON LITERATURE)
# acts_per_year: average sex acts per year by risk group — converts condoms
#   distributed into people with consistent coverage
# condom_use_rate: fraction of acts where someone *with access* uses a condom
#   High-risk rate higher (targeted programmes, stronger motivation)
# ============================================================================
ACTS_PER_YEAR_HIGH        <- hiv_params$acts_per_year_high   # KP / high-concurrency
ACTS_PER_YEAR_GEN         <- hiv_params$acts_per_year_gen   # general population
CONDOM_USE_RATE_HIGH      <- hiv_params$condom_use_rate_high
CONDOM_USE_RATE_GEN       <- hiv_params$condom_use_rate_gen


# Targeting of CD4 tests toward suspected AHD cases.
# Fraction of CD4 tests performed that land on patients who actually have AHD,
# at low/moderate coverage. Reflects clinical triage (WHO stage, BMI, symptoms).
# Capped automatically by the AHD pool size, so at high coverage the effective
# yield converges to prop_ahd$new_initiations.
CD4_AHD_TARGETING_YIELD <- hiv_params$prop_cd4_ahd 


# ============================================================================
# BUILD INTERVENTION GROUPS
# ============================================================================
build_intervention_groups <- function(intervention_params){
  intervention_groups <- list(
    prevention = list(
      name = "Prevention",
      color = "#10b981",
      interventions = list(
        prep_oral = list(
          name = "PrEP (oral)",
          type = "absolute",
          unit_label = "people",
          efficacy = subset(intervention_params, intervention_key == "prep_oral")$efficacy,
          eligible_pop = "sexually_active_negative",
          unit_cost = subset(intervention_params, intervention_key == "prep_oral")$unit_cost,
          outcomes = c("adult_infections")
        ),
        prep_lenacapavir = list(
          name = "PrEP (Lenacapavir)",
          type = "absolute",
          unit_label = "people",
          efficacy = subset(intervention_params, intervention_key == "prep_lenacapavir")$efficacy,
          eligible_pop = "sexually_active_negative",
          unit_cost = subset(intervention_params, intervention_key == "prep_lenacapavir")$unit_cost,
          outcomes = c("adult_infections")
        ),
        vmmc = list(
          name = "VMMC",
          type = "absolute",
          unit_label = "annual people",
          efficacy = subset(intervention_params, intervention_key == "vmmc")$efficacy,
          eligible_pop = "uncircumcised_males_all",
          unit_cost = subset(intervention_params, intervention_key == "vmmc")$unit_cost,
          outcomes = c("adult_infections")
        ),
        condoms = list(
          name = "Condom distribution",
          type = "absolute",
          unit_label = "condoms distributed",
          efficacy = subset(intervention_params, intervention_key == "condoms")$efficacy,
          eligible_pop = "sexually_active_negative",
          unit_cost = subset(intervention_params, intervention_key == "condoms")$unit_cost,
          outcomes = c("adult_infections")
        ),
        infant_prophylaxis = list(
          name = "Infant HIV prophylaxis to reduce vertical transmission",
          type = "coverage",
          unit_label = "% of HIV-exposed infants",
          efficacy = subset(intervention_params, intervention_key == "infant_prophylaxis")$efficacy,
          eligible_pop = "hiv_exposed_infants",
          unit_cost = subset(intervention_params, intervention_key == "infant_prophylaxis")$unit_cost,
          outcomes = c("infant_infections")
        )
      )
    ),
    
    testing = list(
      name = "Testing & Diagnosis",
      color = "#3b82f6",
      interventions = list(
        test_facility_general = list(
          name = "Testing: facility-based (general)",
          type = "absolute",
          unit_label = "tests performed",
          efficacy = subset(intervention_params, intervention_key == "test_facility_general")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "test_facility_general")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "test_facility_general")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "test_facility_general")$linkage_cost,
          outcomes = c("testing")
        ),
        test_network = list(
          name = "Testing: network testing",
          type = "absolute",
          unit_label = "tests performed",
          efficacy = subset(intervention_params, intervention_key == "test_network")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "test_network")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "test_network")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "test_network")$linkage_cost,
          outcomes = c("testing")
        ),
        test_index = list(
          name = "Testing: index testing",
          type = "absolute",
          unit_label = "tests performed",
          efficacy = subset(intervention_params, intervention_key == "test_index")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "test_index")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "test_index")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "test_index")$linkage_cost,
          outcomes = c("testing")
        ),
        test_community = list(
          name = "Testing: community-based",
          type = "absolute",
          unit_label = "tests performed",
          efficacy = subset(intervention_params, intervention_key == "test_community")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "test_community")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "test_community")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "test_community")$linkage_cost,
          outcomes = c("testing")
        ),
        test_kpsti = list(
          name = "Testing: key populations & STI services",
          type = "absolute",
          unit_label = "tests performed",
          efficacy = subset(intervention_params, intervention_key == "test_kpsti")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "test_kpsti")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "test_kpsti")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "test_kpsti")$linkage_cost,
          outcomes = c("testing")
        ),
        hivst_facility = list(
          name = "HIVST (Facility-based)",
          type = "absolute",
          unit_label = "tests distributed",
          efficacy = subset(intervention_params, intervention_key == "hivst_facility")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "hivst_facility")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "hivst_facility")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "hivst_facility")$linkage_cost,
          outcomes = c("testing")
        ),
        hivst_community = list(
          name = "HIVST (Community-based)",
          type = "absolute",
          unit_label = "tests distributed",
          efficacy = subset(intervention_params, intervention_key == "hivst_community")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "hivst_community")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "hivst_community")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "hivst_community")$linkage_cost,
          outcomes = c("testing")
        ),
        eid = list(
          name = "EID (Early Infant Diagnosis)",
          type = "coverage",
          unit_label = "% of HIV-exposed infants",
          efficacy = subset(intervention_params, intervention_key == "eid")$efficacy,
          eligible_pop = "hiv_exposed_infants",
          unit_cost = subset(intervention_params, intervention_key == "eid")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "eid")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "eid")$linkage_cost,
          outcomes = c("infant_diagnosis")
        ),
        anc_hiv_testing = list(
          name = "ANC: HIV testing",
          type = "coverage",
          unit_label = "% of pregnant women",
          efficacy = subset(intervention_params, intervention_key == "anc_hiv_testing")$efficacy,
          eligible_pop = "pregnant_hiv_testable",
          unit_cost = subset(intervention_params, intervention_key == "anc_hiv_testing")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "anc_hiv_testing")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "anc_hiv_testing")$linkage_cost,
          outcomes = c("testing")
        ),
        pnc_hiv_testing = list(
          name = "PNC: HIV testing",
          type = "coverage",
          unit_label = "% of postpartum women",
          efficacy = subset(intervention_params, intervention_key == "pnc_hiv_testing")$efficacy,
          eligible_pop = "pregnant_hiv_testable",
          unit_cost = subset(intervention_params, intervention_key == "pnc_hiv_testing")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "pnc_hiv_testing")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "pnc_hiv_testing")$linkage_cost,
          outcomes = c("testing")
        )
      )
    ),
    
    treatment_monitoring = list(
      name = "Treatment Monitoring & Quality",
      color = "#f59e0b",
      interventions = list(
        vl_monitoring_routine = list(
          name = "Routine VL monitoring",
          type = "coverage",
          unit_label = "% of people on ART",
          efficacy = subset(intervention_params, intervention_key == "vl_monitoring_routine")$efficacy,
          eligible_pop = "on_art",
          unit_cost = subset(intervention_params, intervention_key == "vl_monitoring_routine")$unit_cost,
          outcomes = c("viral_suppression")
        ),
        mmd_3month = list(
          name = "MMD: 3-month dispensing",
          type = "coverage",
          unit_label = "% of stable clients",
          efficacy = subset(intervention_params, intervention_key == "mmd_3month")$efficacy,
          eligible_pop = "on_art_stable",
          unit_cost = subset(intervention_params, intervention_key == "mmd_3month")$unit_cost,
          outcomes = c("retention")
        ),
        mmd_6month = list(
          name = "MMD: 6-month dispensing",
          type = "coverage",
          unit_label = "% of stable clients",
          efficacy = subset(intervention_params, intervention_key == "mmd_6month")$efficacy,
          eligible_pop = "on_art_stable",
          unit_cost = subset(intervention_params, intervention_key == "mmd_6month")$unit_cost,
          outcomes = c("retention")
        ),
        mmd_12month = list(
          name = "MMD: 12-month dispensing",
          type = "coverage",
          unit_label = "% of stable clients",
          efficacy = subset(intervention_params, intervention_key == "mmd_12month")$efficacy,
          eligible_pop = "on_art_stable",
          unit_cost = subset(intervention_params, intervention_key == "mmd_12month")$unit_cost,
          outcomes = c("retention")
        ),
        community_pickup = list(
          name = "Community ART pick-up",
          type = "coverage",
          unit_label = "% of stable clients",
          efficacy = subset(intervention_params, intervention_key == "community_pickup")$efficacy,
          eligible_pop = "on_art_stable",
          unit_cost = subset(intervention_params, intervention_key == "community_pickup")$unit_cost,     
          outcomes = c("retention")
        )
      )
    ),
    
    retention_support = list(
      name = "Retention & Adherence Support",
      color = "#ec4899",
      interventions = list(
        adherence_counseling = list(
          name = "Enhanced adherence counselling",
          type = "coverage",
          unit_label = "% of VL-identified unsuppressed on ART",
          efficacy = subset(intervention_params, intervention_key == "adherence_counseling")$efficacy,
          eligible_pop = "on_art",
          unit_cost = subset(intervention_params, intervention_key == "adherence_counseling")$unit_cost,
          outcomes = c("viral_suppression")
        ),
        tracking_tracing = list(
          name = "Tracking & tracing",
          type = "coverage",
          unit_label = "% of LTFU patients",
          efficacy = subset(intervention_params, intervention_key == "tracking_tracing")$efficacy,
          eligible_pop = "ltfu",
          unit_cost = subset(intervention_params, intervention_key == "tracking_tracing")$unit_cost,
          outcomes = c("retention")
        ),
        anc_vl_testing = list(
          name = "ANC: Viral Load Testing",
          type = "coverage",
          unit_label = "% of pregnant women on ART",
          efficacy = subset(intervention_params, intervention_key == "anc_vl_testing")$efficacy,
          eligible_pop = "pregnant_on_art",
          unit_cost = subset(intervention_params, intervention_key == "anc_vl_testing")$unit_cost,
          outcomes = c("viral_suppression", "pmtct")
        ),
        pnc_vl_testing = list(
          name = "PNC: Viral Load Testing",
          type = "coverage",
          unit_label = "% of postpartum women on ART",
          efficacy  = {v <- subset(intervention_params, intervention_key == "pnc_vl_testing")$efficacy;  if (length(v) > 0) v else 0.40},
          eligible_pop = "pregnant_on_art",
          unit_cost = {v <- subset(intervention_params, intervention_key == "pnc_vl_testing")$unit_cost; if (length(v) > 0) v else 10},
          outcomes = c("viral_suppression", "pmtct")
        )
      )
    ),
    
    advanced_disease = list(
      name = "Advanced HIV Disease Package",
      color = "#8b5cf6",
      interventions = list(
        cd4_testing = list(
          name = "CD4 testing (all new initiations)",
          type = "coverage",
          unit_label = "% of new ART initiations",
          efficacy = subset(intervention_params, intervention_key == "cd4_testing")$efficacy,
          eligible_pop = "new_art_initiations",
          unit_cost = subset(intervention_params, intervention_key == "cd4_testing")$unit_cost,
          outcomes = c("ahd_screening")
        ),
        ahd_package = list(
          name = "Full AHD package (LAM, CrAg, fluconazole)",
          type = "coverage",
          unit_label = "% of AHD-diagnosed new initiations",
          efficacy = subset(intervention_params, intervention_key == "ahd_package")$efficacy,
          eligible_pop = "new_art_initiations",
          unit_cost = subset(intervention_params, intervention_key == "ahd_package")$unit_cost,
          outcomes = c("mortality")
        )
      )
    )
  )
  
  return(intervention_groups)
}

# ============================================================================
# POPULATION CALCULATION FUNCTION
# ============================================================================
calculate_populations <- function(context) {
  
  plhiv <- context$plhiv
  diagnosed <- plhiv * (context$percent_diagnosed/100)
  on_art <- diagnosed * (context$percent_on_art / 100)
  suppressed <- on_art * (context$percent_suppressed / 100)
  unsuppressed_on_art <- on_art - suppressed
  hiv_negative <- context$total_population - plhiv
  
  
  
  # Safe defaults — prevents NULL propagation if CSV columns are missing/misnamed.
  # prop_pop_male drives uncircumcised_males and all FOI strata; if NULL, vmmc
  # baseline and FOI strata would silently become NULL and display as 0.
  # NB: this block must precede the sexually_active calculation, which depends
  # on prop_under14 to restrict the denominator to adults (15+).
  prop_male_pct <- if (!is.null(context$prop_pop_male) && !is.na(context$prop_pop_male))
    context$prop_pop_male else hiv_params$default_prop_pop_male
  prop_under14  <- if (!is.null(context$prop_pop_under_14) && !is.na(context$prop_pop_under_14))
    context$prop_pop_under_14 else hiv_params$default_prop_pop_under_14
  circ_prev     <- if (!is.null(context$circ_prevalence) && !is.na(context$circ_prevalence))
    context$circ_prevalence/100 else hiv_params$default_circ_prevalence
  prop_hr       <- if (!is.null(context$prop_high_risk) && !is.na(context$prop_high_risk))
    context$prop_high_risk else hiv_params$default_prop_high_risk
  
  # Sexually active = adults (15+) × fraction of adults active in past 12 months.
  # sexually_active_frac is now defined as the share of ADULTS (not total pop)
  # who report sex in the past 12 months
  sexually_active <- context$total_population * (1 - prop_under14/100) * hiv_params$sexually_active_frac
  births <- (context$total_population * context$birth_rate)/1000
  # HIV-EXPOSED INFANTS = births to HIV+ mothers (denominator for EID coverage
  # and infant prophylaxis eligibility — NOT HIV-positive infants; infant
  # infections are produced by the MTCT cascade downstream).
  #
  # anc_multiplier bridges general-population hiv_prevalence to prevalence
  # among pregnant women. Country-specific, sourced from CSV (ANC_multiplier
  # column), derived as ANC_prevalence / general_adult_prevalence. Defaults to
  # 1 if not supplied. Applied consistently to hiv_exposed_births AND every
  # pregnant_* denominator so the EID arm and the maternal cascade run off the
  # same cohort (one HIV-exposed infant per HIV+ pregnant woman).
  #
  # Defensive default: callers may build context without anc_multiplier (e.g.
  # the UI reactive context constructed from input$ values). NULL × number
  # gives numeric(0) in R, which silently corrupts every PMTCT calculation
  # downstream, so we coerce to 1 here.
  anc_mult <- context$anc_multiplier
  if (is.null(anc_mult) || length(anc_mult) == 0 || is.na(anc_mult) || anc_mult <= 0) {
    anc_mult <- 1
  }
  hiv_exposed_births <- births * context$hiv_prevalence * anc_mult
  
  # LTFU flow: people dropping off ART during the year, split by stability status.
  # Stable patients: DSD-eligible, lower dropout risk.
  # Unstable patients: not DSD-eligible, higher dropout risk.
  # New initiates excluded — their dropout is captured upstream via linkage rates.
  on_art_stable_n   <- on_art * ((context$percent_suppressed+hiv_params$prop_on_art_stable_diff) / 100)
  on_art_unstable_n <- on_art-on_art_stable_n
  ltfu_new_stable   <- on_art_stable_n   * ANNUAL_LTFU_RATE_STABLE
  ltfu_new_unstable <- on_art_unstable_n * ANNUAL_LTFU_RATE_UNSTABLE
  
  
  # Reconciliation invariant: cascade groups must sum to PLHIV.
  # Reconciliation invariant: cascade groups must sum to PLHIV.
  # Guarded against NULL/NA/zero-length inputs (Custom Country sets plhiv=NULL
  # at preset build; some country presets have NA cascade values).
  if (length(plhiv) == 1 && !is.na(plhiv) &&
      length(diagnosed) == 1 && !is.na(diagnosed) &&
      length(on_art) == 1 && !is.na(on_art)) {
    cascade_sum <- (plhiv - diagnosed) + (diagnosed - on_art) + on_art
    if (abs(cascade_sum - plhiv) > 1) {
      warning(sprintf("Cascade does not reconcile to PLHIV: sum=%.0f, plhiv=%.0f",
                      cascade_sum, plhiv))
    }
  }
  list(
    total = context$total_population,
    adult_pop = context$total_population * (1 - prop_under14/100),
    plhiv = context$plhiv,
    hiv_negative = hiv_negative,
    sexually_active = sexually_active,
    undiagnosed = plhiv - diagnosed,
    diagnosed = diagnosed,
    diagnosed_not_on_art = diagnosed - on_art,
    on_art = on_art,
    on_art_stable = on_art_stable_n ,
    suppressed = suppressed,
    unsuppressed = unsuppressed_on_art,
    # Year-start cascade decomposition (mutually exclusive, sums to PLHIV):
    #   undiagnosed + ltfu (all diagnosed not on ART) + on_art = plhiv
    # The model formerly split the diagnosed-not-on-ART pool into "lapsed
    # from ART" (ltfu) and "never linked" via prevalent_ltfu_frac, but the
    # split was not empirically grounded and produced spurious precision in
    # the cascade allocation. Now collapsed: all diagnosed-not-on-ART are
    # eligible for re-engagement via any route, subject to the testing
    # re-engagement cap (testing_reengagement_cap_frac).
    ltfu         = diagnosed - on_art,
    # Incident LTFU flow (people becoming LTFU during the year), by stability status
    ltfu_new_stable   = ltfu_new_stable,
    ltfu_new_unstable = ltfu_new_unstable,
    ltfu_new          = ltfu_new_stable + ltfu_new_unstable,
    # ── FOI strata (used by stratified infection model) ──────────────────
    high_risk_negative       = hiv_negative * prop_hr,
    general_female           = hiv_negative * (1 - prop_male_pct/100) * (1 - prop_hr),
    uncirc_male              = hiv_negative * (prop_male_pct/100) * (1 - circ_prev) * (1 - prop_hr),
    circ_male                = hiv_negative * (prop_male_pct/100) * circ_prev,
    # kept for VMMC eligible_pop reference in cost loop
    uncircumcised_males      = hiv_negative * (prop_male_pct/100) * (1 - circ_prev),
    uncircumcised_males_all  = context$total_population * (prop_male_pct/100) * (1 - circ_prev),  # HIV+ AND HIV- — used by VMMC cost cap
    sexually_active_negative = hiv_negative * (1 - prop_under14/100) * hiv_params$sexually_active_frac,
    recent_exposure = hiv_negative * hiv_params$recent_exposure_frac,
    hiv_exposed_infants = hiv_exposed_births,
    pregnant_women      = births,
    # PMTCT cascade sub-populations
    # Denominator = births × hiv_prevalence × anc_multiplier (= hiv_exposed_births)
    # The multiplier bridges general-population prevalence to prevalence among
    # pregnant women and MUST match hiv_exposed_births for cohort consistency
    # (every HIV-exposed infant corresponds to one pregnant HIV+ woman).
    pregnant_hiv_pos_cascade     = hiv_exposed_births,
    pregnant_on_art              = hiv_exposed_births *
      (context$percent_diagnosed / 100) * (context$percent_on_art_pregnant / 100),
    pregnant_on_art_suppressed   = hiv_exposed_births *
      (context$percent_diagnosed / 100) * (context$percent_on_art_pregnant / 100) *
      (context$percent_suppressed / 100),
    pregnant_on_art_unsuppressed = hiv_exposed_births *
      (context$percent_diagnosed / 100) * (context$percent_on_art_pregnant / 100) *
      (1 - context$percent_suppressed / 100),
    pregnant_not_on_art          = hiv_exposed_births *
      (1 - (context$percent_diagnosed / 100) * (context$percent_on_art_pregnant / 100)),
    pregnant_undiagnosed         = hiv_exposed_births * (1 - context$percent_diagnosed / 100),
    # HIV testing eligible pool: HIV-negative pregnant + HIV+ undiagnosed pregnant
    pregnant_hiv_testable        = (births - hiv_exposed_births) +
      (hiv_exposed_births * (1 - context$percent_diagnosed / 100))
  )
}

# ============================================================================
# DEFAULT BASELINE INTERVENTIONS
# ============================================================================
default_baseline_interventions <- list(
  prep_oral = 5000, prep_lenacapavir = 0, vmmc = 30000,
  condoms = 200000, infant_prophylaxis = 70,
  test_facility_general = 25000,
  test_network = 3000, test_index = 2000, test_community = 20000,
  test_kpsti = 8000, hivst_facility = 10000, hivst_community = 5000,
  eid = 75, anc_hiv_testing = 88, pnc_hiv_testing = 70,
  vl_monitoring_routine = 60, 
  mmd_3month = 50, mmd_6month = 10, mmd_12month = 5, community_pickup=5,
  adherence_counseling = 55, tracking_tracing = 40, anc_vl_testing = 68, pnc_vl_testing = 0,
  cd4_testing = 92, ahd_package = 88
)

# ============================================================================
# BUILD COUNTRY PRESETS FROM CSV
# ============================================================================
build_country_presets <- function(csv_data, baseline_csv = NULL) {
  presets <- list()
  
  if (!is.null(csv_data) && nrow(csv_data) > 0) {
    for (i in 1:nrow(csv_data)) {
      row <- csv_data[i, ]
      country_name <- row$country
      
      context <- list(
        total_population = row$total_population,
        hiv_prevalence = row$hiv_prevalence / 100,
        # ANC-to-adult HIV prevalence ratio (country-specific, from CSV).
        # Bridges general-population hiv_prevalence to prevalence among pregnant
        # women — used downstream for hiv_exposed_births and ALL pregnant_*
        # cascade denominators. Source: country ANC sentinel survey / Spectrum-AIM.
        # Defaults to 1 if missing or non-positive (avoids NaN propagation).
        # NOTE: nested if() rather than chained &&, because is.na(NULL) returns
        # logical(0) and breaks if() with "missing value where TRUE/FALSE needed"
        # when the ANC_multiplier column is absent from the CSV entirely.
        anc_multiplier = if (is.null(row$ANC_multiplier)) {
          1
        } else {
          val <- suppressWarnings(as.numeric(row$ANC_multiplier))
          if (is.na(val) || val <= 0) 1 else val
        },
        new_infections_per_year = row$new_infections_per_year,
        current_diagnoses = row$current_diagnoses,
        plhiv=row$plhiv,
        percent_diagnosed = row$percent_diagnosed,
        percent_on_art = row$percent_on_art,
        percent_on_art_pregnant = {
          val <- as.numeric(row$percent_on_art_pregnant)
          if (is.na(val)) as.numeric(row$percent_on_art) else val
        },
        # Country-specific ART unit cost (USD per person-year on ART).
        # Falls back to the global ART_COST_STANDARD (from hiv_params /
        # intervention_params Excel) if the column is missing, blank, or
        # negative. See basic_hiv_data.csv `art_cost_standard` column.
        art_cost_standard = {
          if (!is.null(row$art_cost_standard)) {
            val <- suppressWarnings(as.numeric(as.character(row$art_cost_standard)))
            if (is.na(val) || val < 0) ART_COST_STANDARD else val
          } else {
            ART_COST_STANDARD
          }
        },
        # Country-specific test unit costs (USD per test administered).
        # Each value falls back to the global intervention_params$unit_cost
        # for the corresponding intervention_key if the CSV column is missing,
        # blank, NA, or negative. See basic_hiv_data.csv columns prefixed `cost_*`.
        # Stored as a named list keyed by intervention_key so the cost-charge
        # site can look up via context$cost_overrides_test[[int_key]].
        # Covers the 9 modalities for which country costing data is commonly
        # available: 5 general HTS modalities, 2 HIVST, ANC and PNC HIV testing.
        # EID, VL, and CD4 deliberately excluded — keep global default.
        cost_overrides_test = {
          test_cost_keys <- c("test_network", "test_facility_general", "test_index",
                              "test_community", "test_kpsti",
                              "hivst_facility", "hivst_community",
                              "anc_hiv_testing", "pnc_hiv_testing")
          test_cost_out <- list()
          for (key_name in test_cost_keys) {
            csv_col <- paste0("cost_", key_name)
            if (!is.null(row[[csv_col]])) {
              parsed_val <- suppressWarnings(as.numeric(as.character(row[[csv_col]])))
              if (!is.na(parsed_val) && parsed_val >= 0) {
                test_cost_out[[key_name]] <- parsed_val
              }
            }
          }
          test_cost_out  # named list; absent keys mean "use global default"
        },
        
        percent_suppressed = row$percent_suppressed,
        aids_deaths_per_year = row$aids_deaths_per_year,
        birth_rate = row$birth_rate,
        prop_pop_male = if (!is.null(row$prop_male) && !is.na(row$prop_male))
          as.numeric(row$prop_male) else hiv_params$default_prop_pop_male,
        prop_pop_under_14 = if (!is.null(row$prop_under14) && !is.na(row$prop_under14))
          as.numeric(row$prop_under14) else hiv_params$default_prop_pop_under_14,
        # FOI parameters (optional CSV columns; defaults used if absent)
        circ_prevalence = if (!is.null(row$circ_prevalence) && !is.na(row$circ_prevalence)) row$circ_prevalence else hiv_params$default_circ_prevalence,
        prop_high_risk  = if (!is.null(row$prop_high_risk)  && !is.na(row$prop_high_risk))  row$prop_high_risk  else hiv_params$default_prop_high_risk,
        rr_high          = if (!is.null(row$rr_high)              && !is.na(row$rr_high))   row$rr_high  else hiv_params$default_rr_high,
        test_yield       = if (!is.null(row$avg_test_yield)       && !is.na(row$avg_test_yield))
          as.numeric(row$avg_test_yield) / 100 else NULL,  # % in CSV -> proportion
        prior_year_tests = if (!is.null(row$total_tests_prev_year) && !is.na(row$total_tests_prev_year))
          as.numeric(row$total_tests_prev_year) else NULL,
        # Retesting probability: country-specific override; NULL means use hiv_params default
        prop_retesting   = if (!is.null(row$prop_retest)        && !is.na(row$prop_retest))
          as.numeric(row$prop_retest) else NULL,
        # Country-specific mortality calibration flag. When TRUE, the country's
        # baseline modelled deaths are anchored to its UNAIDS aids_deaths_per_year
        # value via a single scalar factor that is then re-applied to all
        # scenarios. Defaults to FALSE when the column is absent or blank, so
        # existing CSVs without this column behave identically to today.
        # Set TRUE for countries where uncalibrated literature rates produce
        # implausible totals (e.g. South Africa, where the very large
        # diagnosed-not-on-ART pool is treated as ART-naive by the rate
        # parameters and inflates predicted deaths ~3x).
        use_mortality_calibration = if (!is.null(row$use_mortality_calibration) &&
                                        !is.na(row$use_mortality_calibration)) {
          val <- row$use_mortality_calibration
          isTRUE(val) || identical(val, 1) || identical(val, 1L) ||
            (is.character(val) && toupper(val) %in% c("TRUE", "T", "1", "YES", "Y"))
        } else FALSE
      )
      
      pops <- calculate_populations(context)
      
      default_baseline_interventions <- list(
        prep_oral = 0.01*pops$total, prep_lenacapavir = 0, vmmc = 0.01*pops$uncircumcised_males,
        condoms = 0.6*pops$total, infant_prophylaxis = 70,
        test_facility_general = round(0.134*pops$adult_pop, -4), 
        test_network = round(0.0024*pops$adult_pop, -4), 
        test_index = round(0.0024*pops$adult_pop, -4), 
        test_community = round(0.019*pops$adult_pop, -4),
        test_kpsti = round(0.005*pops$adult_pop, -4), 
        hivst_facility = round(0.0035*pops$adult_pop, -4), 
        hivst_community = round(0.0035*pops$adult_pop, -4),
        eid = 75, anc_hiv_testing = 88, pnc_hiv_testing = 70,
        vl_monitoring_routine = 60, 
        mmd_3month = 50, mmd_6month = 10, mmd_12month = 5, community_pickup =5, 
        adherence_counseling = 55, tracking_tracing = 40, anc_vl_testing = 68, pnc_vl_testing = 0,
        cd4_testing = 92, ahd_package = 88
      )
      
      baseline <- default_baseline_interventions
      
      # Look up this country's row in the baseline CSV (if provided)
      b_row <- if (!is.null(baseline_csv) && nrow(baseline_csv) > 0)
        baseline_csv[baseline_csv$country == country_name, ] else NULL
      
      # Read country-specific yield multipliers from baseline CSV.
      # Column naming convention: yield_mult_{intervention_key}
      # Falls back to an empty list if baseline CSV is absent or country not found.
      yield_multiplier_keys <- c("test_facility_general", "test_network", "test_index",
                                 "test_community", "test_kpsti", "hivst_facility", "hivst_community")
      context$yield_multipliers <- if (!is.null(b_row) && nrow(b_row) == 1) {
        mults <- lapply(yield_multiplier_keys, function(k) {
          val <- b_row[[paste0("yield_mult_", k)]]
          if (!is.null(val) && !is.na(val)) as.numeric(val) else NULL
        })
        names(mults) <- yield_multiplier_keys
        mults
      } else {
        list()
      }
      
      for (int_name in names(default_baseline_interventions)) {
        # Pull from baseline CSV first; fall back to country CSV for non-testing fields
        csv_val <- if (!is.null(b_row) && nrow(b_row) == 1 && int_name %in% names(b_row))
          b_row[[int_name]] else row[[int_name]]
        src_names <- if (!is.null(b_row) && nrow(b_row) == 1) names(b_row) else names(row)
        
        # Only override when the source has an explicit non-NA value
        if (int_name %in% src_names && !is.null(csv_val) && !is.na(csv_val)) {
          baseline[[int_name]] <- csv_val
        }
      }
      
      presets[[country_name]] <- list(
        description = paste("Country data for", country_name),
        context = context,
        baseline = baseline
      )
    }
  }
  
  # # Add Custom Country option
  # custom_context <- list(
  #   total_population = 1000000,
  #   hiv_prevalence = 0.05,
  #   plhiv = NULL,
  #   percent_diagnosed = 80,
  #   percent_on_art = 75,
  #   percent_suppressed = 85,
  #   new_infections_per_year = 5000,
  #   aids_deaths_per_year = 1000,
  #   birth_rate = 24,
  #   prop_pop_male = 49,
  #   prop_pop_under_14 = 40,
  #   anc_multiplier = 1     # Custom country: defaults to 1 (no ANC adjustment)
  # )
  # 
  # custom_pops <- calculate_populations(custom_context)
  # 
  # custom_baseline <- list(
  #   prep_oral = 0.01*custom_pops$total, 
  #   prep_lenacapavir = 0, 
  #   vmmc = 0.01*custom_pops$uncircumcised_males,
  #   condoms = 0.6*custom_pops$total, 
  #   infant_prophylaxis = 70,
  #   test_facility_general = 0.05*custom_pops$adult_pop, 
  #   test_network = 0.005*custom_pops$adult_pop, 
  #   test_index = 0.005*custom_pops$adult_pop, 
  #   test_community = 0.04*custom_pops$adult_pop,
  #   test_kpsti = 0.02*custom_pops$adult_pop, 
  #   hivst_facility = 0.02*custom_pops$adult_pop, 
  #   hivst_community = 0.01*custom_pops$adult_pop,
  #   eid = 75, 
  #   anc_hiv_testing = 88, 
  #   pnc_hiv_testing = 70,
  #   vl_monitoring_routine = 60, 
  #   mmd_3month = 40, 
  #   mmd_6month = 20, 
  #   mmd_12month = 5,
  #   adherence_counseling = 55, 
  #   tracking_tracing = 40, 
  #   anc_vl_testing = 68,
  #   pnc_vl_testing = 0,
  #   cd4_testing = 92, 
  #   ahd_package = 88
  # )
  # 
  # presets[["Custom Country"]] <- list(
  #   description = "Enter your own parameters",
  #   context = custom_context,
  #   baseline = custom_baseline
  # )
  # 
  return(presets)
}

# ============================================================================
# STRATIFIED FORCE-OF-INFECTION (FOI) MODULE
# ============================================================================
#
# CONCEPTUAL MODEL:
#   New infections arise from four distinct risk strata, each with its own
#   transmission rate (β) calibrated to the country baseline. Interventions act by:
#     (a) reducing INFECTIOUS PRESSURE (viral suppression → fewer unsuppressed PLHIV)
#     (b) reducing SUSCEPTIBLE POOL or per-contact risk within a stratum
#         (PrEP, condoms, VMMC)
#
# STRATA:
#   1. High-risk (KP, high-concurrency partners)    ~% of HIV-negative sexually active
#   2. General female                                ~% of HIV-negative sexually active
#   3. General uncircumcised male                    variable by country
#   4. General circumcised male                      variable by country (lower β)
#
# CALIBRATION:
#   β for each stratum is back-calculated so the model exactly reproduces
#   observed new_infections_per_year at baseline.
# ============================================================================

# ----------------------------------------------------------------------------
# STEP 1: DEFINE STRATUM PARAMETERS
# ----------------------------------------------------------------------------
define_strata_params <- function(context = NULL) {
  prop_high_risk      <- if (!is.null(context$prop_high_risk))      context$prop_high_risk      else hiv_params$default_prop_high_risk
  rr_high             <- if (!is.null(context$rr_high))             context$rr_high             else hiv_params$default_rr_high
  prop_male_general   <- if (!is.null(context$prop_pop_male))       context$prop_pop_male / 100 else hiv_params$default_prop_pop_male
  circ_prevalence     <- if (!is.null(context$circ_prevalence))     context$circ_prevalence/100 else hiv_params$default_circ_prevalence
  # VMMC risk reduction: prefer explicit context override, otherwise route
  # from the VMMC intervention's efficacy field (single source of truth with
  # the intervention_params Excel sheet). Hard-coded 0.60 is a final fallback.
  vmmc_risk_reduction <- if (!is.null(context$vmmc_risk_reduction)) {
    context$vmmc_risk_reduction
  } else if (exists("intervention_groups") &&
             !is.null(intervention_groups$prevention$interventions$vmmc$efficacy) &&
             !is.na(intervention_groups$prevention$interventions$vmmc$efficacy)) {
    intervention_groups$prevention$interventions$vmmc$efficacy
  }
  
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
partition_into_strata <- function(populations, strata_params) {
  hiv_neg_active <- populations$sexually_active_negative
  
  n_high_risk           <- hiv_neg_active * strata_params$prop_high_risk
  n_general             <- hiv_neg_active * strata_params$prop_general
  n_general_male        <- n_general * strata_params$prop_male_general
  n_general_male_uncirc <- n_general_male * (1 - strata_params$circ_prevalence)
  n_general_male_circ   <- n_general_male * strata_params$circ_prevalence
  n_general_female      <- n_general * (1 - strata_params$prop_male_general)
  
  # Total unsuppressed = on ART not suppressed + diagnosed not on ART + undiagnosed
  n_unsuppressed <- populations$unsuppressed +
    populations$diagnosed_not_on_art +
    populations$undiagnosed
  
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
calibrate_beta <- function(context, populations, strata, strata_params,
                           baseline_prev_adj = NULL) {
  # baseline_prev_adj: output of compute_prevention_adjustments() for baseline
  # interventions. When provided, β is calibrated as a BIOLOGICAL rate so that:
  #   FOI(biological_β, baseline_prevention) = new_infections_per_year
  # This means the same baseline prevention inputs reproduce the observed count
  # exactly, while scale-up/down scenarios deviate correctly from it.
  # When NULL (backward-compatible default), β absorbs prevention implicitly.
  
  observed_infections <- context$new_infections_per_year
  infectious_pressure <- strata$n_unsuppressed / populations$total
  
  # Effective susceptible counts after baseline prevention
  if (!is.null(baseline_prev_adj)) {
    n_newly_circ_base <- baseline_prev_adj$vmmc_coverage_frac * strata$n_general_male_uncirc
    eff_high   <- strata$n_high_risk            * (1 - baseline_prev_adj$protection_high)
    eff_genfem <- strata$n_general_female       * (1 - baseline_prev_adj$protection_gen_female)
    eff_uncirc <- (strata$n_general_male_uncirc - n_newly_circ_base) *
      (1 - baseline_prev_adj$protection_gen_male_unc)
    eff_circ   <- (strata$n_general_male_circ + n_newly_circ_base) *
      (1 - baseline_prev_adj$protection_gen_male_circ)
  } else {
    eff_high   <- strata$n_high_risk
    eff_genfem <- strata$n_general_female
    eff_uncirc <- strata$n_general_male_uncirc
    eff_circ   <- strata$n_general_male_circ
  }
  
  w_high            <- strata_params$rr_high
  w_gen_female      <- 1.0
  w_gen_male_uncirc <- 1.0
  w_gen_male_circ   <- 1.0 - strata_params$vmmc_risk_reduction
  
  weighted_high          <- w_high            * eff_high
  weighted_gen_female    <- w_gen_female      * eff_genfem
  weighted_gen_male_unc  <- w_gen_male_uncirc * eff_uncirc
  weighted_gen_male_circ <- w_gen_male_circ   * eff_circ
  
  total_weight <- weighted_high + weighted_gen_female +
    weighted_gen_male_unc + weighted_gen_male_circ
  
  frac_high          <- weighted_high          / total_weight
  frac_gen_female    <- weighted_gen_female    / total_weight
  frac_gen_male_unc  <- weighted_gen_male_unc  / total_weight
  frac_gen_male_circ <- weighted_gen_male_circ / total_weight
  
  inf_high          <- observed_infections * frac_high
  inf_gen_female    <- observed_infections * frac_gen_female
  inf_gen_male_unc  <- observed_infections * frac_gen_male_unc
  inf_gen_male_circ <- observed_infections * frac_gen_male_circ
  
  safe_beta <- function(inf, pressure, n) {
    if (is.null(n) || n == 0 || pressure == 0) return(0)
    inf / (pressure * n)
  }
  
  list(
    beta_high          = safe_beta(inf_high,          infectious_pressure, eff_high),
    beta_gen_female    = safe_beta(inf_gen_female,    infectious_pressure, eff_genfem),
    beta_gen_male_unc  = safe_beta(inf_gen_male_unc,  infectious_pressure, eff_uncirc),
    beta_gen_male_circ = safe_beta(inf_gen_male_circ, infectious_pressure, eff_circ),
    frac_high          = frac_high,
    frac_gen           = 1 - frac_high,
    baseline_infections_check = observed_infections
  )
}

# ----------------------------------------------------------------------------
# STEP 4: COMPUTE PREVENTION COVERAGE ADJUSTMENTS
# ----------------------------------------------------------------------------
# ----------------------------------------------------------------------------
# Allocate PrEP supply across high-risk and general strata with a fold-rate
# advantage to high-risk, with spillover to general when high-risk saturates.
#
#   prep_high_risk_fold (k): per-capita coverage in high-risk is k× general
#
#   Case A (no saturation):       c_G = T / (k*H + G);     c_H = k * c_G
#   Case B (high-risk saturates): c_H = 1; c_G = (T - H) / G
#   Case C (both saturate):       c_H = 1; c_G = 1
#
# Threshold T(B) when c_H -> 1 in Case A: T = H + G/k.
# Threshold T(C) when c_G -> 1 in Case B: T = H + G.
#
# Returns: list(cov_high, cov_gen) — per-capita coverages, each in [0, 1].
# Used independently for prep_oral and prep_lenacapavir.
# ----------------------------------------------------------------------------
allocate_prep_coverage <- function(total_units, n_high_risk, n_general, fold) {
  if (is.null(total_units) || total_units <= 0 || (n_high_risk + n_general) <= 0) {
    return(list(cov_high = 0, cov_gen = 0))
  }
  H <- max(n_high_risk, 0)
  G <- max(n_general,   0)
  k <- max(fold, 1)  # fold < 1 would invert prioritisation; clamp at 1
  
  threshold_no_sat   <- H + G / k          # T at which c_H hits 1
  threshold_full_sat <- H + G              # T at which both hit 1
  
  if (total_units <= threshold_no_sat) {
    cov_gen  <- total_units / (k * H + G)
    cov_high <- k * cov_gen
  } else if (total_units <= threshold_full_sat) {
    cov_high <- 1
    cov_gen  <- (total_units - H) / max(G, 1)
  } else {
    cov_high <- 1
    cov_gen  <- 1
  }
  list(cov_high = min(cov_high, 1), cov_gen = min(cov_gen, 1))
}

# Efficacies passed in via scenario_interventions$eff_* keys so they stay
# in sync with intervention_params (set in calculate_scenario_outcomes before calling).
# Multiplicative stacking prevents double-counting when interventions overlap.
compute_prevention_adjustments <- function(scenario_interventions, strata, populations, strata_params) {
  clip <- function(x) max(0, min(1, x))
  
  # Pull efficacies — with fallbacks if not supplied
  eff_prep_oral <- scenario_interventions$eff_prep_oral %||% 0.99
  eff_prep_len  <- scenario_interventions$eff_prep_len  %||% 1.00
  eff_condom    <- scenario_interventions$eff_condom    %||% 0.80
  
  # Behavioural condom parameters
  # acts_per_year: converts condoms distributed → people with consistent annual coverage
  # condom_use_rate: fraction of sex acts where someone *with access* actually uses a condom
  # High-risk group has higher use rate (targeted programmes, stronger motivation)
  acts_per_year_high        <- scenario_interventions$acts_per_year_high   %||% 100
  acts_per_year_gen         <- scenario_interventions$acts_per_year_gen    %||% 50
  condom_use_rate_high      <- scenario_interventions$condom_use_rate_high %||% 0.75
  condom_use_rate_gen       <- scenario_interventions$condom_use_rate_gen  %||% 0.55
  
  # ---- Demand-weighted condom allocation ----------------------------------------
  # Condoms are allocated proportionally to total sex acts per group, not
  # population share. This ensures the full distributed stock is consumed and
  # the high-risk group — which has more acts per person — receives a
  # correspondingly larger share before the general population is served.
  #
  # Per-person coverage within each group:
  #   condom_cov = (condoms_allocated_to_group / acts_per_year) * condom_use_rate
  #             = (total_condoms * group_acts_share / acts_per_year) * condom_use_rate
  #             = total_condoms * condom_use_rate / total_acts
  #
  # All three general sub-strata (female, uncirc male, circ male) share the
  # same acts_per_year_gen, so they all receive the same per-person coverage.
  # -------------------------------------------------------------------------------
  total_condoms   <- scenario_interventions$condoms %||% 0
  acts_high_total <- strata$n_high_risk * acts_per_year_high
  acts_gen_total  <- strata$n_general   * acts_per_year_gen
  total_acts      <- max(acts_high_total + acts_gen_total, 1)
  
  condom_cov_high <- clip(total_condoms * condom_use_rate_high / total_acts)
  condom_cov_gen  <- clip(total_condoms * condom_use_rate_gen  / total_acts)
  
  # ---- PrEP allocation across high-risk + general ----
  # k-fold per-capita advantage to high-risk. Both prep_oral and
  # prep_lenacapavir are allocated INDEPENDENTLY using the same fold parameter.
  # Surplus beyond high-risk capacity spills to general (see allocate_prep_coverage).
  prep_fold <- hiv_params$prep_high_risk_fold %||% 3
  
  prep_oral_alloc <- allocate_prep_coverage(
    total_units = scenario_interventions$prep_oral %||% 0,
    n_high_risk = strata$n_high_risk,
    n_general   = strata$n_general,
    fold        = prep_fold
  )
  prep_len_alloc <- allocate_prep_coverage(
    total_units = scenario_interventions$prep_lenacapavir %||% 0,
    n_high_risk = strata$n_high_risk,
    n_general   = strata$n_general,
    fold        = prep_fold
  )
  
  prep_oral_cov_high <- prep_oral_alloc$cov_high
  prep_oral_cov_gen  <- prep_oral_alloc$cov_gen
  prep_len_cov_high  <- prep_len_alloc$cov_high
  prep_len_cov_gen   <- prep_len_alloc$cov_gen
  
  # ---- High-risk stratum: PrEP (oral + LEN) + condoms ----
  residual_high   <- (1 - prep_oral_cov_high * eff_prep_oral) *
    (1 - prep_len_cov_high  * eff_prep_len)  *
    (1 - condom_cov_high    * eff_condom)
  protection_high <- 1 - residual_high
  
  # ---- General female: PrEP + condoms ----
  residual_gen_female   <- (1 - prep_oral_cov_gen * eff_prep_oral) *
    (1 - prep_len_cov_gen  * eff_prep_len)  *
    (1 - condom_cov_gen    * eff_condom)
  protection_gen_female <- 1 - residual_gen_female
  
  # ---- General uncircumcised male: PrEP + condoms ----
  residual_gen_male_unc   <- (1 - prep_oral_cov_gen * eff_prep_oral) *
    (1 - prep_len_cov_gen  * eff_prep_len)  *
    (1 - condom_cov_gen    * eff_condom)
  protection_gen_male_unc <- 1 - residual_gen_male_unc
  
  # ---- General circumcised male: PrEP + condoms ----
  # Circumcised men also use PrEP/condoms; this stacks ON TOP of their lower β_circ
  # (which encodes biological circumcision protection only).
  # Without this, men transferred from the uncirc pool by VMMC lose their
  # protection coverage and can appear to gain MORE infections — fixed here.
  residual_gen_male_circ   <- (1 - prep_oral_cov_gen * eff_prep_oral) *
    (1 - prep_len_cov_gen  * eff_prep_len)  *
    (1 - condom_cov_gen    * eff_condom)
  protection_gen_male_circ <- 1 - residual_gen_male_circ
  
  # ---- VMMC: converts uncirc men → circ pool (not a coverage multiplier) ----
  newly_circumcised  <- min(scenario_interventions$vmmc %||% 0, strata$n_general_male_uncirc)
  vmmc_coverage_frac <- clip(newly_circumcised / max(strata$n_general_male_uncirc, 1))
  
  list(
    protection_high          = protection_high,
    protection_gen_female    = protection_gen_female,
    protection_gen_male_unc  = protection_gen_male_unc,
    protection_gen_male_circ = protection_gen_male_circ,
    vmmc_coverage_frac       = vmmc_coverage_frac
  )
}

# ----------------------------------------------------------------------------
# STEP 5: MAIN FOI FUNCTION
# ----------------------------------------------------------------------------
estimate_new_infections_foi <- function(context,
                                        populations,
                                        scenario_interventions,
                                        suppression_delta = 0,
                                        strata_params = NULL,
                                        baseline_interventions = NULL) {
  if (is.null(strata_params)) strata_params <- define_strata_params(context)
  
  strata <- partition_into_strata(populations, strata_params)
  
  # Compute baseline prevention adjustments so calibrate_beta can derive a
  # biological β that, when combined with baseline prevention, reproduces
  # new_infections_per_year exactly.
  baseline_prev_adj <- if (!is.null(baseline_interventions)) {
    compute_prevention_adjustments(baseline_interventions, strata, populations, strata_params)
  } else NULL
  
  betas  <- calibrate_beta(context, populations, strata, strata_params, baseline_prev_adj)
  
  # Adjust unsuppressed pool for treatment-side changes in this scenario
  n_unsuppressed_scenario      <- max(0, strata$n_unsuppressed - suppression_delta)
  infectious_pressure_scenario <- n_unsuppressed_scenario / populations$total
  
  prev_adj <- compute_prevention_adjustments(scenario_interventions, strata, populations, strata_params)
  
  
  # VMMC shifts men from uncirc → circ pool
  n_newly_circ <- prev_adj$vmmc_coverage_frac * strata$n_general_male_uncirc
  n_uncirc_eff <- strata$n_general_male_uncirc - n_newly_circ
  n_circ_eff   <- strata$n_general_male_circ   + n_newly_circ
  
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
  
  # Circumcised men: lower β encodes biological circumcision protection;
  # condom coverage applied on top via protection_gen_male_circ
  infections_gen_male_circ <- betas$beta_gen_male_circ *
    infectious_pressure_scenario *
    n_circ_eff *
    (1 - prev_adj$protection_gen_male_circ)
  
  total_new_infections <- max(0,
                              infections_high + infections_gen_female + infections_gen_male_unc + infections_gen_male_circ)
  
  list(
    new_infections     = round(total_new_infections),
    infections_averted = round(max(0, context$new_infections_per_year - total_new_infections)),
    by_stratum = list(
      high_risk       = round(max(0, infections_high)),
      gen_female      = round(max(0, infections_gen_female)),
      gen_male_uncirc = round(max(0, infections_gen_male_unc)),
      gen_male_circ   = round(max(0, infections_gen_male_circ))
    ),
    diagnostics = list(
      beta_high                    = betas$beta_high,
      beta_gen_female              = betas$beta_gen_female,
      beta_gen_male_unc            = betas$beta_gen_male_unc,
      beta_gen_male_circ           = betas$beta_gen_male_circ,
      frac_infections_high_risk    = betas$frac_high,
      n_unsuppressed_baseline      = strata$n_unsuppressed,
      n_unsuppressed_scenario      = n_unsuppressed_scenario,
      infectious_pressure_baseline = strata$n_unsuppressed / populations$total,
      infectious_pressure_scenario = infectious_pressure_scenario,
      vmmc_newly_circumcised       = round(n_newly_circ)
    )
  )
}

# ----------------------------------------------------------------------------
# STEP 6: CALIBRATION VALIDATION
# ----------------------------------------------------------------------------
validate_calibration <- function(context, populations, betas, strata, strata_params) {
  flags <- character(0)
  
  # Upper bounds widened (May 2026) to accommodate the range of β values
  # produced by legitimate high-burden / low-suppression / peak-incidence
  # epidemic settings in SSA. Derivation:
  #   β ≈ stratum_incidence / (n_unsuppressed / total_pop)
  # Empirical incidence references (sub-Saharan Africa):
  #   - FSW: median 4.3/100py, peaks to 15+ (Joshi 2023, Sauti Tanzania 8.6)
  #   - MSM: 1.0–15.4/100py (Joshi 2021 review)
  #   - General female: 0.2–4.9/100py (SSA cohorts; high-burden peaks)
  #   - General male: ~0.7× female (median F:M IRR 1.47)
  # Upper bounds chosen so a worst-case epidemic (~25% prevalence, ~3% pressure,
  # KP incidence ~15%/yr, female incidence ~3%/yr) does not falsely flag.
  bounds <- list(
    beta_high          = list(lower = 0.05,  upper = 5.00, label = "High-risk (KP)"),
    beta_gen_female    = list(lower = 0.005, upper = 1.50, label = "General (female)"),
    beta_gen_male_unc  = list(lower = 0.003, upper = 1.20, label = "General (uncircumcised male)"),
    beta_gen_male_circ = list(lower = 0.001, upper = 0.60, label = "General (circumcised male)")
  )
  
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
      flags <<- c(flags, sprintf("%s: β = %.4f (%s; plausible range %.3f–%.2f)",
                                 bound$label, b, direction, bound$lower, bound$upper))
    }
    data.frame(stratum = bound$label, beta = round(b, 5),
               lower = bound$lower, upper = bound$upper, pass = pass,
               stringsAsFactors = FALSE)
  }))
  
  # Incidence computed against sexually_active_negative — the population the
  # FOI model actually operates on (60% of hiv_negative). Using hiv_negative
  # inflates the denominator ~1.67x and causes false low-incidence flags in
  # lower-burden countries. Threshold lowered to 0.01% to catch only genuine
  # data mismatches rather than legitimate low-incidence epidemics.
  obs_incidence <- context$new_infections_per_year / max(populations$sexually_active_negative, 1)
  incidence_pct <- round(obs_incidence * 100, 3)
  
  if (obs_incidence > 0.05)
    flags <- c(flags, sprintf("Implied annual incidence = %.2f%% among sexually active HIV-negative adults — unusually high (>5%%). Check new_infections_per_year and prevalence inputs.", incidence_pct))
  else if (obs_incidence < 0.0001)
    flags <- c(flags, sprintf("Implied annual incidence = %.4f%% among sexually active HIV-negative adults — very low (<0.01%%). Check new_infections_per_year input.", incidence_pct))
  
  ratio_inf_to_unsup <- context$new_infections_per_year / max(strata$n_unsuppressed, 1)
  if (ratio_inf_to_unsup > 0.5)
    flags <- c(flags, sprintf("new_infections / unsuppressed_PLHIV = %.2f — implies implausibly high per-person transmission. Check suppression rate or infection counts.", ratio_inf_to_unsup))
  
  if (betas$frac_high > 0.80 && strata_params$prop_high_risk < 0.10)
    flags <- c(flags, sprintf("%.0f%% of infections attributed to high-risk stratum (%.0f%% of population). Consider adjusting prop_high_risk or rr_high.", betas$frac_high * 100, strata_params$prop_high_risk * 100))
  
  n_flags   <- length(flags)
  narrative <- if (n_flags == 0) {
    sprintf("Calibration passed. Implied annual incidence: %.3f%% among HIV-negative adults. High-risk stratum accounts for %.0f%% of baseline infections. All β values within plausible bounds.",
            incidence_pct, betas$frac_high * 100)
  } else {
    sprintf("%d calibration warning(s). Implied incidence: %.3f%%. Review flagged parameters before interpreting results.",
            n_flags, incidence_pct)
  }
  
  list(valid = (n_flags == 0), flags = flags, beta_table = beta_table,
       incidence_check = list(
         observed_rate_pct      = incidence_pct,
         new_infections         = context$new_infections_per_year,
         sexually_active_neg    = round(populations$sexually_active_negative),
         n_unsuppressed         = round(strata$n_unsuppressed),
         ratio_inf_unsup        = round(ratio_inf_to_unsup, 3)
       ),
       narrative = narrative)
}

# ----------------------------------------------------------------------------
# CONVENIENCE WRAPPER + SHINY UI PANEL
# ----------------------------------------------------------------------------
estimate_and_validate <- function(context, populations, scenario_interventions,
                                  suppression_delta = 0, strata_params = NULL) {
  if (is.null(strata_params)) strata_params <- define_strata_params(context)
  strata     <- partition_into_strata(populations, strata_params)
  betas      <- calibrate_beta(context, populations, strata, strata_params)
  validation <- validate_calibration(context, populations, betas, strata, strata_params)
  foi_result <- estimate_new_infections_foi(context, populations, scenario_interventions,
                                            suppression_delta, strata_params)
  foi_result$validation <- validation
  foi_result
}

render_calibration_panel <- function(foi_result) {
  v           <- foi_result$validation
  panel_class <- if (v$valid) "alert alert-success" else "alert alert-warning"
  icon_html   <- if (v$valid) "\u2713 " else "\u26A0\uFE0F "
  
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
        strong(paste0(icon_html, "Calibration Check")), p(v$narrative)),
    if (!v$valid)
      div(class = "alert alert-warning",
          strong("Specific warnings:"), tags$ul(lapply(v$flags, tags$li))),
    tags$table(
      class = "table table-sm table-bordered",
      style = "font-size: 0.85em; margin-top: 10px;",
      tags$thead(tags$tr(tags$th("Stratum"), tags$th("\u03B2 (calibrated)"),
                         tags$th("Plausible range"), tags$th("Status"))),
      tags$tbody(beta_rows)
    ),
    div(style = "font-size: 0.85em; color: #555; margin-top: 8px;",
        sprintf("Implied incidence: %.3f%% | Unsuppressed PLHIV: %s | Infections/unsuppressed ratio: %.3f",
                v$incidence_check$observed_rate_pct,
                format(v$incidence_check$n_unsuppressed, big.mark = ","),
                v$incidence_check$ratio_inf_unsup))
  )
}

# ============================================================================
# SCENARIO OUTCOMES CALCULATION - ABSOLUTE VALUES
# ============================================================================
calculate_scenario_outcomes <- function(context, interventions, populations,
                                        is_baseline                  = FALSE,
                                        baseline_interventions        = NULL,
                                        baseline_additional_suppressed = 0,
                                        baseline_end_suppressed       = NULL,
                                        mortality_calibration_factor   = NULL) {
  
  # Initialize outcome counters
  infections_averted <- 0
  infant_infections_averted <- 0
  positive_tests <- 0
  new_diagnoses <- 0
  re_engagement <- 0
  re_engagement_testing <- 0
  additional_suppressed <- 0
  additional_suppressed_testing <- 0
  art_initiations <- 0
  art_inititations_testing <- 0
  # Per-component linkage accumulators (testing modalities only).
  # Split into new-diagnosis and retest-positive components because each is
  # capped independently downstream (new_diagnoses_cap_prop vs
  # testing_reengagement_cap). Post-loop, each component is scaled by its
  # cap's shrinkage ratio (1 if cap doesn't bind), preserving per-modality
  # cost attribution in the uncapped case and applying proportional scaling
  # under capping.
  #
  #   *_linked_uncapped       = Σ_i (volume_i × linkage_rate_i)
  #   *_linkage_cost_uncapped = Σ_i (volume_i × linkage_cost_i)             # per positive, NOT per linked
  #   *_supp_uncapped         = Σ_i (volume_i × linkage_rate_i × supp_rate_i)
  #
  # The supp accumulator carries the per-source suppression rate (general
  # testing uses testing_art_init_supp; PMTCT uses pmtct_cascade_supp_rate)
  # so the post-loop application doesn't have to know which source any
  # given linked patient came from.
  #
  # Under capping, the same shrinkage ratio scales linked count, cost, and
  # suppression together, preserving the per-modality mix.
  new_dx_linked_uncapped             <- 0
  new_dx_linkage_cost_uncapped       <- 0
  new_dx_supp_uncapped               <- 0
  retest_pos_linked_uncapped         <- 0
  retest_pos_linkage_cost_uncapped   <- 0
  retest_pos_supp_uncapped           <- 0
  # PMTCT / infant cascade trackers
  pmtct_new_diagnoses    <- 0   # HIV+ pregnant women newly diagnosed via ANC/PNC -> PMTCT ART
  infant_prophy_cov_frac <- 0   # efficacy-weighted infant prophylaxis coverage (0-1)
  anc_vl_reached_preg    <- 0   # pregnant women on ART reached by ANC VL monitoring
  pnc_vl_reached_preg    <- 0   # postpartum women on ART reached by PNC VL monitoring
  eid_infants_reached    <- 0   # HIV-exposed infants reached by EID
  # Retention: two distinct counters replacing the old single retention_improvement
  # ltfu_retained_frac: cumulative fraction of at-risk people retained, built
  #   multiplicatively across prevention interventions so the same person cannot
  #   be counted twice (MMD + adherence counseling acting on the same pool)
  ltfu_retained_frac <- 0  # grows as: 1 - prod(1 - coverage_i * efficacy_i)
  ltfu_prevented     <- 0  # converted to people after the loop (ltfu_new * ltfu_retained_frac)
  ltfu_reengaged     <- 0  # LTFU people brought back by tracking/tracing
  # Tracking/tracing is DEFERRED — its true eligible pool is prevalent LTFU +
  # net incident LTFU (after prevention), which is not yet known here.
  # Captured during the intervention loop, applied after ltfu_new_effective resolves.
  deferred_tracking_coverage <- 0  # coverage fraction (0-1) entered by user
  deferred_tracking_efficacy <- 0  # efficacy parameter from intervention spec
  deferred_tracking_unit_cost <- 0 # unit cost for cost calculation against full pool
  total_intervention_cost <- 0
  dsd_cost_adjustment <- 0
  dsd_bundle_done     <- FALSE  # one-shot guard for the DSD bundle calculation
  # (MMD-3/6/12 + community pickup are processed
  # jointly on the first DSD key encountered)
  tests_performed <- 0
  
  # Base test yield: use country prior-year average from CSV if available;
  # fall back to dynamic estimate from undiagnosed + LTFU pools otherwise.
  if (!is.null(context$test_yield) && !is.na(context$test_yield)) {
    base_test_yield <- context$test_yield
  } else {
    base_test_yield <- (populations$undiagnosed + populations$ltfu) / populations$sexually_active
    base_test_yield <- min(base_test_yield, 0.1)  # Cap at 10% positivity for realism
  }
  
  prop_reeng <- if (!is.null(context$prop_retesting) && !is.na(context$prop_retesting)) {
    context$prop_retesting
  } else {
    hiv_params$prop_retest_default
  }
  prop_new_dx <- 1 - prop_reeng
  
  # Positive retests now flow as a single re-engagement candidate pool.
  # The testing re-engagement cap (testing_reengagement_cap_frac) limits how
  # many can actually re-engage via testing; anything beyond the cap is
  # implicitly a no-op (cost still accrues via tests_performed, no cascade
  # movement). Tracking and spontaneous re-engagement flow without an
  # additional cap — they are bounded by their own intervention parameters
  # and the natural spontaneous rate, respectively. Previously the model
  # further split retests into "already-on-ART no-op" (prop_retest_already_on_art)
  # and "active" sub-pools (LTFU vs never_linked via prevalent_ltfu_frac),
  # but these splits were not empirically grounded.
  
  # `average_linkage_cap` removed: per-intervention linkage_rate values are
  # now the authoritative source for linkage. The volume-weighted average is
  # computed in the post-loop block (search for `L_avg`). Parameter validation
  # (linkage_rate <= 1) should be enforced at the parameter-loading stage.
  
  # Flatten intervention structure
  all_interventions <- list()
  for (group_name in names(intervention_groups)) {
    group <- intervention_groups[[group_name]]
    for (int_name in names(group$interventions)) {
      all_interventions[[int_name]] <- group$interventions[[int_name]]
    }
  }
  
  # ── VOLUME DILUTION: order-independent two-pass approach ─────────────────
  # When total planned tests exceed 100% of prior-year volume, positivity
  # drops because the easy-to-find positives are exhausted. Rather than
  # penalising whichever modalities happen to be processed last in the loop,
  # we compute a single global dilution factor from total planned volume and
  # apply it uniformly to every modality — preserving the relative advantage
  # of high-yield targeted testing regardless of loop order.
  #
  # Dilution factor derivation:
  #   Tests <= threshold : full yield
  #   Tests >  threshold : half yield
  #   Factor = (threshold + (total - threshold) * 0.5) / total
  #          = 1.0 when total <= threshold (no dilution)
  #
  # If prior_year_tests is not supplied, threshold = Inf and factor = 1.0.
  # ─────────────────────────────────────────────────────────────────────────
  volume_threshold <- if (!is.null(context$prior_year_tests) && !is.na(context$prior_year_tests))
    context$prior_year_tests * 1 else Inf
  
  # Pre-loop pass: sum total planned tests across ALL testing modalities,
  # including index testing — index now shares the saturation curve of general
  # testing and contributes to the dilution denominator.
  total_planned_tests <- 0
  for (int_key_pre in names(all_interventions)) {
    int_pre     <- all_interventions[[int_key_pre]]
    int_val_pre <- interventions[[int_key_pre]]
    if (is.null(int_val_pre) || int_val_pre == 0) next
    if (!("testing" %in% int_pre$outcomes)) next
    elig_pre <- populations[[int_pre$eligible_pop]] %||% 0
    n_pre <- if (int_pre$type == "coverage")
      min(elig_pre * (int_val_pre / 100), elig_pre)
    else
      min(int_val_pre, elig_pre)
    total_planned_tests <- total_planned_tests + n_pre
  }
  
  yield_dilution_factor <- if (is.infinite(volume_threshold) ||
                               total_planned_tests <= volume_threshold) {
    1.0
  } else {
    (volume_threshold + (total_planned_tests - volume_threshold) * 0.5) /
      total_planned_tests
  }
  
  # Process each intervention
  for (int_key in names(all_interventions)) {
    intervention <- all_interventions[[int_key]]
    intervention_value <- interventions[[int_key]]
    
    if (is.null(intervention_value)) intervention_value <- 0
    if (intervention_value == 0) next
    
    # Skip mortality interventions whose eligible population depends on
    # art_initiations — handled in second pass after that count is finalised
    if (intervention$eligible_pop %in% c("new_art_initiations", "on_art_total")) next
    
    # Get eligible population
    eligible <- populations[[intervention$eligible_pop]]
    if (is.null(eligible)) eligible <- 0
    
    # Calculate number reached
    number_reached <- intervention_value
    if (intervention$type == "coverage") {
      number_reached <- eligible * (intervention_value / 100)
    }
    
    # Cap at eligible population
    if (intervention$type == "absolute") {
      if (intervention_value >= eligible) {
        number_reached <- eligible
      }
    }
    number_reached <- min(number_reached, eligible)
    
    # Calculate outcomes based on intervention type
    if ("testing" %in% intervention$outcomes) {
      # All testing modalities (including index) share the same yield dilution
      # factor when total planned volume exceeds prior-year throughput.
      modality_dilution <- yield_dilution_factor
      
      # Effective yield: country-specific multiplier (from baseline CSV) takes
      # precedence; falls back to intervention params default, then 1.
      country_mult <- context$yield_multipliers[[int_key]]
      effective_yield <- base_test_yield *
        (if (!is.null(country_mult)) country_mult else 1)
      
      pos_tests <- number_reached * effective_yield * modality_dilution * intervention$efficacy
      positive_tests <- positive_tests + pos_tests
      tests_performed <- tests_performed + number_reached
      
      # Split positive tests two ways:
      #   1. new_dx     : first-time positive → new diagnosis → ART (from undiagnosed)
      #   2. retest_pos : previously diagnosed positive (regardless of ART status)
      #                   → re-engagement candidate. The LTFU recovery cap downstream
      #                   determines how many actually re-engage; the rest are
      #                   implicit no-ops (cost accrues, no cascade movement).
      new_dx     <- pos_tests * prop_new_dx
      retest_pos <- pos_tests * prop_reeng
      
      # ANC/PNC HIV testing: general yield path suppressed for cascade metrics.
      # These women are routed into the adult cascade after the loop via the
      # PMTCT-specific yield (pmtct_new_diagnoses), which is more accurate than
      # the background positivity rate for this targeted population.
      # tests_performed still accrues here — the test is administered to all
      # women reached. Unit cost accrues here; linkage cost accrues in the
      # post-loop PMTCT routing block using the PMTCT-specific diagnosed count.
      if (!(int_key %in% c("anc_hiv_testing", "pnc_hiv_testing"))) {
        new_diagnoses         <- new_diagnoses         + new_dx
        re_engagement_testing <- re_engagement_testing + retest_pos
        
        # Linkage accounting (proportional-scaling approach).
        # Accumulate per-component (new-dx vs retest-pos) linked counts,
        # linkage costs, and suppression contributions. Each component is
        # scaled post-loop by its own cap's shrinkage ratio:
        #   shrinkage_new      = new_diagnoses        / new_diagnoses_uncapped
        #   shrinkage_reengage = re_engagement_testing / re_engagement_testing_uncapped
        # When neither cap binds (the common case), both shrinkages = 1.0 and
        # the formula collapses to the original per-modality semantic:
        #   linked_total = Σ_i (new_dx_i + retest_pos_i) × linkage_rate_i
        #   cost_total   = Σ_i (new_dx_i + retest_pos_i) × linkage_cost_i    # per positive
        # When a cap binds, the corresponding component is scaled down,
        # preserving the per-modality mix.
        #
        # Replaces the previous in-loop `linked` and `art_inititations_testing`
        # accumulation, and the `average_linkage_cap` post-loop patch.
        linkage_rate <- intervention$linkage_rate
        # NOTE: linkage_cost is charged per POSITIVE result (cost-per-attempt
        # semantic), not per successfully linked patient. The linkage activity
        # (counseling, referral, escort, follow-up) is incurred for everyone
        # who tests positive, regardless of whether they ultimately link to
        # care. Linked-count and suppression contributions remain multiplied
        # by linkage_rate (only successful linkers reach the cascade).
        new_dx_linked_uncapped       <- new_dx_linked_uncapped +
          new_dx     * linkage_rate
        new_dx_linkage_cost_uncapped <- new_dx_linkage_cost_uncapped +
          new_dx     * intervention$linkage_cost
        new_dx_supp_uncapped         <- new_dx_supp_uncapped +
          new_dx     * linkage_rate * hiv_params$testing_art_init_supp
        retest_pos_linked_uncapped       <- retest_pos_linked_uncapped +
          retest_pos * linkage_rate
        retest_pos_linkage_cost_uncapped <- retest_pos_linkage_cost_uncapped +
          retest_pos * intervention$linkage_cost
        retest_pos_supp_uncapped         <- retest_pos_supp_uncapped +
          retest_pos * linkage_rate * hiv_params$testing_art_init_supp
        
        # Unit cost charged here (per test administered, including implicit
        # no-ops). Linkage cost is charged post-loop after caps are applied.
        # Country-specific unit cost override applied via context$cost_overrides_test;
        # falls back to intervention$unit_cost when no override is supplied for int_key.
        unit_cost_eff <- context$cost_overrides_test[[int_key]] %||% intervention$unit_cost
        total_intervention_cost <- total_intervention_cost +
          number_reached * unit_cost_eff
      } else {
        # ANC/PNC: unit cost per test only; linkage cost charged in post-loop block.
        # Country-specific override via context$cost_overrides_test (anc_hiv_testing,
        # pnc_hiv_testing). Falls back to intervention$unit_cost when absent.
        unit_cost_eff <- context$cost_overrides_test[[int_key]] %||% intervention$unit_cost
        total_intervention_cost <- total_intervention_cost +
          number_reached * unit_cost_eff
      }
      
      # ── ANC HIV testing: route newly diagnosed HIV+ pregnant women into PMTCT cascade ──
      # number_reached = pregnant_hiv_testable x (ANC_coverage/100), so coverage is already embedded.
      # Yield = proportion of testable women (HIV-neg + HIV+ undiagnosed) who are HIV+ undiagnosed.
      if (int_key == "anc_hiv_testing") {
        anc_hiv_yield       <- populations$pregnant_undiagnosed /
          max(populations$pregnant_hiv_testable, 1)
        pmtct_candidates    <- number_reached * anc_hiv_yield * intervention$efficacy
        pmtct_new_diagnoses <- pmtct_new_diagnoses +
          min(pmtct_candidates, populations$pregnant_undiagnosed)
        
        # ── PNC HIV testing: same pool but deduct HIV+ undiagnosed women already caught at ANC ──
        # HIV-negative women remain fully eligible; only the HIV+ undiagnosed pool is reduced.
      } else if (int_key == "pnc_hiv_testing") {
        remaining_undiagnosed_preg <- max(0, populations$pregnant_undiagnosed - pmtct_new_diagnoses)
        # PNC eligible = HIV-negative pregnant women (unchanged) + remaining HIV+ undiagnosed
        hiv_neg_pregnant   <- populations$pregnant_hiv_testable - populations$pregnant_undiagnosed
        pnc_eligible_pool  <- hiv_neg_pregnant + remaining_undiagnosed_preg
        # Override number_reached using the corrected PNC eligible pool
        number_reached     <- pnc_eligible_pool * (intervention_value / 100)
        pnc_hiv_yield      <- remaining_undiagnosed_preg / max(pnc_eligible_pool, 1)
        pmtct_candidates   <- number_reached * pnc_hiv_yield * intervention$efficacy
        pmtct_new_diagnoses <- pmtct_new_diagnoses +
          min(pmtct_candidates, remaining_undiagnosed_preg)
      }
      
    } else if ("infant_infections" %in% intervention$outcomes) {
      # Infant prophylaxis (NVP): accumulate efficacy-weighted coverage fraction.
      # Efficacy drawn from intervention CSV. Actual infection reduction calculated
      # in the MTCT cascade block below.
      infant_prophy_cov_frac <- min(1, infant_prophy_cov_frac +
                                      (number_reached / max(populations$hiv_exposed_infants, 1)) )
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
      
    } else if ("infant_diagnosis" %in% intervention$outcomes) {
      # EID: tests HIV-exposed infants to identify HIV+ infants for early ART initiation.
      # Cost calculated post-cascade using actual yield — see MTCT cascade block below.
      eid_infants_reached <- number_reached
      
    } else if ("viral_suppression" %in% intervention$outcomes) {
      # ── Routine VL monitoring: identification only, NO direct suppression effect ──
      # vl_monitoring_routine identifies unsuppressed patients but does not itself
      # convert them to suppressed. Its effect is realised downstream via Enhanced
      # Adherence Counselling (EAC), which acts on the VL-identified unsuppressed
      # pool. The VL test cost still applies here.
      if (int_key == "vl_monitoring_routine") {
        total_intervention_cost <- total_intervention_cost +
          number_reached * intervention$unit_cost
        next
      }
      
      # ── Enhanced Adherence Counselling (EAC) ──────────────────────────────
      # EAC acts ONLY on unsuppressed patients identified via VL monitoring.
      # Reach   = on_art × vl_coverage × unsuppressed_rate × eac_coverage
      # Effect  = reach × eac_efficacy        (added to additional_suppressed)
      # Cost    = reach × eac_unit_cost       (only those who actually receive EAC)
      # If vl_monitoring_routine coverage is 0, EAC has no eligible pool and
      # contributes nothing to suppression or cost.
      if (int_key == "adherence_counseling") {
        vl_cov_frac       <- (interventions$vl_monitoring_routine %||% 0) / 100
        eac_cov_frac      <- intervention_value / 100
        unsuppressed_rate <- 1 - context$percent_suppressed / 100
        
        eac_reach <- populations$on_art * vl_cov_frac * unsuppressed_rate * eac_cov_frac
        
        additional_suppressed <- additional_suppressed +
          eac_reach * intervention$efficacy
        
        total_intervention_cost <- total_intervention_cost +
          eac_reach * intervention$unit_cost
        
        next
      }
      
      # ── All other viral_suppression interventions (ANC/PNC VL testing, etc.) ──
      additional_suppressed <- additional_suppressed +
        number_reached * (1 - context$percent_suppressed / 100) * intervention$efficacy
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
      
      # ── ANC / PNC VL testing: track pregnant/postpartum women on ART reached for MTCT cascade ──
      if (int_key == "anc_vl_testing") {
        anc_vl_reached_preg <- anc_vl_reached_preg + number_reached
      } else if (int_key == "pnc_vl_testing") {
        pnc_vl_reached_preg <- pnc_vl_reached_preg + number_reached
      }
      
    } else if ("retention" %in% intervention$outcomes) {
      # ── Two distinct retention pathways ──────────────────────────────────
      # tracking_tracing (eligible_pop == "ltfu"): re-engages people already LTFU
      # MMD / DSD options (eligible_pop != "ltfu"): prevents people
      #   from becoming LTFU in the first place
      if (intervention$eligible_pop == "ltfu") {
        # DEFER: full eligible pool (prevalent + net incident LTFU) not yet known.
        # Capture inputs; apply after prevention loop resolves ltfu_new_effective.
        deferred_tracking_coverage <- intervention_value / 100
        deferred_tracking_efficacy <- intervention$efficacy
        deferred_tracking_unit_cost <- intervention$unit_cost
      } else {
        # Prevention interventions: COST applies to everyone reached (all on ART),
        # but EFFECT can only act on the at-risk fraction (those who would drop out).
        # Two combination rules depending on whether interventions can overlap:
        #
        # ADDITIVE — DSD options (eligible_pop == "on_art_stable"):
        #   mmd_3month / mmd_6month / mmd_12month are mutually exclusive
        #   (UI enforces c3+c6+c12 <= 100%) and add directly into
        #   ltfu_retained_frac. Community pickup is NOT additive — it is a
        #   delivery mode that OVERRIDES the MMD facility-pickup mode for a
        #   fraction `cpu` of MMD-enrolled clients, applied equally across
        #   the three MMD categories. The four interventions are therefore
        #   processed jointly as a bundle (see special-case below).
        #
        # MULTIPLICATIVE — overlapping interventions (eligible_pop == "on_art"):
        #   None currently — kept for future overlapping retention interventions
        #   that target the full on_art pool. Multiplicative combination prevents
        #   double-counting:
        #     marginal_retained = (1 - ltfu_retained_frac) * coverage_frac * efficacy
        # Denominator matches the eligible pool: DSD acts on stable patients
        # only, so coverage is the fraction of stable on-ART enrolled, not the
        # fraction of all on-ART. Using on_art here would shrink coverage_frac
        # by the stable share and, combined with the ltfu_new_stable multiplier
        # below, would let DSD coverage of stable patients spuriously reduce
        # unstable LTFU.
        if (intervention$eligible_pop == "on_art_stable") {
          # ── DSD bundle (MMD-3/6/12 + community pickup) ───────────────────
          # Resulting buckets, as fractions of on_art_stable:
          #   MMD-3 only : c3·(1-cpu)         eff = eff_3 ,  cost = uc_3
          #   MMD-6 only : c6·(1-cpu)         eff = eff_6 ,  cost = uc_6
          #   MMD-12 only: c12·(1-cpu)        eff = eff_12,  cost = uc_12
          #   Community  : (c3+c6+c12)·cpu    eff = eff_cpu, cost = uc_cpu
          #   No DSD     : 1 - (c3+c6+c12)    (no effect, no cost)
          #
          # NB: cpu > 0 with mmd_sum = 0 yields zero effect and zero cost
          # by design — community pickup is a delivery mode layered on MMD
          # enrolment, not a standalone DSD option.
          #
          # Processed on the first DSD/community key encountered in the
          # loop; subsequent keys are no-op'd via dsd_bundle_done.
          if (!isTRUE(dsd_bundle_done)) {
            stable_n <- populations$on_art_stable
            
            mmd3_def  <- all_interventions$mmd_3month
            mmd6_def  <- all_interventions$mmd_6month
            mmd12_def <- all_interventions$mmd_12month
            cpu_def   <- all_interventions$community_pickup
            
            c3   <- (interventions$mmd_3month       %||% 0) / 100
            c6   <- (interventions$mmd_6month       %||% 0) / 100
            c12  <- (interventions$mmd_12month      %||% 0) / 100
            cpu  <- (interventions$community_pickup %||% 0) / 100
            
            # Defensive caps (UI already enforces these). If mmd_sum > 1,
            # rescale c3/c6/c12 proportionally so the bucket fractions
            # remain non-negative and sum correctly.
            if ((c3 + c6 + c12) > 1) {
              scale <- 1 / (c3 + c6 + c12)
              c3 <- c3 * scale; c6 <- c6 * scale; c12 <- c12 * scale
            }
            mmd_sum <- c3 + c6 + c12
            cpu     <- min(cpu, 1)
            
            # Combined retained-fraction contribution.
            mmd_only_term <- (1 - cpu) * (
              c3  * (mmd3_def$efficacy  %||% 0) +
                c6  * (mmd6_def$efficacy  %||% 0) +
                c12 * (mmd12_def$efficacy %||% 0)
            )
            community_term <- cpu * mmd_sum * (cpu_def$efficacy %||% 0)
            ltfu_retained_frac <- ltfu_retained_frac + mmd_only_term + community_term
            
            # Combined cost adjustment.
            # unit_cost is interpreted as a FRACTIONAL change relative to the
            # country-specific art_cost_standard (negative = saving, positive =
            # premium). e.g. unit_cost = -0.08 -> DSD costs 8% less than facility
            # standard care for that person-year. Range trusted to Excel input.
            dsd_art_cost_unit <- context$art_cost_standard %||% ART_COST_STANDARD
            mmd_only_cost <- (1 - cpu) * stable_n * dsd_art_cost_unit * (
              c3  * (mmd3_def$unit_cost  %||% 0) +
                c6  * (mmd6_def$unit_cost  %||% 0) +
                c12 * (mmd12_def$unit_cost %||% 0)
            )
            community_cost <- cpu * mmd_sum * stable_n * dsd_art_cost_unit *
              (cpu_def$unit_cost %||% 0)
            dsd_cost_adjustment <- dsd_cost_adjustment + mmd_only_cost + community_cost
            
            dsd_bundle_done <- TRUE
          }
          # Skip the default per-intervention cost path below for DSD keys
          # (cost already applied above as part of the bundle).
          next
        } else {
          # Overlapping interventions (none currently): multiplicative
          coverage_frac <- ifelse(
            populations$on_art > 0,
            number_reached / populations$on_art,
            0
          )
          marginal_retained  <- (1 - ltfu_retained_frac) * coverage_frac * intervention$efficacy
          ltfu_retained_frac <- ltfu_retained_frac + marginal_retained
        }
      }
      
      # Cost: prevention interventions charged here against on_art-based reach.
      # Tracking/tracing is deferred — its cost is charged later against the
      # full LTFU pool (prevalent + net incident), matching the deferred reach.
      # DSD bundle (eligible_pop == "on_art_stable") handled above via `next`.
      if (intervention$eligible_pop != "ltfu" &&
          intervention$eligible_pop != "on_art_stable") {
        total_intervention_cost <- total_intervention_cost +
          number_reached * intervention$unit_cost
      }
      
    } else if ("ahd_screening" %in% intervention$outcomes) {
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
    }
  }
  
  # ========================================================================
  # PMTCT -> ADULT CASCADE ROUTING
  # Route PMTCT-diagnosed women into the adult cascade using the PMTCT-specific
  # yield and linkage rate, replacing the general yield path for ANC/PNC.
  # This ensures end_diagnosed, end_on_art, and end_suppressed reflect the
  # more accurate targeted yield rather than the background positivity rate.
  #
  # Linkage rate: uses anc_hiv_testing linkage rate for all PMTCT women,
  # consistent with the MTCT cascade step 2. ###UPDATE if ANC/PNC linkage
  # rates diverge substantially and warrant separate tracking.
  # Suppression discount: suppression rate of newly diagnosed pregnant women relatove to general pop
  # ========================================================================
  pmtct_cascade_linkage_rate <- all_interventions$anc_hiv_testing$linkage_rate %||% 0.85
  pmtct_cascade_supp_rate    <- (context$percent_suppressed / 100) * hiv_params$pmtct_cascade_supp_discount
  pmtct_cascade_linked_art   <- pmtct_new_diagnoses * pmtct_cascade_linkage_rate
  pmtct_cascade_linked_supp  <- pmtct_cascade_linked_art * pmtct_cascade_supp_rate
  pmtct_cascade_linkage_cost <- all_interventions$anc_hiv_testing$linkage_cost %||% 0
  
  # PMTCT-diagnosed women are routed through the SAME new-diagnosis pool as
  # general testing (so the new_diagnoses cap applies to both together).
  # Feed PMTCT contributions into the new-dx component accumulators so that
  # if the new_diagnoses cap binds, PMTCT linked count, linkage cost, and
  # suppression all scale by the same shrinkage_new factor as general testing.
  # The PMTCT-specific suppression rate (pmtct_cascade_supp_rate) and linkage
  # cost (anc_hiv_testing$linkage_cost) are baked into the supp/cost
  # accumulators here, mirroring the in-loop pattern for general modalities.
  # Linkage cost is charged on ALL PMTCT-diagnosed women (cost-per-attempt
  # semantic), consistent with the general-testing treatment above. Only the
  # linked count and suppression contribution apply the PMTCT linkage rate.
  new_diagnoses                <- new_diagnoses + pmtct_new_diagnoses
  new_dx_linked_uncapped       <- new_dx_linked_uncapped       + pmtct_cascade_linked_art
  new_dx_linkage_cost_uncapped <- new_dx_linkage_cost_uncapped +
    pmtct_new_diagnoses * pmtct_cascade_linkage_cost
  new_dx_supp_uncapped         <- new_dx_supp_uncapped         + pmtct_cascade_linked_supp
  # Note: art_inititations_testing, additional_suppressed_testing, and PMTCT
  # linkage cost are no longer added here. They are computed post-cap in the
  # ART INITIATIONS block below, after proportional scaling by shrinkage_new.
  
  # ========================================================================
  # APPLY CONSTRAINTS - CAP AT REALISTIC MAXIMUMS
  # ========================================================================
  
  # Cannot diagnose more people than 95% are undiagnosed.
  # Capture pre-cap value to compute shrinkage_new in the ART initiations
  # block below, so per-modality linked counts and costs scale proportionally
  # if the cap binds.
  new_diagnoses_uncapped <- new_diagnoses
  new_diagnoses <- min(new_diagnoses, populations$undiagnosed * hiv_params$new_diagnoses_cap_prop)
  
  # ── LTFU PREVENTION: convert retained fraction to people ─────────────────
  # ltfu_retained_frac is the combined fraction of the at-risk on-ART population
  # that prevention interventions retain. Currently only DSD interventions
  # contribute (all with eligible_pop == "on_art_stable"), so the retained
  # fraction reflects stable patients only and prevents stable LTFU only.
  ltfu_retained_frac <- min(ltfu_retained_frac, 1)  # cap at 100%
  
  # Convert retained fraction to people prevented from becoming LTFU.
  # Applied to ltfu_new_stable because all current retention interventions
  # have eligible_pop = "on_art_stable" — they cannot prevent unstable LTFU.
  # If future interventions target the unstable pool, this will need to split.
  ltfu_prevented          <- populations$ltfu_new_stable * ltfu_retained_frac
  ltfu_prevented_stable   <- ltfu_prevented
  ltfu_prevented_unstable <- 0
  
  # Suppression gain from retention: only the unsuppressed portion of retained
  # unstable patients can generate new suppressions — those already suppressed
  # are retained but add nothing to additional_suppressed.
  # Unsuppressed fraction assumed equal to the general on-ART unsuppressed rate.
  prop_unsuppressed_on_art <- ifelse(
    populations$on_art > 0,
    1 - (populations$suppressed / populations$on_art),
    0
  )
  additional_suppressed <- additional_suppressed +
    ltfu_prevented_unstable * prop_unsuppressed_on_art * RETENTION_SUPPRESSION_RATE
  
  # Net incident LTFU after prevention interventions, by stability status
  stable_ltfu    <- max(0, populations$ltfu_new_stable   - ltfu_prevented_stable)
  unstable_ltfu  <- max(0, populations$ltfu_new_unstable - ltfu_prevented_unstable)
  ltfu_new_effective <- stable_ltfu + unstable_ltfu
  
  # Full pool available for re-engagement = prevalent stock + net incident LTFU
  total_ltfu_pool <- populations$ltfu + ltfu_new_effective
  
  # ── DEFERRED TRACKING/TRACING APPLICATION ────────────────────────────────
  # Now that the full eligible pool is known (prevalent stock + net incident
  # LTFU after prevention), apply tracking/tracing reach and cost against it.
  # Matches the UI label "% of LTFU patients" — 40% means 40% of everyone
  # currently disengaged, not just last year's leftovers.
  tracking_reached <- total_ltfu_pool * deferred_tracking_coverage
  ltfu_reengaged   <- ltfu_reengaged + tracking_reached * deferred_tracking_efficacy
  total_intervention_cost <- total_intervention_cost +
    tracking_reached * deferred_tracking_unit_cost
  
  # ── SPONTANEOUS RE-ENGAGEMENT (computed FIRST) ───────────────────────────
  # Background return to care (silent transfers + self-initiated return) is
  # an epidemiological flow that occurs regardless of programmatic activity
  # — literature suggests ~12-15% of those classified as LTFU are silent
  # transfers (Chammartin et al. 2018, Tiendrebeogo et al. 2021) plus
  # self-initiated returners (Beres et al. 2020). It must therefore be
  # computed against the GROSS pool, not against whatever remains after
  # testing/tracking have drawn down. Otherwise, scaling testing down
  # mechanically inflates spontaneous re-engagement, producing the perverse
  # result that less testing → more people re-engaged.
  spontaneous_reengaged <- total_ltfu_pool * ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE
  
  # ── TESTING-DRIVEN RE-ENGAGEMENT CAP ─────────────────────────────────────
  # Testing volumes can mechanically generate more "positive retest" flow than
  # the LTFU pool can plausibly contain (e.g. Mozambique's 12M+ tests/year
  # produce ~285k positive retests against a 230k LTFU pool — physically
  # impossible to re-engage 124% of the pool via testing alone). This cap
  # bounds testing-driven re-engagement to a defensible fraction of the LTFU
  # pool annually.
  #
  # Note: this caps TESTING only. Tracking/tracing is bounded by its own
  # intervention parameters (coverage × efficacy, neither of which can exceed
  # the pool by construction). Spontaneous is bounded by its own rate (~10% of
  # pool). Combined total re-engagement is therefore implicitly limited to
  # spontaneous_rate + tracking_max + testing_cap ≈ 0.10 + ~0.10 + 0.45 = 0.65
  # of pool annually under maximum-intervention scenarios, with 30-45% being
  # the typical observed range (Mali 39%, Guinea 33%, Eshun-Wilson 2022 PLOS
  # Med meta-analysis 39% pooled).
  testing_reengagement_cap_frac <- if (!is.null(hiv_params$testing_reengagement_cap_frac) &&
                                       !is.na(hiv_params$testing_reengagement_cap_frac))
    hiv_params$testing_reengagement_cap_frac
  else 0.45
  testing_reengagement_cap <- max(0, total_ltfu_pool * testing_reengagement_cap_frac)
  
  # Apply cap to testing-driven re-engagement only. Tracking and spontaneous
  # flow freely — they are bounded structurally by their own formulas.
  # Capture pre-cap value to compute shrinkage_reengage in the ART initiations
  # block below, so per-modality linked counts and costs scale proportionally
  # if the cap binds.
  re_engagement_testing_uncapped <- re_engagement_testing
  re_engagement_testing <- min(re_engagement_testing, testing_reengagement_cap)
  re_engagement         <- re_engagement_testing
  
  # positive_tests = new_diagnoses + retest positives that contributed to the
  # cascade. Retest positives beyond the testing cap become implicit no-ops
  # (the test happened, the cost accrued via tests_performed, but no cascade
  # movement occurred).
  positive_tests        <- new_diagnoses + re_engagement_testing
  
  # Spontaneous returns are folded into ltfu_reengaged for downstream cascade
  # accounting (they re-enter on_art the same way tracking/tracing returnees do).
  ltfu_reengaged <- ltfu_reengaged + spontaneous_reengaged
  # Suppression gain from re-engaged patients (tracking/tracing + spontaneous).
  additional_suppressed <- additional_suppressed +
    ltfu_reengaged * hiv_params$tracking_reengagement_supp
  
  # ── ART INITIATIONS (proportional-scaling under caps) ────────────────────
  # Compute per-component shrinkage ratios from the two upstream caps:
  #   shrinkage_new      = new_diagnoses        / new_diagnoses_uncapped
  #   shrinkage_reengage = re_engagement_testing / re_engagement_testing_uncapped
  # Both are 1.0 when their respective caps don't bind (the common case),
  # so the formula reduces to the per-modality semantic:
  #   art_init_testing = Σ_i (new_dx_i + retest_pos_i) × linkage_rate_i
  #   linkage_cost     = Σ_i (new_dx_i + retest_pos_i) × linkage_cost_i   # per positive
  # When a cap binds, the corresponding component is scaled down,
  # preserving the per-modality mix (assumes proportional capping across
  # modalities — see source-comment note).
  #
  # PMTCT contributions were folded into new_dx_*_uncapped above so they
  # share shrinkage_new with general new diagnoses.
  #
  # `average_linkage_cap` is no longer used: per-intervention linkage_rate
  # values are the authoritative source, and the cascade-identity ceiling
  # (max_art_initiations, below) remains the structural backstop. Parameter
  # validation (linkage_rate <= 1) should be enforced at parameter load.
  shrinkage_new <- if (new_diagnoses_uncapped > 0) {
    new_diagnoses / new_diagnoses_uncapped
  } else 0
  shrinkage_reengage <- if (re_engagement_testing_uncapped > 0) {
    re_engagement_testing / re_engagement_testing_uncapped
  } else 0
  
  # Linked count (post-cap): scaled per component
  art_inititations_testing <- shrinkage_new      * new_dx_linked_uncapped +
    shrinkage_reengage * retest_pos_linked_uncapped
  art_initiations          <- art_inititations_testing + art_initiations
  
  # Linkage cost (post-cap): scaled per component, same shrinkage as the
  # corresponding linked count, so cost per linked patient is preserved
  # per-modality in the uncapped case and proportionally scaled under capping.
  total_intervention_cost <- total_intervention_cost +
    shrinkage_new      * new_dx_linkage_cost_uncapped +
    shrinkage_reengage * retest_pos_linkage_cost_uncapped
  
  # Additional suppressed from testing (post-cap): scaled per component. The
  # supp_uncapped accumulators carry per-source suppression rates baked in
  # (general: testing_art_init_supp; PMTCT: pmtct_cascade_supp_rate), so a
  # single sum gives the correct combined suppression contribution.
  additional_suppressed_testing <-
    shrinkage_new      * new_dx_supp_uncapped +
    shrinkage_reengage * retest_pos_supp_uncapped
  additional_suppressed <- additional_suppressed + additional_suppressed_testing
  
  # Cannot initiate more on ART than are diagnosed but not yet on ART
  # (accounting for on_art being reduced by net LTFU losses).
  effective_on_art <- populations$on_art - ltfu_new_effective
  max_art_initiations <- populations$diagnosed + new_diagnoses - effective_on_art +
    re_engagement
  art_initiations     <- min(art_initiations, max(0, max_art_initiations))
  
  # Cannot suppress more than are currently unsuppressed on ART.
  # Denominator = everyone on ART at end of year (effective_on_art + new initiations
  # + re-engaged) minus those already suppressed (net of LTFU losses).
  # ltfu_reengaged must be included: they are on ART and some were previously
  # suppressed before dropping out, so the ceiling must account for them.
  max_additional_suppressed <- effective_on_art + art_initiations + ltfu_reengaged -
    populations$suppressed + stable_ltfu
  additional_suppressed <- min(additional_suppressed, max(0, max_additional_suppressed))
  
  # ========================================================================
  # SECOND PASS: Mortality interventions
  # All depend on finalised art_initiations.
  #
  # Chain for new initiations:
  #   cd4_tested    = art_initiations x cd4_coverage
  #   ahd_diagnosed = cd4_tested x prop_ahd$new_initiations
  #   AHD package effect gated by: cd4_coverage x ahd_pkg_coverage x ahd_pkg_efficacy
  #
  # AHD package applies only to those diagnosed with AHD via CD4 test.
  # ========================================================================
  
  on_art_total_est <- effective_on_art + art_initiations + ltfu_reengaged
  
  # Initialise scalars
  cd4_coverage_frac     <- 0   # proportion of new initiates who receive a CD4 test
  ahd_pkg_eff_reduction <- 0   # reduces AHD mortality rate, gated by CD4 diagnosis
  
  # Collect intervention values first (need cd4 before ahd_package)
  cd4_value     <- ifelse(is.null(interventions$cd4_testing),    0, interventions$cd4_testing)
  ahd_pkg_value <- ifelse(is.null(interventions$ahd_package),    0, interventions$ahd_package)
  
  
  
  # ── CD4 testing ──────────────────────────────────────────────────────────
  if (cd4_value > 0 && art_initiations > 0) {
    n_cd4_tested      <- min(art_initiations * (cd4_value / 100), art_initiations)
    cd4_coverage_frac <- n_cd4_tested / art_initiations
    cd4_cost          <- n_cd4_tested * all_interventions$cd4_testing$unit_cost
    total_intervention_cost <- total_intervention_cost + cd4_cost
  }
  
  # ── AHD package (only those diagnosed with AHD via CD4 test) ─────────────
  if (ahd_pkg_value > 0 && art_initiations > 0) {
    prop_ahd_new_init <- MORTALITY_RATES$prop_ahd$new_initiations
    n_cd4_tested      <- art_initiations * cd4_coverage_frac
    n_ahd_pool        <- art_initiations * prop_ahd_new_init
    
    # Targeted yield: CD4 tests preferentially identify AHD cases, capped by pool size.
    n_ahd_diagnosed   <- min(n_cd4_tested * CD4_AHD_TARGETING_YIELD, n_ahd_pool)
    
    # Effective gating fraction = AHD cases found / total AHD cases that exist
    cd4_ahd_detection_frac <- if (n_ahd_pool > 0) n_ahd_diagnosed / n_ahd_pool else 0
    
    n_ahd_pkg_reached       <- min(n_ahd_diagnosed * (ahd_pkg_value / 100), n_ahd_diagnosed)
    ahd_pkg_cov_frac_of_ahd <- if (n_ahd_diagnosed > 0) n_ahd_pkg_reached / n_ahd_diagnosed else 0
    
    ahd_pkg_eff_reduction   <- cd4_ahd_detection_frac * ahd_pkg_cov_frac_of_ahd *
      all_interventions$ahd_package$efficacy
    
    total_intervention_cost <- total_intervention_cost +
      n_ahd_pkg_reached * all_interventions$ahd_package$unit_cost
  }
  
  # ========================================================================
  # CASCADE POPULATIONS (PRE-MORTALITY)
  # on_art and suppressed are reduced by net LTFU before gains are added.
  # ========================================================================
  
  end_diagnosed_pre_mort  <- min(max(populations$diagnosed + new_diagnoses, 0),
                                 populations$plhiv)
  end_on_art_pre_mort     <- min(max(effective_on_art + art_initiations + ltfu_reengaged, 0),
                                 end_diagnosed_pre_mort)
  
  # ========================================================================
  # FIVE CASCADE GROUPS (mutually exclusive, before mortality)
  #
  # Suppression allocation: each on-ART subgroup gets its own suppression
  # rate applied at the group level, rather than the previous approach which
  # filled n_established_supp first and let new initiates absorb spillover.
  # The old approach caused n_established_treated → 0 whenever the suppression
  # gain flows exceeded n_established_on_art, which made parameters like
  # mortality_treated and prop_ahd_established_treated effectively unused.
  #
  # Base allocation: each group's suppression share derived from its own rate.
  # Intervention boosters (EAC, retention spillover, AHD package, ANC/PNC VL
  # testing) then shift people from "treated" to "supp" within the appropriate
  # group. additional_suppressed remains exposed as the total intervention-
  # attributed shift, for scenario reporting and the existing baseline_delta.
  # ========================================================================
  
  n_undiagnosed        <- max(0, populations$plhiv - end_diagnosed_pre_mort)
  n_diagnosed_not_art  <- max(0, end_diagnosed_pre_mort - end_on_art_pre_mort)
  n_new_initiations    <- min(art_initiations, end_on_art_pre_mort)
  n_established_on_art <- max(0, end_on_art_pre_mort - n_new_initiations)
  
  # --- Base suppression allocation per group ---
  # Established patients: suppress at the country's reported 3rd-95 rate
  # (these are people on ART >1 year; the input pct_suppressed is the
  # steady-state suppression rate for the established population).
  pct_supp_frac <- context$percent_suppressed / 100
  n_est_supp_base    <- n_established_on_art * pct_supp_frac
  n_est_treated_base <- n_established_on_art - n_est_supp_base
  
  # New initiates: suppress at testing_art_init_supp (typically ~0.9 by year-end).
  # This is the 12-month suppression rate for new starts, which is generally
  # lower than the country's overall 3rd-95 since some new starts haven't
  # achieved suppression yet.
  testing_init_supp_frac <- hiv_params$testing_art_init_supp
  n_new_supp_base    <- n_new_initiations * testing_init_supp_frac
  n_new_treated_base <- n_new_initiations - n_new_supp_base
  
  # --- Intervention-attributed suppression shifts ---
  # additional_suppressed accumulates contributions from EAC, retention
  # spillover, ANC/PNC VL testing, PMTCT cascade, and the re-engagement
  # suppression bonus. These represent people moved from "treated" to "supp"
  # ABOVE the baseline rates. Most boosters act on established patients
  # (EAC, retention, ANC/PNC VL), so they shift within the established group.
  #
  # However, the additional_suppressed pool ALREADY double-counts:
  #   - testing → linked × testing_art_init_supp (now in n_new_supp_base)
  #   - re-engagement × tracking_reengagement_supp (re-engagers are in
  #     n_established_on_art via ltfu_reengaged; their suppression at 0.9 vs
  #     country avg pct_supp_frac is a slight adjustment)
  # So we need to net these out before applying as a "shift". The cleanest
  # accounting: re-derive an "intervention shift" from the components that
  # truly represent boosters above baseline.
  
  # Subtract the components already captured in the base allocation:
  #   - new initiates 90% suppression already in n_new_supp_base
  #   - re-engagers' suppression at country avg already in n_est_supp_base
  # What's left in additional_suppressed = intervention boosters
  baseline_new_init_supp     <- art_inititations_testing * testing_init_supp_frac
  baseline_reengagement_supp <- ltfu_reengaged * pct_supp_frac
  intervention_supp_shift    <- additional_suppressed -
    baseline_new_init_supp - baseline_reengagement_supp
  # If re-engagers suppress at a different rate than country avg (e.g.
  # tracking_reengagement_supp = 0.9 vs pct_supp = 0.913), include that delta.
  reengagement_supp_delta <- ltfu_reengaged *
    (hiv_params$tracking_reengagement_supp - pct_supp_frac)
  intervention_supp_shift <- intervention_supp_shift + reengagement_supp_delta
  intervention_supp_shift <- max(0, intervention_supp_shift)
  
  # Apply the shift: most boosters target established patients
  # (EAC, retention, ANC/PNC VL all act on the on-ART unsuppressed pool).
  # Cap at the available unsuppressed-established pool to avoid over-shifting.
  intervention_shift_to_est <- min(intervention_supp_shift, n_est_treated_base)
  intervention_shift_to_new <- max(0, intervention_supp_shift - intervention_shift_to_est)
  intervention_shift_to_new <- min(intervention_shift_to_new, n_new_treated_base)
  
  n_established_supp    <- n_est_supp_base + intervention_shift_to_est
  n_established_treated <- n_est_treated_base - intervention_shift_to_est
  n_new_supp            <- n_new_supp_base + intervention_shift_to_new
  n_new_treated         <- n_new_treated_base - intervention_shift_to_new
  
  # Derive end_suppressed_pre_mort from the per-group allocation
  end_suppressed_pre_mort <- n_established_supp + n_new_supp
  
  # ========================================================================
  # EFFECTIVE AHD MORTALITY RATES (intervention-adjusted where applicable)
  # ========================================================================
  prop_ahd <- MORTALITY_RATES$prop_ahd
  # Untreated groups: no interventions reach them; use untreated AHD rate
  eff_base_rate_untreated <- MORTALITY_RATES$untreated_undiagnosed
  eff_ahd_rate_untreated  <- MORTALITY_RATES$ahd_untreated
  
  # New initiations: year-1 AHD rate (much higher than established AHD);
  # AHD package reduces it; base rate unchanged
  eff_base_rate_new_init <- MORTALITY_RATES$new_art_initiations
  eff_ahd_rate_new_init  <- MORTALITY_RATES$ahd_new *
    (1 - ahd_pkg_eff_reduction)
  
  # Established on ART: long-term AHD rate (cohort-weighted, much lower than
  # year-1); AHD package reduces it (though established patients are less
  # likely to receive the AHD package — it targets new initiates). Base
  # rates unchanged.
  eff_ahd_rate_established <- MORTALITY_RATES$ahd_established *
    (1 - ahd_pkg_eff_reduction)
  
  # ========================================================================
  # DEATHS BY GROUP
  # Each person appears in exactly one group.
  # Within each group: (1 - prop_ahd) face base rate, prop_ahd face AHD rate.
  # ========================================================================
  
  calc_deaths <- function(n, base_rate, ahd_rate, prop_ahd) {
    n * ((1 - prop_ahd) * base_rate + prop_ahd * ahd_rate)
  }
  
  deaths_undiagnosed         <- calc_deaths(n_undiagnosed,         eff_base_rate_untreated,    eff_ahd_rate_untreated,   prop_ahd$undiagnosed)
  deaths_diagnosed_not_art   <- calc_deaths(n_diagnosed_not_art,   eff_base_rate_untreated,    eff_ahd_rate_untreated,   prop_ahd$diagnosed_not_art)
  deaths_new_initiations     <- calc_deaths(n_new_initiations,     eff_base_rate_new_init,     eff_ahd_rate_new_init,    prop_ahd$new_initiations)
  deaths_established_treated <- calc_deaths(n_established_treated, MORTALITY_RATES$treated,    eff_ahd_rate_established, prop_ahd$established_treated)
  deaths_established_supp    <- calc_deaths(n_established_supp,    MORTALITY_RATES$suppressed, eff_ahd_rate_established, prop_ahd$established_supp)
  
  total_hiv_deaths <- deaths_undiagnosed + deaths_diagnosed_not_art +
    deaths_new_initiations + deaths_established_treated + deaths_established_supp
  
  # ========================================================================
  # MORTALITY CALIBRATION TO COUNTRY UNAIDS TARGET
  # ------------------------------------------------------------------------
  # Literature-based per-cascade rates produce SSA-average mortality, which
  # over- or under-estimates country-specific deaths because cascade
  # composition (especially the AHD distribution in untreated pools) varies
  # by country and epidemic maturity. We anchor the model to the country's
  # UNAIDS-published AIDS deaths.
  #
  # Baseline run: compute calibration_factor = target / modelled_deaths
  # Scenario run: receive baseline_factor and apply it (preserves relative
  #               impact of interventions while keeping absolute deaths
  #               anchored to country reality at baseline)
  # If no target provided, factor defaults to 1 (no calibration).
  # ========================================================================
  target_aids_deaths <- context$aids_deaths_per_year
  
  # Calibration fires if either (a) the global toggle is on, or (b) the
  # country-specific flag from basic_hiv_data.csv (use_mortality_calibration)
  # is TRUE. Country flag defaults to FALSE when absent.
  country_calibration_flag <- isTRUE(context$use_mortality_calibration)
  calibrate_this_country   <- USE_MORTALITY_CALIBRATION || country_calibration_flag
  
  if (is_baseline) {
    if (calibrate_this_country &&
        !is.null(target_aids_deaths) && !is.na(target_aids_deaths) &&
        target_aids_deaths > 0 && total_hiv_deaths > 0) {
      mortality_calibration_factor <- target_aids_deaths / total_hiv_deaths
    } else {
      mortality_calibration_factor <- 1
    }
  } else if (is.null(mortality_calibration_factor)) {
    mortality_calibration_factor <- 1
  }
  
  # Apply factor to all death components
  deaths_undiagnosed         <- deaths_undiagnosed         * mortality_calibration_factor
  deaths_diagnosed_not_art   <- deaths_diagnosed_not_art   * mortality_calibration_factor
  deaths_new_initiations     <- deaths_new_initiations     * mortality_calibration_factor
  deaths_established_treated <- deaths_established_treated * mortality_calibration_factor
  deaths_established_supp    <- deaths_established_supp    * mortality_calibration_factor
  total_hiv_deaths           <- total_hiv_deaths           * mortality_calibration_factor
  
  # Deaths averted = difference between unadjusted (no interventions) and adjusted deaths
  # Only on-treatment groups are affected by interventions
  unadjusted_deaths_on_treatment <-
    calc_deaths(n_new_initiations,     MORTALITY_RATES$new_art_initiations, MORTALITY_RATES$ahd_new,         prop_ahd$new_initiations) +
    calc_deaths(n_established_treated, MORTALITY_RATES$treated,             MORTALITY_RATES$ahd_established, prop_ahd$established_treated) +
    calc_deaths(n_established_supp,    MORTALITY_RATES$suppressed,          MORTALITY_RATES$ahd_established, prop_ahd$established_supp)
  unadjusted_deaths_on_treatment <- unadjusted_deaths_on_treatment * mortality_calibration_factor
  
  adjusted_deaths_on_treatment <- deaths_new_initiations + deaths_established_treated + deaths_established_supp
  
  total_deaths_averted <- max(0, unadjusted_deaths_on_treatment - adjusted_deaths_on_treatment)
  end_deaths           <- max(0, total_hiv_deaths)
  
  # ========================================================================
  # CASCADE (POST-MORTALITY)
  # ========================================================================
  
  remaining_undiagnosed      <- max(0, n_undiagnosed        - deaths_undiagnosed)
  remaining_diagnosed_not_art<- max(0, n_diagnosed_not_art  - deaths_diagnosed_not_art)
  # New initiates: apply the same mortality rate proportionally to suppressed/treated sub-groups
  # (both sub-groups use new_art_initiations rate — suppressed new initiates are still early
  # in treatment and face the same elevated early-ART mortality as unsuppressed ones).
  new_init_mort_frac     <- if (n_new_initiations > 0)
    min(1, deaths_new_initiations / n_new_initiations) else 0
  remaining_new_supp     <- max(0, round(n_new_supp    * (1 - new_init_mort_frac)))
  remaining_new_treated  <- max(0, round(n_new_treated * (1 - new_init_mort_frac)))
  remaining_new_init     <- remaining_new_supp + remaining_new_treated
  remaining_est_treated  <- max(0, n_established_treated - deaths_established_treated)
  remaining_est_supp     <- max(0, n_established_supp    - deaths_established_supp)
  
  end_suppressed <- remaining_est_supp + remaining_new_supp   # fix: include suppressed new initiates
  end_on_art     <- remaining_est_treated + remaining_est_supp + remaining_new_init
  end_diagnosed  <- remaining_diagnosed_not_art + end_on_art
  
  end_plhiv      <- max(0, remaining_undiagnosed + end_diagnosed)
  
  # Ensure cascade consistency
  end_suppressed <- min(end_suppressed, end_on_art)
  end_on_art     <- min(end_on_art, end_diagnosed)
  
  # ========================================================================
  # CALCULATE END-OF-YEAR INFECTIONS — Stratified FOI
  # ========================================================================
  # β is calibrated from baseline so the model always reproduces observed
  # new_infections_per_year exactly. Prevention interventions reduce the
  # effective susceptible pool; suppression_delta reduces infectious pressure.
  # Prevention loop below is COSTS ONLY — FOI handles all infection impact.
  # ========================================================================
  
  # suppression_delta = MARGINAL change in END-OF-YEAR suppressed PLHIV between
  # the scenario and baseline. Positive when a scenario raises end_suppressed
  # above baseline (reduces infectious pressure → fewer new infections);
  # NEGATIVE when a scenario lowers end_suppressed (e.g. cutting testing or
  # VL → fewer people suppressed → more new infections). At baseline this is
  # zero by construction.
  #
  # β was calibrated using the observed baseline cascade state (which already
  # reflects the baseline programme), so only the marginal end-state change
  # is applied here — preventing double-counting.
  #
  # WHY STATE-BASED rather than event-flow (additional_suppressed delta):
  # The event-flow approach undercredited stable-client retention, because
  # retention generates no new "event" — a retained stable patient stays
  # continuously suppressed, contributing to end_suppressed but not to
  # additional_suppressed. It also caused a perverse +infections-from-
  # retention result: more retention shrinks the LTFU pool that re-engagement
  # acts on, reducing additional_suppressed and (paradoxically) raising FOI
  # even though end_suppressed went up. Switching to a state-based delta
  # makes whatever moves end_suppressed (retention, testing, EAC, mortality
  # reductions) feed FOI symmetrically. Each person can only be counted once
  # in end_suppressed, so no double-counting risk.
  #
  # Edge case: baseline_end_suppressed defaults to NULL so callers can be
  # added incrementally. When NULL and !is_baseline, fall back to 0 (treats
  # the scenario as if baseline had no suppression — wrong direction, but
  # the caller in this app always supplies the value, so this path is a
  # defensive fallback only).
  suppression_delta <- if (is_baseline) {
    0
  } else if (!is.null(baseline_end_suppressed)) {
    end_suppressed - baseline_end_suppressed   # allow negative
  } else {
    # Defensive fallback if caller forgot to pass baseline_end_suppressed
    warning("baseline_end_suppressed not provided to calculate_scenario_outcomes(); ",
            "FOI suppression_delta defaulting to 0 (scenario will reproduce baseline ",
            "infections regardless of suppression change). Update the caller to pass ",
            "outcomes_baseline()$end_suppressed.")
    0
  }
  
  # Pass efficacies from intervention_params into FOI so they stay in sync
  foi_interventions <- c(
    interventions,
    list(
      eff_prep_oral = all_interventions$prep_oral$efficacy       %||% 0.99,
      eff_prep_len  = all_interventions$prep_lenacapavir$efficacy %||% 1.00,
      eff_condom    = all_interventions$condoms$efficacy          %||% 0.80,
      acts_per_year_high     = ACTS_PER_YEAR_HIGH,    
      acts_per_year_gen      = ACTS_PER_YEAR_GEN,     
      condom_use_rate_high   = CONDOM_USE_RATE_HIGH,    
      condom_use_rate_gen    = CONDOM_USE_RATE_GEN    
    )
  )
  
  # NEW — augment baseline_interventions with the same efficacy & behaviour
  # parameters that get appended to foi_interventions. Without this,
  # compute_prevention_adjustments() falls back to its internal %||% defaults
  # during calibration (e.g. eff_condom = 0.80, condom_use_rate_gen = 0.55)
  # while the FOI calculation uses Excel-sourced values from intervention_params
  # and hiv_params. The mismatch breaks the baseline round-trip: calibration
  # assumes one level of baseline protection, FOI applies a different level,
  # and the recomputed baseline infections diverge from the observed count.
  baseline_foi <- if (!is.null(baseline_interventions)) {
    c(baseline_interventions,
      list(
        eff_prep_oral        = all_interventions$prep_oral$efficacy        %||% 0.99,
        eff_prep_len         = all_interventions$prep_lenacapavir$efficacy %||% 1.00,
        eff_condom           = all_interventions$condoms$efficacy          %||% 0.80,
        acts_per_year_high   = ACTS_PER_YEAR_HIGH,
        acts_per_year_gen    = ACTS_PER_YEAR_GEN,
        condom_use_rate_high = CONDOM_USE_RATE_HIGH,
        condom_use_rate_gen  = CONDOM_USE_RATE_GEN
      ))
  } else NULL
  
  foi_result         <- estimate_new_infections_foi(
    context                = context,
    populations            = populations,
    scenario_interventions = foi_interventions,
    suppression_delta      = suppression_delta,
    baseline_interventions = baseline_foi
  )
  
  
  # Then your diagnostic block becomes:
  if (is_baseline) {
    # Build a fingerprint of the current context
    fingerprint <- paste(round(context$total_population),
                         round(context$plhiv),
                         round(context$new_infections_per_year),
                         sep = "_")
    
    last <- get0("last_fp", envir = .last_diag_country, ifnotfound = "")
    if (fingerprint != last) {
      cat("\n=== BASELINE FOI ROUNDTRIP ===\n")
      cat("input new_infections_per_year:", context$new_infections_per_year, "\n")
      cat("computed foi_result$new_infections:", foi_result$new_infections, "\n")
      cat("ratio computed/input:", foi_result$new_infections / context$new_infections_per_year, "\n")
      cat("rr_high used:", define_strata_params(context)$rr_high, "\n")
      cat("prop_high_risk used:", define_strata_params(context)$prop_high_risk, "\n")
      cat("frac_high (calib):", foi_result$diagnostics$frac_infections_high_risk, "\n")
      cat("==============================\n\n")
      assign("last_fp", fingerprint, envir = .last_diag_country)
    }
  }
  
  end_new_infections <- foi_result$new_infections
  
  # Add new infections into PLHIV — they enter as undiagnosed
  remaining_undiagnosed <- remaining_undiagnosed + end_new_infections
  end_plhiv             <- max(0, remaining_undiagnosed + end_diagnosed)
  
  infections_averted <- foi_result$infections_averted
  
  
  # Validate calibration — logs warnings but does not stop execution
  strata_params_val <- define_strata_params(context)
  strata_val        <- partition_into_strata(populations, strata_params_val)
  betas_val         <- calibrate_beta(context, populations, strata_val, strata_params_val)
  cal_check         <- validate_calibration(context, populations, betas_val, strata_val, strata_params_val)
  # Validate calibration — logs warnings but does not stop execution.
  # Suppress warnings during Shiny reactive transitions: when switching
  # country presets, plhiv may update before total_population (or vice
  # versa), briefly producing plhiv > total_population and negative
  # hiv_negative / sexually_active_negative. The validation flags raised
  # on those transient states are noise, not signal — gate on context
  # self-consistency before emitting.
  context_is_sane <-
    !is.null(context$plhiv) && !is.null(context$total_population) &&
    !is.na(context$plhiv) && !is.na(context$total_population) &&
    context$plhiv <= context$total_population &&
    context$total_population > 0 &&
    !is.null(populations$sexually_active_negative) &&
    populations$sexually_active_negative > 0
  
  if (!cal_check$valid && context_is_sane) {
    warning(paste("FOI calibration flags:", paste(cal_check$flags, collapse = "; ")))
  }
  
  # Prevention cost loop (COSTS ONLY — infection impact already captured by FOI)
  for (int_key in names(all_interventions)) {
    intervention       <- all_interventions[[int_key]]
    intervention_value <- interventions[[int_key]]
    
    if (is.null(intervention_value) || intervention_value == 0) next
    
    eligible       <- populations[[intervention$eligible_pop]] %||% 0
    number_reached <- if (intervention$type == "coverage")
      eligible * (intervention_value / 100)
    else
      min(intervention_value, eligible)
    number_reached <- min(number_reached, eligible)
    
    if ("adult_infections" %in% intervention$outcomes) {
      # Cost only — FOI accounts for protective effect.
      # PrEP cost is capped at sexually_active_negative (HIV-neg, sexually active).
      # This is the same denominator used by FOI for impact, so cost and
      # impact stay internally consistent. Children and non-sexually-active
      # adults are excluded — they can't biologically benefit from PrEP and
      # are not in the FOI strata. Allocation (3-fold high-risk advantage with
      # spillover) is handled in compute_prevention_adjustments.
      units_costed <- if (int_key == "condoms") {
        (intervention_value %||% 0)
      } else if (int_key %in% c("prep_oral", "prep_lenacapavir")) {
        min(intervention_value, populations$sexually_active_negative %||% 0)
      } else {
        number_reached
      }
      total_intervention_cost <- total_intervention_cost +
        units_costed * intervention$unit_cost
    }
  }
  # ========================================================================
  # MTCT CASCADE: infant infections from cascade-based maternal risk groups
  # ========================================================================
  
  # Step 1: ANC VL testing shifts unsuppressed pregnant women on ART -> suppressed.
  # anc_vl_reached_preg is the subset of anc_vl_testing number_reached (pregnant_on_art).
  anc_vl_eff   <- all_interventions$anc_vl_testing$efficacy %||% 0
  anc_vl_shift <- min(
    anc_vl_reached_preg * (1 - context$percent_suppressed / 100) * anc_vl_eff,
    populations$pregnant_on_art_unsuppressed
  )
  mtct_supp   <- populations$pregnant_on_art_suppressed   + anc_vl_shift
  mtct_unsupp <- max(0, populations$pregnant_on_art_unsuppressed - anc_vl_shift)
  
  # Step 1b: PNC VL testing shifts remaining unsuppressed postpartum mothers -> suppressed.
  # Applied after ANC VL so it cannot double-count women already shifted at ANC.
  # Capped at mtct_unsupp (the remaining unsuppressed pool after ANC VL).
  pnc_vl_eff   <- all_interventions$pnc_vl_testing$efficacy %||% 0
  pnc_vl_shift <- min(
    pnc_vl_reached_preg * (1 - context$percent_suppressed / 100) * pnc_vl_eff,
    mtct_unsupp
  )
  mtct_supp   <- mtct_supp   + pnc_vl_shift
  mtct_unsupp <- max(0, mtct_unsupp - pnc_vl_shift)
  
  # Step 2: Of newly diagnosed HIV+ pregnant women, a proportion link to PMTCT ART,
  # and of those, a proportion achieve viral suppression during pregnancy/breastfeeding.
  # Suppression rate is discounted from the country average — newly initiating women
  # are less likely to fully suppress quickly.
  pmtct_linkage_rate  <- all_interventions$anc_hiv_testing$linkage_rate %||% 0.85
  pmtct_supp_rate     <- (context$percent_suppressed / 100) * hiv_params$pmtct_cascade_supp_discount 
  
  pmtct_linked_total  <- min(pmtct_new_diagnoses, populations$pregnant_not_on_art)
  pmtct_linked_art    <- pmtct_linked_total * pmtct_linkage_rate
  pmtct_linked_supp   <- pmtct_linked_art   * pmtct_supp_rate
  pmtct_linked_unsupp <- pmtct_linked_art   - pmtct_linked_supp
  pmtct_not_linked    <- pmtct_linked_total - pmtct_linked_art  # diagnosed but did not start ART
  
  mtct_supp   <- mtct_supp   + pmtct_linked_supp    # newly suppressed PMTCT mothers
  mtct_unsupp <- mtct_unsupp + pmtct_linked_unsupp  # on ART but not suppressed
  mtct_no_art <- max(0, populations$pregnant_not_on_art - pmtct_linked_total) +
    pmtct_not_linked                 # undiagnosed remainder + diagnosed but unlinked
  
  # Step 3: Infant infections from risk-stratified MTCT rates
  baseline_infant_infections <-
    mtct_supp   * MTCT_RATES$on_art_suppressed   +
    mtct_unsupp * MTCT_RATES$on_art_unsuppressed +
    mtct_no_art * MTCT_RATES$not_on_art
  
  # Step 4: Infant prophylaxis (NVP) reduces transmission over its coverage window.
  #
  # NVP only protects during the period it is administered. Effective efficacy
  # against the full cumulative MTCT rate is scaled by the fraction of the total
  # risk window covered:
  #
  #   eff_efficacy = nvp_raw_efficacy * (nvp_prophylaxis_duration_months / bf_duration_months)
  #
  # hiv_params inputs:
  #   bf_duration_months              - total breastfeeding duration (default 18)
  #   nvp_prophylaxis_duration_months - months NVP is given (1.5 = 6-week standard;
  #                                     18 = extended through full breastfeeding)
  #
  # Example: 54% efficacy, 6-week NVP (1.5m), 18m breastfeeding
  #   -> 0.54 * (1.5/18) = 0.045 effective efficacy
  nvp_bf_months     <- hiv_params$bf_duration_months              %||% 18
  nvp_prophy_months <- hiv_params$nvp_prophylaxis_duration_months %||% 1.5
  nvp_raw_eff       <- all_interventions$infant_prophylaxis$efficacy %||% 0
  nvp_eff_adjusted  <- nvp_raw_eff * min(1, nvp_prophy_months / nvp_bf_months)
  
  infant_prophy_reduction   <- baseline_infant_infections * infant_prophy_cov_frac * nvp_eff_adjusted
  end_infant_infections     <- max(0, baseline_infant_infections - infant_prophy_reduction)
  infant_infections_averted <- infant_prophy_reduction
  
  # ========================================================================
  # ACUTE MATERNAL INFECTION DURING BREASTFEEDING (incident pathway)
  # ========================================================================
  # Women who acquire HIV during pregnancy or breastfeeding are NOT captured
  # by the prevalent-cohort PMTCT cascade above. Their seroconversion typically
  # post-dates the ANC test, so without late-pregnancy or PNC retesting they
  # remain undiagnosed and untreated through the breastfeeding period. Acute
  # maternal infection carries substantially elevated MTCT risk (Johnson et al.
  #
  # Approach: estimate the share of new adult infections that occur in women
  # currently pregnant/breastfeeding using their time-at-risk in that state,
  # approximated as births × 1.75 woman-years (0.75 yr pregnancy + 1.0 yr BF).
  # NOTE: the 1.0 yr BF here is a steady-state cohort weight specific to this
  # calculation; the existing hiv_params$bf_duration_months (default 18) drives
  # NVP efficacy duration scaling above and is a different quantity.
  #
  # These infections are MISSED BY EID: the mother was HIV-negative at ANC, so
  # her infant is not flagged as HIV-exposed and never enters the EID program.
  # They are routed directly to infant_untreated downstream.
  percent_maternal_bf_inf    <- (context$total_population * context$birth_rate / 1000 * (3+hiv_params$bf_duration_months)/12) /
    context$total_population
  maternal_infections_bf     <- percent_maternal_bf_inf * context$new_infections_per_year
  infant_infections_acute_bf <- maternal_infections_bf *
    (hiv_params$acute_bf_transmission %||% 0.28)
  
  # ── DEBUG: acute-BF pathway diagnostic (console only) ─────────────────────
  if (populations$hiv_exposed_infants > 0 || infant_infections_acute_bf > 0) {
    cat(sprintf(
      "[MTCT-ACUTE] preg_bf_share=%.3f  maternal_inf_bf=%.0f  infant_inf_acute=%.0f\n",
      percent_maternal_bf_inf, maternal_infections_bf, infant_infections_acute_bf))
  }
  
  # ── DEBUG: implied MTCT rate vs published (console only) ──────────────────
  # Implied final MTCT rate = infant infections / HIV-exposed infants. This is
  # the model's analogue of a published country final transmission rate (the
  # % of HIV-exposed infants infected through pregnancy/delivery/breastfeeding),
  # so it can be eyeballed against UNAIDS/UNICEF country figures. Baseline is
  # the pre-prophylaxis (maternal-cascade-only) rate. Denominator-consistent
  # with the published rate (both use HIV-exposed infants). Print only.
  if (populations$hiv_exposed_infants > 0) {
    cat(sprintf(
      "[MTCT] implied final rate: %.2f%%  (baseline %.2f%%)  | %.0f / %.0f exposed infants\n",
      100 * end_infant_infections      / populations$hiv_exposed_infants,
      100 * baseline_infant_infections / populations$hiv_exposed_infants,
      end_infant_infections,
      populations$hiv_exposed_infants))
  } else {
    cat("[MTCT] implied rate: n/a (no HIV-exposed infants)\n")
  }
  
  # ── DEBUG: MTCT composition breakdown (console only) ──────────────────────
  # Localises a suspicious implied rate. Two suspects:
  #   (1) cascade composition — if mtct_supp dominates, baseline collapses
  #       because suppressed mothers carry the lowest transmission rate;
  #   (2) the three MTCT_RATES inputs themselves (placeholder / mis-scaled).
  # Shares are over hiv_exposed_infants so they should sum to ~100%.
  if (populations$hiv_exposed_infants > 0) {
    .hei <- populations$hiv_exposed_infants
    cat(sprintf(
      "[MTCT]   buckets: supp=%.0f (%.1f%%)  unsupp=%.0f (%.1f%%)  no_art=%.0f (%.1f%%)\n",
      mtct_supp,   100 * mtct_supp   / .hei,
      mtct_unsupp, 100 * mtct_unsupp / .hei,
      mtct_no_art, 100 * mtct_no_art / .hei))
    cat(sprintf(
      "[MTCT]   rates: supp=%.4f  unsupp=%.4f  no_art=%.4f\n",
      MTCT_RATES$on_art_suppressed,
      MTCT_RATES$on_art_unsuppressed,
      MTCT_RATES$not_on_art))
    cat(sprintf(
      "[MTCT]   prophylaxis: cov_frac=%.3f  efficacy=%.3f  reduction=%.0f infections\n",
      infant_prophy_cov_frac,
      all_interventions$infant_prophylaxis$efficacy %||% 0,
      infant_prophy_reduction))
  }
  
  # Step 5: Finalise EID diagnosis count and costs.
  # Cost applies to ALL HIV-exposed infants tested (hiv_exposed_infants x coverage),
  # regardless of infection status — every exposed infant receives a test.
  # Effect (diagnosis and subsequent ART linkage) applies only to infected infants,
  # captured via actual_eid_yield = end_infant_infections / hiv_exposed_infants.
  if (eid_infants_reached > 0 && populations$hiv_exposed_infants > 0) {
    actual_eid_yield      <- end_infant_infections / populations$hiv_exposed_infants
    eid_infants_diagnosed <- eid_infants_reached * actual_eid_yield *
      (all_interventions$eid$efficacy %||% 0.90)
  } else {
    eid_infants_diagnosed <- 0
  }
  # Testing cost: all HIV-exposed infants reached (infected or not)
  # Linkage cost: HIV+ infants identified 
  total_intervention_cost <- total_intervention_cost +
    eid_infants_reached   * (all_interventions$eid$unit_cost    %||% 0) +
    eid_infants_diagnosed * (all_interventions$eid$linkage_cost %||% 0)
  
  # ========================================================================
  # INFANT MORTALITY CASCADE
  # EID-diagnosed infants split by linkage to ART and suppression, mirroring
  # the adult cascade mortality structure.
  # Suppression rate discounted from country average — newly initiating infants
  # are less likely to suppress quickly. ###UPDATE discount from literature.
  # ========================================================================
  eid_linkage_rate     <- all_interventions$eid$linkage_rate %||% 0.80
  infant_supp_rate     <- hiv_params$eid_supp_rate
  
  infant_on_art        <- eid_infants_diagnosed * eid_linkage_rate
  infant_suppressed    <- infant_on_art * infant_supp_rate
  infant_on_art_unsupp <- infant_on_art - infant_suppressed
  # Acute-BF infections bypass EID entirely (mother never flagged HIV-exposed),
  # so they add directly to the untreated infant pool.
  infant_untreated     <- max(0, end_infant_infections - infant_on_art) +
    infant_infections_acute_bf
  
  # Deaths by infant treatment group
  infant_deaths_suppressed  <- infant_suppressed    * INFANT_MORTALITY_RATES$suppressed
  infant_deaths_on_art      <- infant_on_art_unsupp * INFANT_MORTALITY_RATES$on_art
  infant_deaths_untreated   <- infant_untreated     * INFANT_MORTALITY_RATES$untreated
  total_infant_deaths       <- infant_deaths_suppressed + infant_deaths_on_art +
    infant_deaths_untreated
  
  # Infant deaths averted vs no-EID counterfactual (all infected infants untreated).
  # Counterfactual includes acute-BF infections (also untreated) so deaths_averted
  # reflects what EID/PMTCT prevents vs a true no-intervention world.
  infant_deaths_averted <- max(0,
                               (end_infant_infections + infant_infections_acute_bf) *
                                 INFANT_MORTALITY_RATES$untreated - total_infant_deaths
  )
  
  # Wire into adult totals
  total_deaths_averted <- total_deaths_averted + infant_deaths_averted
  end_deaths           <- end_deaths           + total_infant_deaths
  
  # ========================================================================
  # CALCULATE COSTS
  # ========================================================================
  
  art_cost_unit <- context$art_cost_standard %||% ART_COST_STANDARD
  art_provision_cost <- end_on_art * art_cost_unit + dsd_cost_adjustment
  if (art_provision_cost < 0) {
    warning(sprintf(
      paste0("art_provision_cost floored to 0: DSD savings (%.0f) exceeded ",
             "standard ART provision cost (%.0f × %.0f = %.0f). ",
             "Check DSD unit costs in intervention_params."),
      -dsd_cost_adjustment, end_on_art, art_cost_unit,
      end_on_art * art_cost_unit
    ))
    art_provision_cost <- 0
  }
  # Total cost
  total_cost <- total_intervention_cost + art_provision_cost
  
  # ========================================================================
  # FINAL SANITY CHECKS - ENSURE NO NaN OR INVALID VALUES
  # ========================================================================
  
  # Check for any NaN or Inf values and replace with 0
  if (is.na(end_diagnosed) | is.nan(end_diagnosed) | is.infinite(end_diagnosed)) {
    end_diagnosed <- 0
    warning("end_diagnosed was invalid, set to 0")
  }
  if (is.na(end_on_art) | is.nan(end_on_art) | is.infinite(end_on_art)) {
    end_on_art <- 0
    warning("end_on_art was invalid, set to 0")
  }
  if (is.na(end_suppressed) | is.nan(end_suppressed) | is.infinite(end_suppressed)) {
    end_suppressed <- 0
    warning("end_suppressed was invalid, set to 0")
  }
  
  # Ensure cascade makes sense — floors at 0 (not starting population) so that
  # LTFU-driven reductions are preserved
  end_diagnosed  <- max(0, min(end_diagnosed,  populations$plhiv))
  end_on_art     <- max(0, min(end_on_art,     end_diagnosed))
  end_suppressed <- max(0, min(end_suppressed, end_on_art))
  
  # Final verification - these should NEVER be NaN at this point
  if (is.na(end_diagnosed) | is.na(end_on_art) | is.na(end_suppressed)) {
    stop("CRITICAL ERROR: Cascade values still NA after sanity checks!")
  }
  
  # ========================================================================
  # DIAGNOSTIC BLOCK — gated on `ltfu_debug` option
  # Enable with: options(ltfu_debug = TRUE) before running.
  # Prints LTFU / re-engagement / cascade flows to console for one run.
  # ========================================================================
  if (isTRUE(getOption("ltfu_debug", FALSE))) {
    cat("\n========== LTFU / RE-ENGAGEMENT DIAGNOSTICS ==========\n")
    cat(sprintf("Country tag: %s\n",
                ifelse(is.null(context$country), "(unknown)",
                       as.character(context$country))))
    
    cat("\n--- Year-start populations ---\n")
    cat(sprintf("  plhiv                   : %12.0f\n", populations$plhiv))
    cat(sprintf("  diagnosed               : %12.0f\n", populations$diagnosed))
    cat(sprintf("  on_art                  : %12.0f\n", populations$on_art))
    cat(sprintf("  on_art_stable           : %12.0f\n", populations$on_art_stable))
    cat(sprintf("  on_art_unstable         : %12.0f\n",
                populations$on_art - populations$on_art_stable))
    cat(sprintf("  suppressed              : %12.0f\n", populations$suppressed))
    cat(sprintf("  ltfu (all dx not on ART): %12.0f\n", populations$ltfu))
    cat(sprintf("  ltfu_new_stable         : %12.0f\n", populations$ltfu_new_stable))
    cat(sprintf("  ltfu_new_unstable       : %12.0f\n", populations$ltfu_new_unstable))
    cat(sprintf("  ltfu_new (gross)        : %12.0f\n", populations$ltfu_new))
    
    cat("\n--- LTFU prevention (DSD) ---\n")
    cat(sprintf("  ltfu_retained_frac      : %12.4f\n", ltfu_retained_frac))
    cat(sprintf("  ltfu_prevented (total)  : %12.0f\n", ltfu_prevented))
    cat(sprintf("  ltfu_prevented_stable   : %12.0f\n", ltfu_prevented_stable))
    cat(sprintf("  ltfu_prevented_unstable : %12.0f\n", ltfu_prevented_unstable))
    
    cat("\n--- Net incident LTFU after prevention ---\n")
    cat(sprintf("  stable_ltfu             : %12.0f\n", stable_ltfu))
    cat(sprintf("  unstable_ltfu           : %12.0f\n", unstable_ltfu))
    cat(sprintf("  ltfu_new_effective      : %12.0f\n", ltfu_new_effective))
    cat(sprintf("  total_ltfu_pool         : %12.0f  (prev_stock + ltfu_new_effective)\n",
                total_ltfu_pool))
    
    cat("\n--- Re-engagement flows ---\n")
    cat(sprintf("  deferred_tracking_cov   : %12.4f  (fraction)\n",
                deferred_tracking_coverage))
    cat(sprintf("  deferred_tracking_eff   : %12.4f\n", deferred_tracking_efficacy))
    cat(sprintf("  tracking_reached        : %12.0f\n", tracking_reached))
    cat(sprintf("  spontaneous_reengaged   : %12.0f  (= pool * %.3f, uncapped)\n",
                spontaneous_reengaged, ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE))
    cat(sprintf("  testing_reeng_cap       : %12.0f  (= %.2f * pool, testing only)\n",
                testing_reengagement_cap, testing_reengagement_cap_frac))
    cat(sprintf("  re_engagement_testing   : %12.0f  (testing flow after cap)\n",
                re_engagement_testing))
    cat(sprintf("  ltfu_reengaged (final)  : %12.0f  (tracking + spontaneous, uncapped)\n",
                ltfu_reengaged))
    
    cat("\n--- Testing & new diagnoses ---\n")
    cat(sprintf("  positive_tests          : %12.0f  (new + retest positives reaching cap)\n", positive_tests))
    cat(sprintf("  new_diagnoses           : %12.0f  (first-time positives → ART)\n", new_diagnoses))
    cat(sprintf("  re_engagement_testing   : %12.0f  (retest positives within LTFU cap)\n",
                re_engagement_testing))
    cat(sprintf("  art_inititations_testing: %12.0f\n", art_inititations_testing))
    cat(sprintf("  art_initiations (total) : %12.0f\n", art_initiations))
    
    cat("\n--- End-of-year cascade (post-mortality) ---\n")
    cat(sprintf("  end_plhiv               : %12.0f\n", end_plhiv))
    cat(sprintf("  end_diagnosed           : %12.0f\n", end_diagnosed))
    cat(sprintf("  end_on_art              : %12.0f\n", end_on_art))
    cat(sprintf("  end_suppressed          : %12.0f\n", end_suppressed))
    cat(sprintf("  effective_on_art        : %12.0f  (= on_art - ltfu_new_effective)\n",
                effective_on_art))
    
    cat("\n--- Pre-mortality cascade allocation ---\n")
    cat(sprintf("  end_diagnosed_pre_mort  : %12.0f\n", end_diagnosed_pre_mort))
    cat(sprintf("  end_on_art_pre_mort     : %12.0f\n", end_on_art_pre_mort))
    cat(sprintf("  end_suppressed_pre_mort : %12.0f  (derived from per-group allocation)\n",
                end_suppressed_pre_mort))
    cat(sprintf("  additional_suppressed   : %12.0f  (sum of all suppression gain flows)\n",
                additional_suppressed))
    cat(sprintf("  intervention_supp_shift : %12.0f  (additional shift above per-group base)\n",
                intervention_supp_shift))
    cat(sprintf("    shift_to_est          : %12.0f\n", intervention_shift_to_est))
    cat(sprintf("    shift_to_new          : %12.0f\n", intervention_shift_to_new))
    cat("\n  Base allocation (per-group rates):\n")
    cat(sprintf("    n_est_supp_base       : %12.0f  (= %d × pct_supp %.3f)\n",
                n_est_supp_base, round(n_established_on_art), pct_supp_frac))
    cat(sprintf("    n_est_treated_base    : %12.0f\n", n_est_treated_base))
    cat(sprintf("    n_new_supp_base       : %12.0f  (= %d × testing_init_supp %.3f)\n",
                n_new_supp_base, round(n_new_initiations), testing_init_supp_frac))
    cat(sprintf("    n_new_treated_base    : %12.0f\n", n_new_treated_base))
    cat("\n  Final allocation (base + intervention shift):\n")
    cat(sprintf("  n_undiagnosed           : %12.0f\n", n_undiagnosed))
    cat(sprintf("  n_diagnosed_not_art     : %12.0f\n", n_diagnosed_not_art))
    cat(sprintf("  n_new_initiations       : %12.0f\n", n_new_initiations))
    cat(sprintf("  n_established_on_art    : %12.0f\n", n_established_on_art))
    cat(sprintf("  n_established_supp      : %12.0f\n", n_established_supp))
    cat(sprintf("  n_established_treated   : %12.0f\n", n_established_treated))
    cat(sprintf("  n_new_supp              : %12.0f\n", n_new_supp))
    cat(sprintf("  n_new_treated           : %12.0f\n", n_new_treated))
    
    cat("\n--- Cascade ratios ---\n")
    pct_dx_end <- if (end_plhiv > 0) end_diagnosed / end_plhiv * 100 else 0
    pct_art_end <- if (end_diagnosed > 0) end_on_art / end_diagnosed * 100 else 0
    pct_sup_end <- if (end_on_art > 0) end_suppressed / end_on_art * 100 else 0
    cat(sprintf("  1st 95 (diagnosed/PLHIV): %10.2f%%\n", pct_dx_end))
    cat(sprintf("  2nd 95 (on_art/dx)     : %10.2f%%\n", pct_art_end))
    cat(sprintf("  3rd 95 (supp/on_art)   : %10.2f%%\n", pct_sup_end))
    
    cat("\n--- Deaths breakdown ---\n")
    cat(sprintf("  deaths_undiagnosed      : %12.0f\n", deaths_undiagnosed))
    cat(sprintf("  deaths_diagnosed_not_art: %12.0f\n", deaths_diagnosed_not_art))
    cat(sprintf("  deaths_new_initiations  : %12.0f\n", deaths_new_initiations))
    cat(sprintf("  deaths_est_treated      : %12.0f\n", deaths_established_treated))
    cat(sprintf("  deaths_est_suppressed   : %12.0f\n", deaths_established_supp))
    cat(sprintf("  total_hiv_deaths        : %12.0f\n", total_hiv_deaths))
    cat(sprintf("  calibration_factor      : %12.4f\n", mortality_calibration_factor))
    cat("=====================================================\n\n")
  }
  
  # ========================================================================
  # RETURN ALL OUTCOMES
  # ========================================================================
  
  list(
    # Testing outcomes
    tests_performed = round(tests_performed),
    positive_tests = round(positive_tests),
    test_positivity_rate = ifelse(tests_performed > 0,
                                  round((positive_tests / tests_performed) * 100, 2), 0),
    new_diagnoses = round(new_diagnoses),
    re_engagement = round(re_engagement),
    
    # Treatment outcomes
    art_initiations = round(art_initiations),
    additional_suppressed = round(additional_suppressed),  # exposed for baseline→scenario delta
    # LTFU flow outputs (replacing old single retention_improvement)
    ltfu_new_effective  = round(ltfu_new_effective),   # net people lost to follow-up
    ltfu_prevented      = round(ltfu_prevented),        # prevented from dropping off by MMD/adherence
    ltfu_reengaged      = round(ltfu_reengaged),        # re-engaged by tracking/tracing
    suppressed_ltfu     = round(stable_ltfu),        # stable patients lost (cascade impact)
    unsuppressed_ltfu   = round(unstable_ltfu),       # unstable patients lost
    
    # Health outcomes (infections/deaths averted)
    adult_infections_averted = round(infections_averted),
    infant_infections_averted = round(infant_infections_averted),
    total_infections_averted = round(infections_averted + infant_infections_averted),
    deaths_averted = round(total_deaths_averted),
    
    # MTCT cascade outputs
    mtct_pregnant_suppressed   = round(mtct_supp),
    mtct_pregnant_unsuppressed = round(mtct_unsupp),
    mtct_pregnant_no_art       = round(mtct_no_art),
    pmtct_newly_diagnosed      = round(pmtct_linked_total),
    pmtct_newly_linked         = round(pmtct_linked_art),
    pmtct_newly_suppressed     = round(pmtct_linked_supp),
    eid_infants_diagnosed      = round(eid_infants_diagnosed),
    
    # Infant mortality cascade outputs
    infant_on_art              = round(infant_on_art),
    infant_suppressed          = round(infant_suppressed),
    infant_deaths_averted      = round(infant_deaths_averted),
    total_infant_deaths        = round(total_infant_deaths),
    
    # End-of-year cascade (absolute values after mortality)
    end_plhiv = round(end_plhiv),
    end_diagnosed = round(end_diagnosed),
    end_on_art = round(end_on_art),
    end_suppressed = round(end_suppressed),
    
    # Mortality breakdown
    deaths_undiagnosed = round(deaths_undiagnosed),
    deaths_diagnosed_not_art = round(deaths_diagnosed_not_art),
    deaths_new_initiations = round(deaths_new_initiations),
    deaths_established_treated = round(deaths_established_treated),
    deaths_established_suppressed = round(deaths_established_supp),
    mortality_calibration_factor = mortality_calibration_factor,
    total_hiv_deaths_before_interventions = round(total_hiv_deaths + total_deaths_averted),
    
    # End-of-year epidemiological outcomes (absolute values)
    end_new_infections = round(end_new_infections),
    end_infant_infections = round(end_infant_infections + infant_infections_acute_bf),
    end_infant_infections_cascade  = round(end_infant_infections),
    end_infant_infections_acute_bf = round(infant_infections_acute_bf),
    maternal_infections_bf         = round(maternal_infections_bf),
    end_total_infections = round(end_new_infections + end_infant_infections +
                                   infant_infections_acute_bf),
    end_deaths = round(end_deaths),
    
    # Costs
    total_intervention_cost = round(total_intervention_cost),
    art_provision_cost = round(art_provision_cost),
    total_cost = round(total_cost)
  )
}

# ============================================================================
# CALCULATE DIFFERENCES BETWEEN SCENARIOS
# ============================================================================
calculate_scenario_difference <- function(scenario, baseline) {
  list(
    # Cascade differences
    diff_diagnosed   = scenario$end_diagnosed  - baseline$end_diagnosed,
    diff_on_art      = scenario$end_on_art     - baseline$end_on_art,
    diff_suppressed  = scenario$end_suppressed - baseline$end_suppressed,
    
    # Testing differences
    diff_tests_performed = scenario$tests_performed - baseline$tests_performed,
    diff_positive_tests  = scenario$positive_tests  - baseline$positive_tests,
    diff_new_diagnoses   = scenario$new_diagnoses   - baseline$new_diagnoses,
    diff_art_initiations = scenario$art_initiations - baseline$art_initiations,
    
    # LTFU flow differences
    diff_ltfu_new_effective = scenario$ltfu_new_effective - baseline$ltfu_new_effective,
    diff_ltfu_prevented     = scenario$ltfu_prevented     - baseline$ltfu_prevented,
    diff_ltfu_reengaged     = scenario$ltfu_reengaged     - baseline$ltfu_reengaged,
    
    # Epidemiological differences
    diff_new_infections    = scenario$end_new_infections    - baseline$end_new_infections,
    diff_infant_infections = scenario$end_infant_infections - baseline$end_infant_infections,
    diff_total_infections  = scenario$end_total_infections  - baseline$end_total_infections,
    diff_deaths            = scenario$end_deaths            - baseline$end_deaths,
    
    # Percentage change vs baseline (NA-safe; 0 baseline -> NA)
    pct_art_initiations = ifelse(baseline$art_initiations  == 0, NA,
                                 (scenario$art_initiations - baseline$art_initiations) / baseline$art_initiations * 100),
    pct_new_infections  = ifelse(baseline$end_new_infections == 0, NA,
                                 (scenario$end_new_infections - baseline$end_new_infections) / baseline$end_new_infections * 100),
    pct_deaths          = ifelse(baseline$end_deaths == 0, NA,
                                 (scenario$end_deaths - baseline$end_deaths) / baseline$end_deaths * 100),
    pct_suppressed      = ifelse(baseline$end_suppressed == 0, NA,
                                 (scenario$end_suppressed - baseline$end_suppressed) / baseline$end_suppressed * 100),
    
    # Infections/deaths averted (relative to baseline)
    additional_infections_averted = -((scenario$end_new_infections - baseline$end_new_infections) +
                                        (scenario$end_infant_infections - baseline$end_infant_infections)),
    additional_deaths_averted     = -(scenario$end_deaths - baseline$end_deaths),
    
    # Cost differences
    diff_intervention_cost  = scenario$total_intervention_cost - baseline$total_intervention_cost,
    diff_art_provision_cost = scenario$art_provision_cost      - baseline$art_provision_cost,
    diff_total_cost         = scenario$total_cost              - baseline$total_cost,
    
    # For display: scale up vs scale down
    scale_up_cost      = max(0, scenario$total_intervention_cost - baseline$total_intervention_cost),
    scale_down_savings = max(0, baseline$total_intervention_cost - scenario$total_intervention_cost)
  )
}

# ============================================================================
# RUN FUNCTIONS TO INITIALIZE
# ============================================================================
intervention_params <- load_intervention_params()
intervention_groups <- build_intervention_groups(intervention_params)
regional_presets    <- build_country_presets(country_data_csv, baseline_csv = baseline_data_csv)