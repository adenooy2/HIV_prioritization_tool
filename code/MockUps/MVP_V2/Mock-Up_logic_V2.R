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

# Null-coalescing helper used throughout FOI module
`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

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




# ============================================================================
# MORTALITY RATES BY CASCADE STAGE (UPDATE THESE BASED ON LITERATURE)
# ============================================================================
MORTALITY_RATES <- list(
  untreated_undiagnosed = hiv_params$mortality_untreated_undiagnosed,  # Average 350-500
  new_art_initiations   = hiv_params$mortality_new_art_initiations,  # first year on ART (pre-stabilisation)
  treated               = hiv_params$mortality_treated, # established on ART, not virally suppressed
  suppressed            = hiv_params$mortality_suppressed, # established on ART, virally suppressed
  ahd_untreated         = hiv_params$mortality_ahd_untreated,  # AHD (CD4<200) among undiagnosed / diagnosed not on ART
  ahd_treated           = hiv_params$mortality_ahd,  # AHD (CD4<200) among those on ART (new init or established)
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
# Stable patients: established, clinically stable,assume 10% less than virally suppressed

#   DSD-eligible — lower dropout. Rate calibrated from 3MMD conventional care
#   retention (86.4%) in Matsimela et al. 2024: 1 - 0.864 = 13.6%.
# Unstable patients (remaining 15% of on_art): treatment-struggling, side effects,
#   not yet DSD-eligible — higher dropout. Rate calibrated from "not eligible for
#   DSD" retention (77.7%): 1 - 0.777 = 22.3%.
# New initiates are excluded from this LTFU calculation — their dropout is
#   implicitly captured upstream via linkage rates (<100%) in the testing cascade.
# ============================================================================
ANNUAL_LTFU_RATE_STABLE   <- 0.136  # 13.6% of stable on-ART patients become LTFU per year
ANNUAL_LTFU_RATE_UNSTABLE <- 0.223  # 22.3% of unstable on-ART patients become LTFU per year

# ============================================================================
# SPONTANEOUS RE-ENGAGEMENT RATE (###UPDATE based on literature)
# Annual proportion of the LTFU pool that returns to care without any explicit
# tracking/tracing intervention. Captures silent transfers (patients classified
# as LTFU at one facility but receiving care elsewhere) and self-initiated
# re-engagement. Literature suggests ~12-15% of those classified as LTFU are
# silent transfers (Chammartin et al. 2018, Tiendrebeogo et al. 2021), plus
# additional self-initiated return documented in Zambian tracing cohorts
# (Beres et al. 2020). A conservative 15% annual return rate is used.
# ============================================================================
ANNUAL_SPONTANEOUS_REENGAGEMENT_RATE <- 0.15


# Proportion of *retained* unsuppressed patients who achieve viral suppression
# as a direct result of the improved adherence that prevented their dropout.
# i.e. the intervention both keeps them in care AND improves their adherence enough
# to suppress. ###UPDATE based on literature
RETENTION_SUPPRESSION_RATE <- 0.4

# ============================================================================
# MTCT RATES BY MATERNAL ART/SUPPRESSION STATUS
# Covers transmission risk across pregnancy, delivery, and breastfeeding period.
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
ACTS_PER_YEAR_HIGH        <- 100   # KP / high-concurrency
ACTS_PER_YEAR_GEN         <- 50    # general population
CONDOM_USE_RATE_HIGH      <- 0.75
CONDOM_USE_RATE_GEN       <- 0.55


# Targeting of CD4 tests toward suspected AHD cases.
# Fraction of CD4 tests performed that land on patients who actually have AHD,
# at low/moderate coverage. Reflects clinical triage (WHO stage, BMI, symptoms).
# Capped automatically by the AHD pool size, so at high coverage the effective
# yield converges to prop_ahd$new_initiations.
CD4_AHD_TARGETING_YIELD <- 0.40   # e.g. 40% of CD4 tests are on true AHD cases


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
          eligible_pop = "high_risk_negative",
          unit_cost = subset(intervention_params, intervention_key == "prep_oral")$unit_cost,
          outcomes = c("adult_infections")
        ),
        prep_lenacapavir = list(
          name = "PrEP (Lenacapavir)",
          type = "absolute",
          unit_label = "people",
          efficacy = subset(intervention_params, intervention_key == "prep_lenacapavir")$efficacy,
          eligible_pop = "high_risk_negative",
          unit_cost = subset(intervention_params, intervention_key == "prep_lenacapavir")$unit_cost,
          outcomes = c("adult_infections")
        ),
        vmmc = list(
          name = "VMMC",
          type = "absolute",
          unit_label = "annual people",
          efficacy = subset(intervention_params, intervention_key == "vmmc")$efficacy,
          eligible_pop = "uncircumcised_males",
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
        pep = list(
          name = "PEP",
          type = "absolute",
          unit_label = "people",
          efficacy = subset(intervention_params, intervention_key == "pep")$efficacy,
          eligible_pop = "recent_exposure",
          unit_cost = subset(intervention_params, intervention_key == "pep")$unit_cost,
          outcomes = c("adult_infections")
        ),
        infant_prophylaxis = list(
          name = "Infant prophylaxis",
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
        fast_track = list(
          name = "Fast-track",
          type = "coverage",
          unit_label = "% of stable clients",
          efficacy = subset(intervention_params, intervention_key == "fast_track")$efficacy,
          eligible_pop = "on_art_stable",
          unit_cost =  subset(intervention_params, intervention_key == "fast_track")$unit_cost,     
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
          name = "Adherence counseling/psychosocial support",
          type = "coverage",
          unit_label = "% of people on ART",
          efficacy = subset(intervention_params, intervention_key == "adherence_counseling")$efficacy,
          eligible_pop = "on_art",
          unit_cost = subset(intervention_params, intervention_key == "adherence_counseling")$unit_cost,
          outcomes = c("retention")
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
  sexually_active <- context$total_population * hiv_params$sexually_active_frac
  births <- (context$total_population * context$birth_rate)/1000
  hiv_positive_births <- births * context$hiv_prevalence * 1.5
  
  # Safe defaults — prevents NULL propagation if CSV columns are missing/misnamed.
  # prop_pop_male drives uncircumcised_males and all FOI strata; if NULL, vmmc
  # baseline and FOI strata would silently become NULL and display as 0.
  prop_male_pct <- if (!is.null(context$prop_pop_male) && !is.na(context$prop_pop_male))
    context$prop_pop_male else hiv_params$default_prop_pop_male
  prop_under14  <- if (!is.null(context$prop_pop_under_14) && !is.na(context$prop_pop_under_14))
    context$prop_pop_under_14 else hiv_params$default_prop_pop_under_14
  circ_prev     <- if (!is.null(context$circ_prevalence) && !is.na(context$circ_prevalence))
    context$circ_prevalence/100 else hiv_params$default_circ_prevalence
  prop_hr       <- if (!is.null(context$prop_high_risk) && !is.na(context$prop_high_risk))
    context$prop_high_risk else hiv_params$default_prop_high_risk
  
  # LTFU flow: people dropping off ART during the year, split by stability status.
  # Stable patients (on_art × 0.85): DSD-eligible, lower dropout risk.
  # Unstable patients (on_art × 0.15): not DSD-eligible, higher dropout risk.
  # New initiates excluded — their dropout is captured upstream via linkage rates.
  on_art_stable_n   <- on_art * ((context$percent_suppressed+hiv_params$prop_on_art_stable_diff) / 100)
  on_art_unstable_n <- on_art-on_art_stable_n
  ltfu_new_stable   <- on_art_stable_n   * ANNUAL_LTFU_RATE_STABLE
  ltfu_new_unstable <- on_art_unstable_n * ANNUAL_LTFU_RATE_UNSTABLE
  
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
    # Prevalent LTFU stock (people already lost before the year begins)
    ltfu = on_art * 0.15,
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
    sexually_active_negative = hiv_negative * hiv_params$sexually_active_frac,
    recent_exposure = hiv_negative * 0.002,
    hiv_exposed_infants = hiv_positive_births,
    pregnant_women      = births,
    # PMTCT cascade sub-populations (denominator = births x hiv_prevalence)
    pregnant_hiv_pos_cascade     = births * context$hiv_prevalence,
    pregnant_on_art              = births * context$hiv_prevalence *
      (context$percent_diagnosed / 100) * (context$percent_on_art / 100),
    pregnant_on_art_suppressed   = births * context$hiv_prevalence *
      (context$percent_diagnosed / 100) * (context$percent_on_art / 100) *
      (context$percent_suppressed / 100),
    pregnant_on_art_unsuppressed = births * context$hiv_prevalence *
      (context$percent_diagnosed / 100) * (context$percent_on_art / 100) *
      (1 - context$percent_suppressed / 100),
    pregnant_not_on_art          = births * context$hiv_prevalence *
      (1 - (context$percent_diagnosed / 100) * (context$percent_on_art / 100)),
    pregnant_undiagnosed         = births * context$hiv_prevalence * (1 - context$percent_diagnosed / 100),
    # HIV testing eligible pool: HIV-negative + HIV+ undiagnosed pregnant women
    # (already-diagnosed HIV+ women are not re-offered an HIV test)
    pregnant_hiv_testable        = births * (1 - context$hiv_prevalence * (context$percent_diagnosed / 100))
  )
}

# ============================================================================
# DEFAULT BASELINE INTERVENTIONS
# ============================================================================
default_baseline_interventions <- list(
  prep_oral = 5000, prep_lenacapavir = 0, vmmc = 30000,
  condoms = 200000, pep = 2000, infant_prophylaxis = 70,
  test_facility_general = 25000,
  test_network = 3000, test_index = 2000, test_community = 20000,
  test_kpsti = 8000, hivst_facility = 10000, hivst_community = 5000,
  eid = 75, anc_hiv_testing = 88, pnc_hiv_testing = 70,
  vl_monitoring_routine = 60, 
  mmd_3month = 50, mmd_6month = 10, mmd_12month = 5, fast_track =5, community_pickup=5,
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
        new_infections_per_year = row$new_infections_per_year,
        current_diagnoses = row$current_diagnoses,
        plhiv=row$plhiv,
        percent_diagnosed = row$percent_diagnosed,
        percent_on_art = row$percent_on_art,
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
          as.numeric(row$prop_retest) else NULL
      )
      
      pops <- calculate_populations(context)
      
      default_baseline_interventions <- list(
        prep_oral = 0.01*pops$total, prep_lenacapavir = 0, vmmc = 0.01*pops$uncircumcised_males,
        condoms = 0.6*pops$total, pep = 0.2*pops$recent_exposure, infant_prophylaxis = 70,
        test_facility_general = round(0.134*pops$adult_pop, -4), 
        test_network = round(0.0024*pops$adult_pop, -4), 
        test_index = round(0.0024*pops$adult_pop, -4), 
        test_community = round(0.019*pops$adult_pop, -4),
        test_kpsti = round(0.005*pops$adult_pop, -4), 
        hivst_facility = round(0.0035*pops$adult_pop, -4), 
        hivst_community = round(0.0035*pops$adult_pop, -4),
        eid = 75, anc_hiv_testing = 88, pnc_hiv_testing = 70,
        vl_monitoring_routine = 60, 
        mmd_3month = 50, mmd_6month = 10, mmd_12month = 5, fast_track=5, community_pickup =5, 
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
  
  # Add Custom Country option
  custom_context <- list(
    total_population = 1000000,
    hiv_prevalence = 0.05,
    plhiv = NULL,
    percent_diagnosed = 80,
    percent_on_art = 75,
    percent_suppressed = 85,
    new_infections_per_year = 5000,
    aids_deaths_per_year = 1000,
    birth_rate = 24,
    prop_pop_male = 49,
    prop_pop_under_14 = 40
  )
  
  custom_pops <- calculate_populations(custom_context)
  
  custom_baseline <- list(
    prep_oral = 0.01*custom_pops$total, 
    prep_lenacapavir = 0, 
    vmmc = 0.01*custom_pops$uncircumcised_males,
    condoms = 0.6*custom_pops$total, 
    pep = 0.2*custom_pops$recent_exposure, 
    infant_prophylaxis = 70,
    test_facility_general = 0.05*custom_pops$adult_pop, 
    test_network = 0.005*custom_pops$adult_pop, 
    test_index = 0.005*custom_pops$adult_pop, 
    test_community = 0.04*custom_pops$adult_pop,
    test_kpsti = 0.02*custom_pops$adult_pop, 
    hivst_facility = 0.02*custom_pops$adult_pop, 
    hivst_community = 0.01*custom_pops$adult_pop,
    eid = 75, 
    anc_hiv_testing = 88, 
    pnc_hiv_testing = 70,
    vl_monitoring_routine = 60, 
    mmd_3month = 40, 
    mmd_6month = 20, 
    mmd_12month = 5,
    adherence_counseling = 55, 
    tracking_tracing = 40, 
    anc_vl_testing = 68,
    pnc_vl_testing = 0,
    cd4_testing = 92, 
    ahd_package = 88
  )
  
  presets[["Custom Country"]] <- list(
    description = "Enter your own parameters",
    context = custom_context,
    baseline = custom_baseline
  )
  
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
#   1. High-risk (KP, high-concurrency partners)    ~5% of HIV-negative sexually active
#   2. General female                                ~47.5% of HIV-negative sexually active
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
# Efficacies passed in via scenario_interventions$eff_* keys so they stay
# in sync with intervention_params (set in calculate_scenario_outcomes before calling).
# Multiplicative stacking prevents double-counting when interventions overlap.
compute_prevention_adjustments <- function(scenario_interventions, strata, populations, strata_params) {
  clip <- function(x) max(0, min(1, x))
  
  # Pull efficacies — with fallbacks if not supplied
  eff_prep_oral <- scenario_interventions$eff_prep_oral %||% 0.99
  eff_prep_len  <- scenario_interventions$eff_prep_len  %||% 1.00
  eff_condom    <- scenario_interventions$eff_condom    %||% 0.80
  eff_pep       <- scenario_interventions$eff_pep       %||% 0.80
  
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
  
  # ---- High-risk stratum: PrEP (oral + LEN) + condoms ----
  prep_oral_cov_high <- clip((scenario_interventions$prep_oral        %||% 0) / max(strata$n_high_risk, 1))
  prep_len_cov_high  <- clip((scenario_interventions$prep_lenacapavir %||% 0) / max(strata$n_high_risk, 1))
  
  residual_high   <- (1 - prep_oral_cov_high * eff_prep_oral) *
    (1 - prep_len_cov_high  * eff_prep_len)  *
    (1 - condom_cov_high    * eff_condom)
  protection_high <- 1 - residual_high
  
  # ---- General female: condoms + PEP ----
  condom_cov_gen_f <- condom_cov_gen
  pep_cov_gen_f    <- clip((scenario_interventions$pep %||% 0) * 0.5 / max(strata$n_general_female, 1))
  
  residual_gen_female   <- (1 - condom_cov_gen_f * eff_condom) *
    (1 - pep_cov_gen_f   * eff_pep)
  protection_gen_female <- 1 - residual_gen_female
  
  # ---- General uncircumcised male: condoms + PEP ----
  condom_cov_gen_mu <- condom_cov_gen
  pep_cov_gen_mu    <- clip((scenario_interventions$pep %||% 0) * 0.5 / max(strata$n_general_male_uncirc, 1))
  
  residual_gen_male_unc   <- (1 - condom_cov_gen_mu * eff_condom) *
    (1 - pep_cov_gen_mu   * eff_pep)
  protection_gen_male_unc <- 1 - residual_gen_male_unc
  
  # ---- General circumcised male: condoms + PEP ----
  # Circumcised men also use condoms; this stacks ON TOP of their lower β_circ
  # (which encodes biological circumcision protection only).
  # Without this, men transferred from the uncirc pool by VMMC lose their
  # condom coverage and can appear to gain MORE infections — fixed here.
  condom_cov_gen_mc <- condom_cov_gen
  pep_cov_gen_mc    <- clip((scenario_interventions$pep %||% 0) * 0.5 / max(strata$n_general_male_circ, 1))
  
  residual_gen_male_circ   <- (1 - condom_cov_gen_mc * eff_condom) *
    (1 - pep_cov_gen_mc    * eff_pep)
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
  # condom/PEP coverage applied on top via protection_gen_male_circ
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
  
  bounds <- list(
    beta_high = list(lower = 0.05, upper = 3.00, label = "High-risk (KP)"),
    beta_gen_female    = list(lower = 0.005, upper = 0.50, label = "General (female)"),
    beta_gen_male_unc  = list(lower = 0.003, upper = 0.40, label = "General (uncircumcised male)"),
    beta_gen_male_circ = list(lower = 0.001, upper = 0.20, label = "General (circumcised male)")
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
  total_intervention_cost <- 0
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
  
  
  average_linkage <- 0.9
  
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
  
  # Index testing: exempt from yield dilution (targeted contacts remain high-yield
  # regardless of total programme volume) but capped at 2x prior-year new infections
  # (finite contact pool — cannot meaningfully exceed the contacts of incident cases).
  index_pop_cap <- if (!is.null(context$new_infections_per_year) &&
                       !is.na(context$new_infections_per_year))
    2 * context$new_infections_per_year else Inf
  
  # Pre-loop pass: sum total planned tests across all testing modalities,
  # EXCLUDING index testing so it does not inflate the dilution denominator.
  total_planned_tests <- 0
  for (int_key_pre in names(all_interventions)) {
    int_pre     <- all_interventions[[int_key_pre]]
    int_val_pre <- interventions[[int_key_pre]]
    if (is.null(int_val_pre) || int_val_pre == 0) next
    if (!("testing" %in% int_pre$outcomes)) next
    if (int_key_pre == "test_index") next   # exempt: does not saturate general pool
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
      # Index testing: cap at 2x prior-year infections; never diluted (contact
      # pool quality does not degrade with general programme volume expansion).
      if (int_key == "test_index") {
        number_reached   <- min(number_reached, index_pop_cap)
        modality_dilution <- 1.0
      } else {
        modality_dilution <- yield_dilution_factor
      }
      
      # Effective yield: country-specific multiplier (from baseline CSV) takes
      # precedence; falls back to intervention params default, then 1.
      country_mult <- context$yield_multipliers[[int_key]]
      effective_yield <- base_test_yield *
        (if (!is.null(country_mult)) country_mult else 1)
      
      pos_tests <- number_reached * effective_yield * modality_dilution * intervention$efficacy
      positive_tests <- positive_tests + pos_tests
      tests_performed <- tests_performed + number_reached
      
      # Split into new diagnoses vs re-engagement based on pool composition
      new_dx <- pos_tests * prop_new_dx
      re_eng  <- pos_tests * prop_reeng
      
      # ANC/PNC HIV testing: general yield path suppressed for cascade metrics.
      # These women are routed into the adult cascade after the loop via the
      # PMTCT-specific yield (pmtct_new_diagnoses), which is more accurate than
      # the background positivity rate for this targeted population.
      # tests_performed still accrues here — the test is administered to all
      # women reached. Unit cost accrues here; linkage cost accrues in the
      # post-loop PMTCT routing block using the PMTCT-specific diagnosed count.
      if (!(int_key %in% c("anc_hiv_testing", "pnc_hiv_testing"))) {
        new_diagnoses         <- new_diagnoses         + new_dx
        re_engagement_testing <- re_engagement_testing + re_eng
        
        # ART initiations based on linkage rate
        linkage_rate <- intervention$linkage_rate
        linked <- pos_tests * linkage_rate
        art_inititations_testing <- art_inititations_testing + linked
        
        additional_suppressed_testing <- additional_suppressed_testing +
          linked * ((context$percent_suppressed * 0.9) / 100)
        
        # Full costs: unit cost per test + linkage cost per linked patient
        total_intervention_cost <- total_intervention_cost +
          (number_reached * intervention$unit_cost + linked * intervention$linkage_cost)
      } else {
        # ANC/PNC: unit cost per test only; linkage cost charged in post-loop block
        total_intervention_cost <- total_intervention_cost +
          number_reached * intervention$unit_cost
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
                                      (number_reached / max(populations$hiv_exposed_infants, 1)) * intervention$efficacy)
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
      
    } else if ("infant_diagnosis" %in% intervention$outcomes) {
      # EID: tests HIV-exposed infants to identify HIV+ infants for early ART initiation.
      # Cost calculated post-cascade using actual yield — see MTCT cascade block below.
      eid_infants_reached <- number_reached
      
    } else if ("viral_suppression" %in% intervention$outcomes) {
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
      # MMD / adherence_counseling (eligible_pop != "ltfu"): prevents people
      #   from becoming LTFU in the first place
      if (intervention$eligible_pop == "ltfu") {
        ltfu_reengaged <- ltfu_reengaged + number_reached * intervention$efficacy
      } else {
        # Prevention interventions: COST applies to everyone reached (all on ART),
        # but EFFECT can only act on the at-risk fraction (those who would drop out).
        # Two combination rules depending on whether interventions can overlap:
        #
        # ADDITIVE — DSD options (eligible_pop == "on_art_stable"):
        #   mmd_3month / mmd_6month / mmd_12month / fast_track / community_pickup
        #   are mutually exclusive — the UI enforces they sum to ≤100% of on_art_stable,
        #   so no person can be enrolled in two DSD slots. Simple addition is correct:
        #     ltfu_retained_frac += coverage_frac * efficacy
        #
        # MULTIPLICATIVE — overlapping interventions (eligible_pop == "on_art"):
        #   e.g. adherence_counseling targets the full on_art pool and CAN overlap
        #   with DSD patients. Multiplicative combination prevents double-counting:
        #     marginal_retained = (1 - ltfu_retained_frac) * coverage_frac * efficacy
        coverage_frac <- ifelse(
          populations$on_art > 0,
          number_reached / populations$on_art,
          0
        )
        if (intervention$eligible_pop == "on_art_stable") {
          # Mutually exclusive DSD: additive
          ltfu_retained_frac <- ltfu_retained_frac + coverage_frac * intervention$efficacy
        } else {
          # Overlapping interventions (e.g. adherence_counseling): multiplicative
          marginal_retained  <- (1 - ltfu_retained_frac) * coverage_frac * intervention$efficacy
          ltfu_retained_frac <- ltfu_retained_frac + marginal_retained
        }
      }
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
      
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
  # Suppression discount: 70% of country average — newly initiating pregnant
  # women are less likely to suppress quickly. ###UPDATE from literature.
  # ========================================================================
  pmtct_cascade_linkage_rate <- all_interventions$anc_hiv_testing$linkage_rate %||% 0.85
  pmtct_cascade_supp_rate    <- (context$percent_suppressed / 100) * 0.70
  pmtct_cascade_linked_art   <- pmtct_new_diagnoses * pmtct_cascade_linkage_rate
  pmtct_cascade_linked_supp  <- pmtct_cascade_linked_art * pmtct_cascade_supp_rate
  
  new_diagnoses                 <- new_diagnoses                 + pmtct_new_diagnoses
  art_inititations_testing      <- art_inititations_testing      + pmtct_cascade_linked_art
  additional_suppressed_testing <- additional_suppressed_testing + pmtct_cascade_linked_supp
  
  # Linkage cost for PMTCT-linked women (unit cost already charged in loop above)
  total_intervention_cost <- total_intervention_cost +
    pmtct_cascade_linked_art * (all_interventions$anc_hiv_testing$linkage_cost %||% 0)
  
  # ========================================================================
  # APPLY CONSTRAINTS - CAP AT REALISTIC MAXIMUMS
  # ========================================================================
  
  # Cannot diagnose more people than 95% are undiagnosed
  new_diagnoses <- min(new_diagnoses, populations$undiagnosed * 0.95)
  
  # ── LTFU PREVENTION: convert retained fraction to people ─────────────────
  # ltfu_retained_frac is the multiplicatively-combined fraction of the at-risk
  # on-ART population that prevention interventions will retain.
  # Apply to the incident LTFU pool (those actually at risk of dropping out).
  ltfu_retained_frac <- min(ltfu_retained_frac, 1)  # cap at 100%
  ltfu_prevented     <- populations$ltfu_new * ltfu_retained_frac
  
  # ── LTFU FLOW ─────────────────────────────────────────────────────────────
  # Prevention interventions reduce incident LTFU, split proportionally across
  # the stable and unstable dropout sub-groups.
  # If ltfu_new == 0 (no dropout), prevention has nothing to act on.
  prop_ltfu_stable <- ifelse(
    populations$ltfu_new > 0,
    populations$ltfu_new_stable / populations$ltfu_new,
    0
  )
  prop_ltfu_unstable <- 1 - prop_ltfu_stable
  
  ltfu_prevented_stable   <- ltfu_prevented * prop_ltfu_stable
  ltfu_prevented_unstable <- ltfu_prevented * prop_ltfu_unstable
  
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
  
  # Programmatic re-engagement (testing + tracking/tracing) competes for the
  # remainder of the 95% cap, after spontaneous returns are reserved.
  programmatic_cap <- max(0, total_ltfu_pool * 0.95 - spontaneous_reengaged)
  
  # Testing re-engagement draws from the programmatic share first;
  # tracking/tracing takes from whatever remains.
  re_engagement_testing <- min(re_engagement_testing, programmatic_cap)
  re_engagement         <- re_engagement_testing
  positive_tests        <- new_diagnoses + re_engagement_testing
  
  ltfu_reengaged <- min(ltfu_reengaged,
                        max(0, programmatic_cap - re_engagement_testing))
  
  # Spontaneous returns are folded into ltfu_reengaged for downstream cascade
  # accounting (they re-enter on_art the same way tracking/tracing returnees do).
  ltfu_reengaged <- ltfu_reengaged + spontaneous_reengaged
  # Suppression gain from re-engaged patients (tracking/tracing).
  # Re-engaged patients return to ART; those who were previously suppressed
  # are assumed to re-achieve suppression at 80% of the background rate
  # (lower than the 90% used for new testing initiations, reflecting the
  # greater disruption of a period out of care). ###UPDATE
  additional_suppressed <- additional_suppressed +
    ltfu_reengaged * ((context$percent_suppressed * 0.8) / 100)
  
  # ── ART INITIATIONS ───────────────────────────────────────────────────────
  art_inititations_testing <- min(art_inititations_testing,
                                  average_linkage * (new_diagnoses + re_engagement_testing))
  art_initiations <- art_inititations_testing + art_initiations
  
  # Additional suppressed from testing
  additional_suppressed_testing <- min(
    art_initiations * ((context$percent_suppressed * 0.9) / 100),
    additional_suppressed_testing
  )
  additional_suppressed <- additional_suppressed + additional_suppressed_testing
  
  # Cannot initiate more on ART than are diagnosed but not yet on ART
  # (accounting for on_art being reduced by net LTFU losses)
  effective_on_art <- populations$on_art - ltfu_new_effective
  max_art_initiations <- populations$diagnosed + new_diagnoses - effective_on_art + re_engagement
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
  end_suppressed_pre_mort <- min(max(populations$suppressed - stable_ltfu + additional_suppressed, 0),
                                 end_on_art_pre_mort)
  
  # ========================================================================
  # FIVE CASCADE GROUPS (mutually exclusive, before mortality)
  # ========================================================================
  
  n_undiagnosed        <- max(0, populations$plhiv - end_diagnosed_pre_mort)
  n_diagnosed_not_art  <- max(0, end_diagnosed_pre_mort - end_on_art_pre_mort)
  n_new_initiations    <- min(art_initiations, end_on_art_pre_mort)
  n_established_on_art <- max(0, end_on_art_pre_mort - n_new_initiations)
  # Suppression split: established patients first, then new initiates absorb any remainder.
  # Previously all suppression was attributed to established patients, causing end_suppressed
  # to be capped at n_established_on_art even when new initiates also achieved suppression —
  # producing a disconnect where infections changed (via suppression_delta) but
  # end_suppressed did not (overflow was silently discarded).
  n_established_supp   <- min(end_suppressed_pre_mort, n_established_on_art)
  n_established_treated<- max(0, n_established_on_art - n_established_supp)
  # Suppressed new initiates: suppression that could not fit in the established bucket
  n_new_supp           <- max(0, min(end_suppressed_pre_mort - n_established_supp, n_new_initiations))
  n_new_treated        <- max(0, n_new_initiations - n_new_supp)
  
  # ========================================================================
  # EFFECTIVE AHD MORTALITY RATES (intervention-adjusted where applicable)
  # ========================================================================
  prop_ahd <- MORTALITY_RATES$prop_ahd
  # Untreated groups: no interventions reach them; use untreated AHD rate
  eff_base_rate_untreated <- MORTALITY_RATES$untreated_undiagnosed
  eff_ahd_rate_untreated  <- MORTALITY_RATES$ahd_untreated
  
  # New initiations: use treated AHD rate; AHD package reduces it; base rate unchanged
  eff_base_rate_new_init <- MORTALITY_RATES$new_art_initiations
  eff_ahd_rate_new_init  <- MORTALITY_RATES$ahd_treated *
    (1 - ahd_pkg_eff_reduction)
  
  # Established on ART: use treated AHD rate; AHD package reduces it; base rates unchanged
  eff_ahd_rate_established <- MORTALITY_RATES$ahd_treated *
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
  
  if (is_baseline) {
    if (USE_MORTALITY_CALIBRATION &&
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
    calc_deaths(n_new_initiations,     MORTALITY_RATES$new_art_initiations, MORTALITY_RATES$ahd_treated, prop_ahd$new_initiations) +
    calc_deaths(n_established_treated, MORTALITY_RATES$treated,             MORTALITY_RATES$ahd_treated, prop_ahd$established_treated) +
    calc_deaths(n_established_supp,    MORTALITY_RATES$suppressed,          MORTALITY_RATES$ahd_treated, prop_ahd$established_supp)
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
  
  # suppression_delta = MARGINAL change in suppression above the baseline
  # treatment programme. Positive when a scenario scales suppression above
  # baseline (reduces infectious pressure); NEGATIVE when a scenario scales
  # testing/VL/linkage down below baseline (raises infectious pressure and
  # therefore infections). At baseline this is zero by construction.
  # β was calibrated using the observed cascade state (percent_suppressed),
  # which already reflects the baseline programme, so only marginal change
  # is applied here — preventing double-counting.
  suppression_delta <- if (is_baseline) {
    0
  } else {
    additional_suppressed - baseline_additional_suppressed   # allow negative
  }
  
  # Pass efficacies from intervention_params into FOI so they stay in sync
  foi_interventions <- c(
    interventions,
    list(
      eff_prep_oral = all_interventions$prep_oral$efficacy       %||% 0.99,
      eff_prep_len  = all_interventions$prep_lenacapavir$efficacy %||% 1.00,
      eff_condom    = all_interventions$condoms$efficacy          %||% 0.80,
      eff_pep       = all_interventions$pep$efficacy              %||% 0.80,
      acts_per_year_high     = ACTS_PER_YEAR_HIGH,    
      acts_per_year_gen      = ACTS_PER_YEAR_GEN,     
      condom_use_rate_high   = CONDOM_USE_RATE_HIGH,    
      condom_use_rate_gen    = CONDOM_USE_RATE_GEN    
    )
  )
  
  foi_result         <- estimate_new_infections_foi(
    context                = context,
    populations            = populations,
    scenario_interventions = foi_interventions,
    suppression_delta      = suppression_delta,
    baseline_interventions = baseline_interventions
  )
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
  if (!cal_check$valid) {
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
      # Cost only — FOI accounts for protective effect
      units_costed <- if (int_key == "condoms")
        (intervention_value %||% 0)
      else
        number_reached
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
  # are less likely to fully suppress quickly. ###UPDATE discount factor from literature.
  pmtct_linkage_rate  <- all_interventions$anc_hiv_testing$linkage_rate %||% 0.85
  pmtct_supp_rate     <- (context$percent_suppressed / 100) * 0.70  ###UPDATE discount
  
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
  
  # Step 4: Infant prophylaxis (NVP) further reduces residual transmission.
  # Efficacy drawn from intervention CSV, consistent with all other interventions.
  infant_prophy_reduction   <- baseline_infant_infections * infant_prophy_cov_frac *
    (all_interventions$infant_prophylaxis$efficacy %||% 0)
  end_infant_infections     <- max(0, baseline_infant_infections - infant_prophy_reduction)
  infant_infections_averted <- infant_prophy_reduction
  
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
  # Linkage cost: only HIV+ infants identified and linked to ART
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
  infant_untreated     <- max(0, end_infant_infections - infant_on_art)
  
  # Deaths by infant treatment group
  infant_deaths_suppressed  <- infant_suppressed    * INFANT_MORTALITY_RATES$suppressed
  infant_deaths_on_art      <- infant_on_art_unsupp * INFANT_MORTALITY_RATES$on_art
  infant_deaths_untreated   <- infant_untreated     * INFANT_MORTALITY_RATES$untreated
  total_infant_deaths       <- infant_deaths_suppressed + infant_deaths_on_art +
    infant_deaths_untreated
  
  # Infant deaths averted vs no-EID counterfactual (all infected infants untreated)
  infant_deaths_averted <- max(0,
                               end_infant_infections * INFANT_MORTALITY_RATES$untreated - total_infant_deaths
  )
  
  # Wire into adult totals
  total_deaths_averted <- total_deaths_averted + infant_deaths_averted
  end_deaths           <- end_deaths           + total_infant_deaths
  
  # ========================================================================
  # CALCULATE COSTS
  # ========================================================================
  
  # ART provision cost (outcome-driven)
  art_provision_cost <- end_on_art * 200
  
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
    end_infant_infections = round(end_infant_infections),
    end_total_infections = round(end_new_infections + end_infant_infections),
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
