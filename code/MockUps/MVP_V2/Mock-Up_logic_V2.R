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

# ============================================================================
# MORTALITY RATES BY CASCADE STAGE (UPDATE THESE BASED ON LITERATURE)
# ============================================================================
MORTALITY_RATES <- list(
  untreated_undiagnosed = 0.10,  # undiagnosed PLHIV + diagnosed not on ART
  new_art_initiations   = 0.06,  # first year on ART (pre-stabilisation)
  treated               = 0.008, # established on ART, not virally suppressed
  suppressed            = 0.003, # established on ART, virally suppressed
  ahd                   = 0.20,  # advanced HIV disease (CD4 < 200), any stage
  prop_ahd = list(
    undiagnosed        = 0.20,   # undiagnosed PLHIV
    diagnosed_not_art  = 0.20,   # diagnosed but not on ART
    new_initiations    = 0.20,   # first year on ART
    established_treated= 0.00,   # established on ART, not suppressed
    established_supp   = 0.00    # established on ART, suppressed
  )
)

# ============================================================================
# LTFU RATES BY SUPPRESSION STATUS (UPDATE THESE BASED ON LITERATURE)
# Suppressed patients: stable, feel well, lower side-effect burden -> lower dropout
# Unsuppressed patients: treatment struggling, side effects, stigma -> higher dropout
# ============================================================================
ANNUAL_LTFU_RATE_SUPPRESSED   <- 0.05   # 5%  of suppressed on ART become LTFU per year
ANNUAL_LTFU_RATE_UNSUPPRESSED <- 0.15   # 15% of unsuppressed on ART become LTFU per year

# ============================================================================
# LOAD DATA
# ============================================================================
# Load country data
response <- GET("https://1drv.ms/x/c/2ae90f5cbd0fd171/IQBCFFlfF2AaRLcGuaCvNAcJAbE-8Ak2_gDyNJnL0GQu8Ag?e=k5dAU1&download=1")
country_data_csv <- content(response, as = "parsed", type = "text/csv")

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
  intervention_params$multiplier <- as.numeric(intervention_params$multiplier)
  
  return(intervention_params)
}

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
          name = "Condom availability",
          type = "absolute",
          unit_label = "people reached",
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
        test_facility_targeted = list(
          name = "Testing: facility-based (targeted)",
          type = "absolute",
          unit_label = "tests performed",
          efficacy = subset(intervention_params, intervention_key == "test_facility_targeted")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "test_facility_targeted")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "test_facility_targeted")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "test_facility_targeted")$linkage_cost,
          test_yield_multiplier = subset(intervention_params, intervention_key == "test_facility_targeted")$yield_multiplier,
          outcomes = c("testing")
        ),
        test_facility_general = list(
          name = "Testing: facility-based (general)",
          type = "absolute",
          unit_label = "tests performed",
          efficacy = subset(intervention_params, intervention_key == "test_facility_general")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "test_facility_general")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "test_facility_general")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "test_facility_general")$linkage_cost,
          test_yield_multiplier = subset(intervention_params, intervention_key == "test_facility_general")$yield_multiplier,
          outcomes = c("testing")
        ),
        test_network_index = list(
          name = "Testing: network/index testing",
          type = "absolute",
          unit_label = "tests performed",
          efficacy = subset(intervention_params, intervention_key == "test_network_index")$efficacy,
          eligible_pop = "total",
          unit_cost = subset(intervention_params, intervention_key == "test_network_index")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "test_network_index")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "test_network_index")$linkage_cost,
          test_yield_multiplier = subset(intervention_params, intervention_key == "test_network_index")$yield_multiplier,
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
          test_yield_multiplier = subset(intervention_params, intervention_key == "test_community")$yield_multiplier,
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
          test_yield_multiplier = subset(intervention_params, intervention_key == "test_kpsti")$yield_multiplier,
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
          test_yield_multiplier = subset(intervention_params, intervention_key == "hivst_facility")$yield_multiplier,
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
          test_yield_multiplier = subset(intervention_params, intervention_key == "hivst_community")$yield_multiplier,
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
          test_yield_multiplier = subset(intervention_params, intervention_key == "eid")$yield_multiplier,
          outcomes = c("testing")
        ),
        anc_hiv_testing = list(
          name = "ANC: HIV testing",
          type = "coverage",
          unit_label = "% of pregnant women",
          efficacy = subset(intervention_params, intervention_key == "anc_hiv_testing")$efficacy,
          eligible_pop = "pregnant_women",
          unit_cost = subset(intervention_params, intervention_key == "anc_hiv_testing")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "anc_hiv_testing")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "anc_hiv_testing")$linkage_cost,
          test_yield_multiplier = subset(intervention_params, intervention_key == "anc_hiv_testing")$yield_multiplier,
          outcomes = c("testing")
        ),
        pnc_hiv_testing = list(
          name = "PNC: HIV testing",
          type = "coverage",
          unit_label = "% of postpartum women",
          efficacy = subset(intervention_params, intervention_key == "pnc_hiv_testing")$efficacy,
          eligible_pop = "pregnant_women",
          unit_cost = subset(intervention_params, intervention_key == "pnc_hiv_testing")$unit_cost,
          linkage_rate = subset(intervention_params, intervention_key == "pnc_hiv_testing")$linkage_rate,
          linkage_cost = subset(intervention_params, intervention_key == "pnc_hiv_testing")$linkage_cost,
          test_yield_multiplier = subset(intervention_params, intervention_key == "pnc_hiv_testing")$yield_multiplier,
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
        cotrimoxazole = list(
          name = "Cotrimoxazole prophylaxis (according to guidelines)",
          type = "coverage",
          unit_label = "% of new ART initiations",
          efficacy = subset(intervention_params, intervention_key == "cotrimoxazole")$efficacy,
          eligible_pop = "new_art_initiations",
          unit_cost = subset(intervention_params, intervention_key == "cotrimoxazole")$unit_cost,
          outcomes = c("mortality")
        ),
        oi_management = list(
          name = "OI screening & management",
          type = "coverage",
          unit_label = "% of new ART initiations",
          efficacy = subset(intervention_params, intervention_key == "oi_management")$efficacy,
          eligible_pop = "new_art_initiations",
          unit_cost = subset(intervention_params, intervention_key == "oi_management")$unit_cost,
          outcomes = c("mortality")
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
  
  plhiv <- context$total_population * context$hiv_prevalence
  diagnosed <- plhiv * (context$percent_diagnosed/100)
  on_art <- diagnosed * (context$percent_on_art / 100)
  suppressed <- on_art * (context$percent_suppressed / 100)
  unsuppressed_on_art <- on_art - suppressed
  hiv_negative <- context$total_population - plhiv
  sexually_active <- context$total_population * 0.60
  births <- (context$total_population * context$birth_rate)/1000
  hiv_positive_births <- births * context$hiv_prevalence * 1.5
  
  # LTFU flow: people dropping off ART during the year, split by suppression status.
  # Suppressed patients have lower dropout (stable, feel well, fewer side effects).
  # Unsuppressed patients have higher dropout (side effects, treatment fatigue, stigma).
  ltfu_new_suppressed   <- suppressed       * ANNUAL_LTFU_RATE_SUPPRESSED
  ltfu_new_unsuppressed <- unsuppressed_on_art * ANNUAL_LTFU_RATE_UNSUPPRESSED
  
  list(
    total = context$total_population,
    adult_pop = context$total_population * (1 - context$prop_pop_under_14/100),
    plhiv = plhiv,
    hiv_negative = hiv_negative,
    sexually_active = sexually_active,
    undiagnosed = plhiv - diagnosed,
    diagnosed = diagnosed,
    diagnosed_not_on_art = diagnosed - on_art,
    on_art = on_art,
    on_art_stable = on_art * 0.85,
    on_art_suspected_failure = on_art * 0.08,
    suppressed = suppressed,
    unsuppressed = unsuppressed_on_art,
    # Prevalent LTFU stock (people already lost before the year begins)
    ltfu = on_art * 0.15,
    # Incident LTFU flow (people becoming LTFU during the year), by suppression status
    ltfu_new_suppressed   = ltfu_new_suppressed,
    ltfu_new_unsuppressed = ltfu_new_unsuppressed,
    ltfu_new              = ltfu_new_suppressed + ltfu_new_unsuppressed,
    high_risk_negative = hiv_negative * 0.05,
    uncircumcised_males = (hiv_negative * context$prop_pop_male/100) * 0.25,
    sexually_active_negative = (hiv_negative * 0.60),
    recent_exposure = hiv_negative * 0.002,
    hiv_exposed_infants = hiv_positive_births,
    pregnant_women = births,
    pregnant_on_art = births * context$hiv_prevalence * (context$percent_on_art / 100),
    newly_diagnosed_advanced = (plhiv - diagnosed) * 0.20
  )
}

# ============================================================================
# DEFAULT BASELINE INTERVENTIONS
# ============================================================================
default_baseline_interventions <- list(
  prep_oral = 5000, prep_lenacapavir = 0, vmmc = 30000,
  condoms = 200000, pep = 2000, infant_prophylaxis = 70,
  cotrimoxazole = 60,
  test_facility_targeted = 25000, test_facility_general = 25000,
  test_network_index = 5000, test_community = 20000,
  test_kpsti = 8000, hivst_facility = 10000, hivst_community = 5000,
  eid = 75, anc_hiv_testing = 88, pnc_hiv_testing = 70,
  vl_monitoring_routine = 60, 
  oi_management = 50, mmd_3month = 40, mmd_6month = 20, mmd_12month = 5,
  adherence_counseling = 55, tracking_tracing = 40, anc_vl_testing = 68,
  cd4_testing = 92, ahd_package = 88
)

# ============================================================================
# BUILD COUNTRY PRESETS FROM CSV
# ============================================================================
build_country_presets <- function(csv_data) {
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
        percent_diagnosed = row$percent_diagnosed,
        percent_on_art = row$percent_on_art,
        percent_suppressed = row$percent_suppressed,
        aids_deaths_per_year = row$aids_deaths_per_year,
        birth_rate = row$birth_rate,
        prop_pop_male = row$prop_male,
        prop_pop_under_14 = row$prop_under14
      )
      
      pops <- calculate_populations(context)
      
      default_baseline_interventions <- list(
        prep_oral = 0.01*pops$total, prep_lenacapavir = 0, vmmc = 0.01*pops$uncircumcised_males,
        condoms = 0.6*pops$total, pep = 0.2*pops$recent_exposure, infant_prophylaxis = 70,
        cotrimoxazole = 60, 
        test_facility_targeted = round(0.067*pops$adult_pop, -4), 
        test_facility_general = round(0.134*pops$adult_pop, -4), 
        test_network_index = round(0.0047*pops$adult_pop, -4), 
        test_community = round(0.019*pops$adult_pop, -4),
        test_kpsti = round(0.005*pops$adult_pop, -4), 
        hivst_facility = round(0.0035*pops$adult_pop, -4), 
        hivst_community = round(0.0035*pops$adult_pop, -4),
        eid = 75, anc_hiv_testing = 88, pnc_hiv_testing = 70,
        vl_monitoring_routine = 60, 
        oi_management = 50, mmd_3month = 40, mmd_6month = 20, mmd_12month = 5,
        adherence_counseling = 55, tracking_tracing = 40, anc_vl_testing = 68,
        cd4_testing = 92, ahd_package = 88
      )
      
      baseline <- default_baseline_interventions
      for (int_name in names(default_baseline_interventions)) {
        if (int_name %in% names(row)) {
          baseline[[int_name]] <- row[[int_name]]
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
    cotrimoxazole = 60, 
    test_facility_targeted = 0.05*custom_pops$adult_pop, 
    test_facility_general = 0.05*custom_pops$adult_pop, 
    test_network_index = 0.01*custom_pops$adult_pop, 
    test_community = 0.04*custom_pops$adult_pop,
    test_kpsti = 0.02*custom_pops$adult_pop, 
    hivst_facility = 0.02*custom_pops$adult_pop, 
    hivst_community = 0.01*custom_pops$adult_pop,
    eid = 75, 
    anc_hiv_testing = 88, 
    pnc_hiv_testing = 70,
    vl_monitoring_routine = 60, 
    oi_management = 50, 
    mmd_3month = 40, 
    mmd_6month = 20, 
    mmd_12month = 5,
    adherence_counseling = 55, 
    tracking_tracing = 40, 
    anc_vl_testing = 68,
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
# SCENARIO OUTCOMES CALCULATION - ABSOLUTE VALUES
# ============================================================================
calculate_scenario_outcomes <- function(context, interventions, populations) {
  
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
  # Retention: two distinct counters replacing the old single retention_improvement
  ltfu_prevented <- 0   # people kept on ART by prevention interventions (MMD, adherence)
  ltfu_reengaged <- 0   # LTFU people brought back by tracking/tracing
  total_intervention_cost <- 0
  tests_performed <- 0
  
  # Calculate dynamic testing yield
  # Yield = probability that a test is positive
  # This is based on undiagnosed (true new positives) + LTFU (re-engagement)
  base_test_yield <- ((populations$undiagnosed + populations$ltfu) / populations$sexually_active)
  base_test_yield <- min(base_test_yield, 0.1)  # Cap at 10% positivity for realism
  
  prop_new_dx <- 0.7 ###UPDATE
  prop_reeng  <- (1 - prop_new_dx) ###UPDATE
  
  average_linkage <- 0.9
  
  # Flatten intervention structure
  all_interventions <- list()
  for (group_name in names(intervention_groups)) {
    group <- intervention_groups[[group_name]]
    for (int_name in names(group$interventions)) {
      all_interventions[[int_name]] <- group$interventions[[int_name]]
    }
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
      # Testing interventions
      test_yield <- base_test_yield
      if (!is.null(intervention$test_yield_multiplier)) {
        test_yield <- test_yield * as.numeric(intervention$test_yield_multiplier)
      }
      
      pos_tests <- number_reached * test_yield * intervention$efficacy
      positive_tests <- positive_tests + pos_tests
      tests_performed <- tests_performed + number_reached
      
      # Split into new diagnoses vs re-engagement based on pool composition
      new_dx <- pos_tests * prop_new_dx
      re_eng  <- pos_tests * prop_reeng
      
      new_diagnoses       <- new_diagnoses + new_dx
      re_engagement_testing <- re_engagement_testing + re_eng
      
      # ART initiations based on linkage rate
      linkage_rate <- intervention$linkage_rate
      linked <- pos_tests * linkage_rate
      art_inititations_testing <- art_inititations_testing + linked
      
      additional_suppressed_testing <- additional_suppressed_testing +
        linked * ((context$percent_suppressed * 0.9) / 100)
      
      # Costs
      total_intervention_cost <- total_intervention_cost +
        (number_reached * intervention$unit_cost + linked * intervention$linkage_cost)
      
    } else if ("infant_infections" %in% intervention$outcomes) {
      infant_incidence_rate <- 0.15 ####UPDATE
      infant_infections_averted <- infant_infections_averted +
        number_reached * infant_incidence_rate * intervention$efficacy
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
      
    } else if ("viral_suppression" %in% intervention$outcomes) {
      additional_suppressed <- additional_suppressed +
        number_reached * (1 - context$percent_suppressed / 100) * intervention$efficacy
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
      
    } else if ("retention" %in% intervention$outcomes) {
      # ── Two distinct retention pathways ──────────────────────────────────
      # tracking_tracing (eligible_pop == "ltfu"): re-engages people already LTFU
      # MMD / adherence_counseling (eligible_pop != "ltfu"): prevents people
      #   from becoming LTFU in the first place
      if (intervention$eligible_pop == "ltfu") {
        ltfu_reengaged <- ltfu_reengaged + number_reached * intervention$efficacy
      } else {
        ltfu_prevented <- ltfu_prevented + number_reached * intervention$efficacy
      }
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
      
    } else if ("ahd_screening" %in% intervention$outcomes) {
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
      
    } else if ("pmtct" %in% intervention$outcomes) {
      mtct_rate <- 0.15
      infant_infections_averted <- infant_infections_averted +
        number_reached * mtct_rate * 0.30
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
    }
  }
  
  # ========================================================================
  # APPLY CONSTRAINTS - CAP AT REALISTIC MAXIMUMS
  # ========================================================================
  
  # Cannot diagnose more people than 95% are undiagnosed
  new_diagnoses <- min(new_diagnoses, populations$undiagnosed * 0.95)
  
  # ── LTFU FLOW ─────────────────────────────────────────────────────────────
  # Prevention interventions reduce incident LTFU, split proportionally across
  # the suppressed and unsuppressed dropout sub-groups.
  # If ltfu_new == 0 (no dropout), prevention has nothing to act on.
  prop_ltfu_supp <- ifelse(
    populations$ltfu_new > 0,
    populations$ltfu_new_suppressed / populations$ltfu_new,
    0
  )
  prop_ltfu_unsupp <- 1 - prop_ltfu_supp
  
  ltfu_prevented_supp   <- ltfu_prevented * prop_ltfu_supp
  ltfu_prevented_unsupp <- ltfu_prevented * prop_ltfu_unsupp
  
  # Net incident LTFU after prevention interventions, by suppression status
  suppressed_ltfu   <- max(0, populations$ltfu_new_suppressed   - ltfu_prevented_supp)
  unsuppressed_ltfu <- max(0, populations$ltfu_new_unsuppressed - ltfu_prevented_unsupp)
  ltfu_new_effective <- suppressed_ltfu + unsuppressed_ltfu
  
  # Full pool available for re-engagement = prevalent stock + net incident LTFU
  total_ltfu_pool <- populations$ltfu + ltfu_new_effective
  
  # Testing re-engagement draws from the full pool first;
  # tracking/tracing takes from whatever remains (up to 95% total)
  re_engagement_testing <- min(re_engagement_testing, total_ltfu_pool * 0.95)
  re_engagement         <- re_engagement_testing
  positive_tests        <- new_diagnoses + re_engagement_testing
  
  ltfu_reengaged <- min(ltfu_reengaged,
                        max(0, total_ltfu_pool * 0.95 - re_engagement_testing))
  
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
  
  # Cannot suppress more than are currently unsuppressed on ART
  # (after LTFU losses, net unsuppressed on ART is also reduced)
  max_additional_suppressed <- effective_on_art + art_initiations - populations$suppressed +
    suppressed_ltfu   # suppressed who became LTFU are no longer counted as suppressed
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
  # Cotrimoxazole and OI management apply to all new initiations (no CD4 test required).
  # AHD package applies only to those diagnosed with AHD via CD4 test.
  # ========================================================================
  
  on_art_total_est <- effective_on_art + art_initiations + ltfu_reengaged
  
  # Initialise scalars
  cd4_coverage_frac     <- 0   # proportion of new initiates who receive a CD4 test
  cotrix_eff_reduction  <- 0   # reduces base mortality rate for new initiations
  oi_eff_reduction      <- 0   # reduces base mortality rate for new initiations
  ahd_pkg_eff_reduction <- 0   # reduces AHD mortality rate, gated by CD4 diagnosis
  
  # Collect intervention values first (need cd4 before ahd_package)
  cd4_value     <- ifelse(is.null(interventions$cd4_testing),    0, interventions$cd4_testing)
  cotrix_value  <- ifelse(is.null(interventions$cotrimoxazole),  0, interventions$cotrimoxazole)
  oi_value      <- ifelse(is.null(interventions$oi_management),  0, interventions$oi_management)
  ahd_pkg_value <- ifelse(is.null(interventions$ahd_package),    0, interventions$ahd_package)
  
  # ── CD4 testing ──────────────────────────────────────────────────────────
  if (cd4_value > 0 && art_initiations > 0) {
    n_cd4_tested      <- min(art_initiations * (cd4_value / 100), art_initiations)
    cd4_coverage_frac <- n_cd4_tested / art_initiations
    cd4_cost          <- n_cd4_tested * all_interventions$cd4_testing$unit_cost
    total_intervention_cost <- total_intervention_cost + cd4_cost
  }
  
  # ── Cotrimoxazole (all new initiations, no CD4 required) ─────────────────
  if (cotrix_value > 0 && art_initiations > 0) {
    n_cotrix             <- min(art_initiations * (cotrix_value / 100), art_initiations)
    cotrix_cov_frac      <- n_cotrix / art_initiations
    cotrix_eff_reduction <- cotrix_cov_frac * all_interventions$cotrimoxazole$efficacy
    total_intervention_cost <- total_intervention_cost + n_cotrix * all_interventions$cotrimoxazole$unit_cost
  }
  
  # ── OI management (all new initiations, no CD4 required) ─────────────────
  if (oi_value > 0 && art_initiations > 0) {
    n_oi             <- min(art_initiations * (oi_value / 100), art_initiations)
    oi_cov_frac      <- n_oi / art_initiations
    oi_eff_reduction <- oi_cov_frac * all_interventions$oi_management$efficacy
    total_intervention_cost <- total_intervention_cost + n_oi * all_interventions$oi_management$unit_cost
  }
  
  # ── AHD package (only those diagnosed with AHD via CD4 test) ─────────────
  if (ahd_pkg_value > 0 && art_initiations > 0) {
    prop_ahd_new_init        <- MORTALITY_RATES$prop_ahd$new_initiations
    n_ahd_diagnosed          <- art_initiations * cd4_coverage_frac * prop_ahd_new_init
    n_ahd_pkg_reached        <- min(n_ahd_diagnosed * (ahd_pkg_value / 100), n_ahd_diagnosed)
    ahd_pkg_cov_frac_of_ahd  <- if (n_ahd_diagnosed > 0) n_ahd_pkg_reached / n_ahd_diagnosed else 0
    ahd_pkg_eff_reduction    <- cd4_coverage_frac * ahd_pkg_cov_frac_of_ahd *
      all_interventions$ahd_package$efficacy
    total_intervention_cost  <- total_intervention_cost + n_ahd_pkg_reached * all_interventions$ahd_package$unit_cost
  }
  
  # ========================================================================
  # CASCADE POPULATIONS (PRE-MORTALITY)
  # on_art and suppressed are reduced by net LTFU before gains are added.
  # ========================================================================
  
  end_diagnosed_pre_mort  <- min(max(populations$diagnosed + new_diagnoses, 0),
                                 populations$plhiv)
  end_on_art_pre_mort     <- min(max(effective_on_art + art_initiations + ltfu_reengaged, 0),
                                 end_diagnosed_pre_mort)
  end_suppressed_pre_mort <- min(max(populations$suppressed - suppressed_ltfu + additional_suppressed, 0),
                                 end_on_art_pre_mort)
  
  # ========================================================================
  # FIVE CASCADE GROUPS (mutually exclusive, before mortality)
  # ========================================================================
  
  n_undiagnosed        <- max(0, populations$plhiv - end_diagnosed_pre_mort)
  n_diagnosed_not_art  <- max(0, end_diagnosed_pre_mort - end_on_art_pre_mort)
  n_new_initiations    <- min(art_initiations, end_on_art_pre_mort)
  n_established_on_art <- max(0, end_on_art_pre_mort - n_new_initiations)
  # Suppression attributed to established patients (new initiates counted at new_art rate)
  n_established_supp   <- min(end_suppressed_pre_mort, n_established_on_art)
  n_established_treated<- max(0, n_established_on_art - n_established_supp)
  
  # ========================================================================
  # EFFECTIVE AHD MORTALITY RATES (intervention-adjusted where applicable)
  # ========================================================================
  
  prop_ahd <- MORTALITY_RATES$prop_ahd
  
  # Untreated groups: no interventions reach them
  eff_base_rate_untreated <- MORTALITY_RATES$untreated_undiagnosed
  eff_ahd_rate_untreated  <- MORTALITY_RATES$ahd
  
  # New initiations: cotrimoxazole + OI management reduce the base rate
  #                  AHD package reduces the AHD rate
  eff_base_rate_new_init <- MORTALITY_RATES$new_art_initiations *
    (1 - cotrix_eff_reduction) * (1 - oi_eff_reduction)
  eff_ahd_rate_new_init  <- MORTALITY_RATES$ahd *
    (1 - ahd_pkg_eff_reduction)
  
  # Established on ART: AHD package reduces the AHD rate only; base rates unchanged
  eff_ahd_rate_established <- MORTALITY_RATES$ahd *
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
  
  # Deaths averted = difference between unadjusted (no interventions) and adjusted deaths
  # Only on-treatment groups are affected by interventions
  unadjusted_deaths_on_treatment <-
    calc_deaths(n_new_initiations,     MORTALITY_RATES$new_art_initiations, MORTALITY_RATES$ahd, prop_ahd$new_initiations) +
    calc_deaths(n_established_treated, MORTALITY_RATES$treated,             MORTALITY_RATES$ahd, prop_ahd$established_treated) +
    calc_deaths(n_established_supp,    MORTALITY_RATES$suppressed,          MORTALITY_RATES$ahd, prop_ahd$established_supp)
  
  adjusted_deaths_on_treatment <- deaths_new_initiations + deaths_established_treated + deaths_established_supp
  
  total_deaths_averted <- max(0, unadjusted_deaths_on_treatment - adjusted_deaths_on_treatment)
  end_deaths           <- max(0, total_hiv_deaths)
  
  # ========================================================================
  # CASCADE (POST-MORTALITY)
  # ========================================================================
  
  remaining_undiagnosed      <- max(0, n_undiagnosed        - deaths_undiagnosed)
  remaining_diagnosed_not_art<- max(0, n_diagnosed_not_art  - deaths_diagnosed_not_art)
  remaining_new_init         <- max(0, n_new_initiations     - deaths_new_initiations)
  remaining_est_treated      <- max(0, n_established_treated - deaths_established_treated)
  remaining_est_supp         <- max(0, n_established_supp    - deaths_established_supp)
  
  end_suppressed <- remaining_est_supp
  end_on_art     <- remaining_est_treated + remaining_est_supp + remaining_new_init
  end_diagnosed  <- remaining_diagnosed_not_art + end_on_art
  end_plhiv      <- max(0, remaining_undiagnosed + end_diagnosed)
  
  # Ensure cascade consistency
  end_suppressed <- min(end_suppressed, end_on_art)
  end_on_art     <- min(end_on_art, end_diagnosed)
  
  # ========================================================================
  # CALCULATE END-OF-YEAR INFECTIONS and PREVENTION interventions
  # ========================================================================
  
  infectious_prop <- (populations$plhiv - end_suppressed) / populations$total
  force_inf       <- context$new_infections_per_year / (populations$plhiv - populations$suppressed)
  unprotected_pop <- populations$hiv_negative
  infections      <- unprotected_pop * force_inf * infectious_prop
  
  # Process prevention interventions
  for (int_key in names(all_interventions)) {
    intervention <- all_interventions[[int_key]]
    intervention_value <- interventions[[int_key]]
    
    if (is.null(intervention_value)) intervention_value <- 0
    if (intervention_value == 0) next
    
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
    
    if ("adult_infections" %in% intervention$outcomes) {
      infections_averted <- infections_averted +
        number_reached * force_inf * infectious_prop * (1 - intervention$efficacy)
      
      total_intervention_cost <- total_intervention_cost +
        number_reached * intervention$unit_cost
    }
  }
  
  end_new_infections     <- max(0, infections - infections_averted)
  baseline_infant_infections <- populations$hiv_exposed_infants * 0.15 ###UPDATE
  end_infant_infections  <- max(0, baseline_infant_infections - infant_infections_averted)
  
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
    additional_suppressed = round(additional_suppressed),
    # LTFU flow outputs (replacing old single retention_improvement)
    ltfu_new_effective  = round(ltfu_new_effective),   # net people lost to follow-up
    ltfu_prevented      = round(ltfu_prevented),        # prevented from dropping off by MMD/adherence
    ltfu_reengaged      = round(ltfu_reengaged),        # re-engaged by tracking/tracing
    suppressed_ltfu     = round(suppressed_ltfu),       # suppressed patients lost (cascade impact)
    unsuppressed_ltfu   = round(unsuppressed_ltfu),     # unsuppressed patients lost
    
    # Health outcomes (infections/deaths averted)
    adult_infections_averted = round(infections_averted),
    infant_infections_averted = round(infant_infections_averted),
    total_infections_averted = round(infections_averted + infant_infections_averted),
    deaths_averted = round(total_deaths_averted),
    
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
    additional_infections_averted = scenario$total_infections_averted - baseline$total_infections_averted,
    additional_deaths_averted     = scenario$deaths_averted           - baseline$deaths_averted,
    
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
regional_presets    <- build_country_presets(country_data_csv)