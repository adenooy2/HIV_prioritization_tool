# ============================================================================
# HIV Intervention Impact Calculator - Logic
# ============================================================================
# This tool allows users to model the health and cost impacts of scaling
# HIV interventions up or down across prevention, testing, and treatment.
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


dir="/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/"

#data/tier_app/basic_hiv_data.csv

# ============================================================================
# LOAD  DATA
# ============================================================================
# Load country data
#url to onedrive
response <- GET("https://1drv.ms/x/c/2ae90f5cbd0fd171/IQBCFFlfF2AaRLcGuaCvNAcJAbE-8Ak2_gDyNJnL0GQu8Ag?e=k5dAU1&download=1")
country_data_csv <- content(response, as = "parsed", type = "text/csv")

#load intervention data
# Load intervention parameters from Excel (LONG FORMAT)

load_intervention_params=function(){

sharepoint_url_interventions <- "https://bushare-my.sharepoint.com/:x:/g/personal/brooken_bu_edu/IQDkEN28uBz4Q6HD1Ydfa-mKASlPto-TuBhjDXChgC-eFbs?e=WuMKZs&download=1"

temp_file_int <- tempfile(fileext = ".xlsx")
download.file(sharepoint_url_interventions, temp_file_int, mode = "wb", method = "libcurl")

# Read Excel file
intervention_params <- read_excel(temp_file_int, col_names = FALSE)

# Make first row the column names and remove it
colnames(intervention_params) <- as.character(intervention_params[2, ])
intervention_params <- intervention_params[-1, ]
intervention_params <- intervention_params[-1, ]

intervention_params=intervention_params %>% select(category,intervention,intervention_key,parameter_type,current_value) %>% 
  spread(parameter_type,current_value)
# Convert current_Value to numeric
intervention_params$efficacy <- as.numeric(intervention_params$efficacy)
intervention_params$unit_cost <- as.numeric(intervention_params$unit_cost)
intervention_params$linkage_cost <- as.numeric(intervention_params$linkage_cost)
intervention_params$linkage_rate <- as.numeric(intervention_params$linkage_rate)
intervention_params$multiplier <- as.numeric(intervention_params$multiplier)

return(intervention_params)
}

# intervention_params=load_intervention_params()
# ============================================================================
# INTERVENTION PARAMETERS DATABASE
# ============================================================================
# NOTE: All efficacy values, costs, and population assumptions need to be
# validated against the literature. See accompanying CSV template.
# UPDATE LOCATION: Lines 28-280
# ============================================================================

build_intervention_groups=function(intervention_params){
intervention_groups <- list(
  prevention = list(
    name = "Prevention",
    color = "#10b981",
    interventions = list(
      prep_oral = list(
        name = "PrEP (oral)",
        type = "absolute",
        unit_label = "people",
        efficacy = subset(intervention_params, intervention_key == "prep_oral")$efficacy,  # UPDATE: Efficacy from RCTs/meta-analysis
        eligible_pop = "high_risk_negative",
        unit_cost = subset(intervention_params, intervention_key == "prep_oral")$unit_cost,  # UPDATE: Local/regional ART cost per person-year
        outcomes = c("adult_infections")
      ),
      prep_lenacapavir = list(
        name = "PrEP (Lenacapavir)",
        type = "absolute",
        unit_label = "people",
        efficacy = subset(intervention_params, intervention_key == "prep_lenacapavir")$efficacy,  # UPDATE: Based on PURPOSE trials
        eligible_pop = "high_risk_negative",
        unit_cost = subset(intervention_params, intervention_key == "prep_lenacapavir")$unit_cost,  # UPDATE: Estimated cost (may vary)
        outcomes = c("adult_infections")
      ),
      vmmc = list(
        name = "VMMC",
        type = "absolute",
        unit_label = "annual people",
        efficacy = subset(intervention_params, intervention_key == "vmmc")$efficacy,  # UPDATE: RCT evidence
        eligible_pop = "uncircumcised_males",
        unit_cost = subset(intervention_params, intervention_key == "vmmc")$unit_cost,   # UPDATE: Regional VMMC program costs
        outcomes = c("adult_infections")
      ),
      condoms = list(
        name = "Condom availability",
        type = "absolute",
        unit_label = "people reached",
        efficacy = subset(intervention_params, intervention_key == "condoms")$efficacy,  # UPDATE: Consistent use efficacy
        eligible_pop = "sexually_active_negative",
        unit_cost = subset(intervention_params, intervention_key == "condoms")$unit_cost, # UPDATE: Cost per condom distributed
        outcomes = c("adult_infections")
      ),
      pep = list(
        name = "PEP",
        type = "absolute",
        unit_label = "people",
        efficacy = subset(intervention_params, intervention_key == "pep")$efficacy,  # UPDATE: PEP efficacy estimates
        eligible_pop = "recent_exposure",
        unit_cost = subset(intervention_params, intervention_key == "pep")$unit_cost,  # UPDATE: 28-day PEP course cost
        outcomes = c("adult_infections")
      ),
      infant_prophylaxis = list(
        name = "Infant prophylaxis",
        type = "coverage",
        unit_label = "% of HIV-exposed infants",
        efficacy = subset(intervention_params, intervention_key == "infant_prophylaxis")$efficacy,  # UPDATE: ARV prophylaxis for HIV-exposed infants
        eligible_pop = "hiv_exposed_infants",
        unit_cost = subset(intervention_params, intervention_key == "infant_prophylaxis")$unit_cost,   # UPDATE: Cost per infant treated
        outcomes = c("infant_infections")
      ),
      cotrimoxazole = list(
        name = "Cotrimoxazole prophylaxis (according to guidelines)",
        type = "coverage",
        unit_label = "% of PLHIV",
        efficacy = subset(intervention_params, intervention_key == "cotrimoxazole")$efficacy,  # UPDATE: Mortality reduction from cotrimoxazole
        eligible_pop = "plhiv",
        unit_cost = subset(intervention_params, intervention_key == "cotrimoxazole")$unit_cost,   # UPDATE: Annual cost per person
        outcomes = c("mortality")
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
        eligible_pop = "adult_pop",
        unit_cost = subset(intervention_params, intervention_key == "test_facility_targeted")$unit_cost,
        linkage_rate = subset(intervention_params, intervention_key == "test_facility_targeted")$linkage_rate,
        linkage_cost = subset(intervention_params, intervention_key == "test_facility_targeted")$linkage_cost,
        test_yield_multiplier = subset(intervention_params, intervention_key == "test_facility_targeted")$yield_multiplier,  # UPDATE: Higher yield
        outcomes = c("testing")
      ),
      test_facility_general = list(
        name = "Testing: facility-based (general)",
        type = "absolute",
        unit_label = "tests performed",
        efficacy = subset(intervention_params, intervention_key == "test_facility_general")$efficacy,
        eligible_pop = "adult_pop",
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
        eligible_pop = "adult_pop",
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
        efficacy = subset(intervention_params, intervention_key == "test_community")$efficacy,  # UPDATE: Test sensitivity
        eligible_pop = "adult_pop",
        unit_cost = subset(intervention_params, intervention_key == "test_community")$unit_cost,    # UPDATE: Cost per test
        linkage_rate = subset(intervention_params, intervention_key == "test_community")$linkage_rate,  # UPDATE: Linkage to care
        linkage_cost = subset(intervention_params, intervention_key == "test_community")$linkage_cost,
        test_yield_multiplier = subset(intervention_params, intervention_key == "test_community")$yield_multiplier,
        outcomes = c("testing")
      ),
      test_kpsti = list(
        name = "Testing: key populations & STI services",
        type = "absolute",
        unit_label = "tests performed",
        efficacy = subset(intervention_params, intervention_key == "test_kpsti")$efficacy,  # UPDATE: Test sensitivity
        eligible_pop = "adult_pop",
        unit_cost = subset(intervention_params, intervention_key == "test_kpsti")$unit_cost,   # UPDATE: Cost per test
        linkage_rate = subset(intervention_params, intervention_key == "test_kpsti")$linkage_rate,
        linkage_cost = subset(intervention_params, intervention_key == "test_kpsti")$linkage_cost,
        test_yield_multiplier = subset(intervention_params, intervention_key == "test_kpsti")$yield_multiplier,  # UPDATE: Higher yield in key pops
        outcomes = c("testing")
      ),
      hivst_facility = list(
        name = "HIVST (Facility-based)",
        type = "absolute",
        unit_label = "tests distributed",
        efficacy = subset(intervention_params, intervention_key == "hivst_facility")$efficacy,  # UPDATE: Self-test sensitivity
        eligible_pop = "adult_pop",
        unit_cost = subset(intervention_params, intervention_key == "hivst_facility")$unit_cost,    # UPDATE: Cost per self-test kit
        linkage_rate = subset(intervention_params, intervention_key == "hivst_facility")$linkage_rate,  # UPDATE: Lower linkage for HIVST
        linkage_cost = subset(intervention_params, intervention_key == "hivst_facility")$linkage_cost,
        test_yield_multiplier = subset(intervention_params, intervention_key == "hivst_facility")$yield_multiplier,
        outcomes = c("testing")
      ),
      hivst_community = list(
        name = "HIVST (Community-based)",
        type = "absolute",
        unit_label = "tests distributed",
        efficacy = subset(intervention_params, intervention_key == "hivst_community")$efficacy,  # UPDATE: Self-test sensitivity
        eligible_pop = "adult_pop",
        unit_cost = subset(intervention_params, intervention_key == "hivst_community")$unit_cost,    # UPDATE: Cost per self-test kit
        linkage_rate = subset(intervention_params, intervention_key == "hivst_community")$linkage_rate,
        linkage_cost = subset(intervention_params, intervention_key == "hivst_community")$linkage_cost,
        test_yield_multiplier = subset(intervention_params, intervention_key == "hivst_community")$yield_multiplier,
        outcomes = c("testing")
      ),
      eid = list(
        name = "EID (Early Infant Diagnosis)",
        type = "coverage",
        unit_label = "% of HIV-exposed infants",
        efficacy = subset(intervention_params, intervention_key == "eid")$efficacy,  # UPDATE: Test sensitivity for infants
        eligible_pop = "hiv_exposed_infants",
        unit_cost = subset(intervention_params, intervention_key == "eid")$unit_cost,   # UPDATE: Cost per infant tested
        linkage_rate = subset(intervention_params, intervention_key == "eid")$linkage_rate,
        linkage_cost = subset(intervention_params, intervention_key == "eid")$linkage_cost,
        test_yield_multiplier = subset(intervention_params, intervention_key == "eid")$yield_multiplier,
        outcomes = c("testing")
      ),
      anc_hiv_testing = list(
        name = "ANC: HIV testing",
        type = "coverage",
        unit_label = "% of pregnant women",
        efficacy = subset(intervention_params, intervention_key == "anc_hiv_testing")$efficacy,  # UPDATE: Test sensitivity
        eligible_pop = "pregnant_women",
        unit_cost = subset(intervention_params, intervention_key == "anc_hiv_testing")$unit_cost,    # UPDATE: Cost per test
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
        eligible_pop = "pregnant_women",  # Or create a new "postpartum_women" population
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
        efficacy = subset(intervention_params, intervention_key == "vl_monitoring_routine")$efficacy,  # UPDATE: VL suppression improvement
        eligible_pop = "on_art",
        unit_cost = subset(intervention_params, intervention_key == "vl_monitoring_routine")$unit_cost,   # UPDATE: Cost per VL test
        outcomes = c("viral_suppression")
      ),
      vl_monitoring_targeted = list(
        name = "VL monitoring: suspected failure",
        type = "absolute",
        unit_label = "people tested",
        efficacy = subset(intervention_params, intervention_key == "vl_monitoring_targeted")$efficacy,  # UPDATE: Targeted VL efficacy
        eligible_pop = "on_art_suspected_failure",
        unit_cost = subset(intervention_params, intervention_key == "vl_monitoring_targeted")$unit_cost,   # UPDATE: Cost per test + followup
        outcomes = c("viral_suppression")
      ),
      oi_management = list(
        name = "OI screening & management",
        type = "coverage",
        unit_label = "% of new ART initiations",
        efficacy = subset(intervention_params, intervention_key == "oi_management")$efficacy,  # UPDATE: Mortality reduction from OI management
        eligible_pop = "new_art_initiations",
        unit_cost = subset(intervention_params, intervention_key == "oi_management")$unit_cost,   # UPDATE: Cost per person screened/treated
        outcomes = c("mortality")
      ),
      mmd_3month = list(
        name = "MMD: 3-month dispensing",
        type = "coverage",
        unit_label = "% of stable clients",
        efficacy = subset(intervention_params, intervention_key == "mmd_3month")$efficacy,  # UPDATE: Retention improvement vs monthly
        eligible_pop = "on_art_stable",
        unit_cost = subset(intervention_params, intervention_key == "mmd_3month")$unit_cost,    # UPDATE: Additional cost vs monthly
        outcomes = c("retention")
      ),
      mmd_6month = list(
        name = "MMD: 6-month dispensing",
        type = "coverage",
        unit_label = "% of stable clients",
        efficacy = subset(intervention_params, intervention_key == "mmd_6month")$efficacy,  # UPDATE: Retention improvement vs monthly
        eligible_pop = "on_art_stable",
        unit_cost = subset(intervention_params, intervention_key == "mmd_6month")$unit_cost,    # UPDATE: Additional cost vs monthly
        outcomes = c("retention")
      ),
      mmd_12month = list(
        name = "MMD: 12-month dispensing",
        type = "coverage",
        unit_label = "% of stable clients",
        efficacy = subset(intervention_params, intervention_key == "mmd_12month")$efficacy,  # UPDATE: Retention improvement vs monthly
        eligible_pop = "on_art_stable",
        unit_cost =subset(intervention_params, intervention_key == "mmd_12month")$unit_cost ,   # UPDATE: Additional cost vs monthly
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
        efficacy = subset(intervention_params, intervention_key == "adherence_counseling")$efficacy,  # UPDATE: Adherence improvement efficacy
        eligible_pop = "on_art",
        unit_cost = subset(intervention_params, intervention_key == "adherence_counseling")$unit_cost,   # UPDATE: Cost per person counseled
        outcomes = c("viral_suppression", "retention")
      ),
      tracking_tracing = list(
        name = "Tracking & tracing",
        type = "coverage",
        unit_label = "% of LTFU patients",
        efficacy = subset(intervention_params, intervention_key == "tracking_tracing")$efficacy,  # UPDATE: Return-to-care rate
        eligible_pop = "ltfu",
        unit_cost = subset(intervention_params, intervention_key == "tracking_tracing")$unit_cost,   # UPDATE: Cost per person traced
        outcomes = c("retention")
      ),
      anc_vl_testing = list(
        name = "ANC: Viral Load Testing",
        type = "coverage",
        unit_label = "% of pregnant women on ART",
        efficacy = subset(intervention_params, intervention_key == "anc_vl_testing")$efficacy,  # UPDATE: VL suppression in pregnancy
        eligible_pop = "pregnant_on_art",
        unit_cost = subset(intervention_params, intervention_key == "anc_vl_testing")$unit_cost,   # UPDATE: Cost per VL test
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
        efficacy = subset(intervention_params, intervention_key == "cd4_testing")$efficacy,  # Just a screening test
        eligible_pop = "new_art_initiations",
        unit_cost = subset(intervention_params, intervention_key == "cd4_testing")$unit_cost,  # UPDATE: Cost per CD4 test
        outcomes = c("ahd_screening")
      ),
      ahd_package = list(
        name = "Full AHD package (LAM, CrAg, fluconazole)",
        type = "coverage",
        unit_label = "% of those with CD4<200",
        efficacy = subset(intervention_params, intervention_key == "ahd_package")$efficacy,  # UPDATE: Mortality reduction from AHD package
        eligible_pop = "advanced_disease",
        proportion_advanced = subset(intervention_params, intervention_key == "ahd_package")$proportion_advanced,  # UPDATE: Proportion with CD4<200 - country specific?]
        unit_cost = subset(intervention_params, intervention_key == "ahd_package")$unit_cost,   # UPDATE: Cost of LAM + CrAg + fluconazole (not including CD4)
        outcomes = c("mortality")
      )
    )
  )
)

return(intervention_groups)
}

# intervention_groups=build_intervention_groups(intervention_params)

# ============================================================================
# POPULATION CALCULATION FUNCTION
# ============================================================================

calculate_populations <- function(context) {
  
  plhiv <- context$total_population * context$hiv_prevalence
  diagnosed <- plhiv * (context$percent_diagnosed/100)  # UPDATE: Diagnosis rate assumption
  on_art <- diagnosed * (context$percent_on_art / 100)
  suppressed <- on_art * (context$percent_suppressed / 100)
  hiv_negative <- context$total_population - plhiv
  sexually_active <- context$total_population * 0.60  # UPDATE: Sexual activity rate
  births <- (context$total_population * context$birth_rate)/1000  # UPDATE: Birth rate
  hiv_positive_births <- births * context$hiv_prevalence * 1.5  # UPDATE: Prevalence multiplier
  
  
  list(
    total = context$total_population,
    adult_pop=context$total_population*(1-context$prop_pop_under_14/100),
    plhiv = plhiv,
    hiv_negative = hiv_negative,
    sexually_active = sexually_active,
    undiagnosed = plhiv - diagnosed,
    diagnosed = diagnosed,
    diagnosed_not_on_art = diagnosed - on_art,
    on_art = on_art,
    on_art_stable = on_art * 0.85,  # UPDATE: Stability assumption
    on_art_suspected_failure = on_art * 0.08,  # UPDATE: Failure rate
    suppressed = suppressed,
    unsuppressed = on_art - suppressed,
    ltfu = on_art * 0.15,  # UPDATE: LTFU rate
    high_risk_negative = hiv_negative * 0.05,  # UPDATE: High-risk proportion
    uncircumcised_males = (hiv_negative * context$prop_pop_male) * 0.25,  # UPDATE: Male proportion * uncircumcised rate
    sexually_active_negative = (hiv_negative * 0.60),  # UPDATE: Sexual activity
    recent_exposure = hiv_negative * 0.002,  # UPDATE: PEP need
    hiv_exposed_infants = hiv_positive_births,
    pregnant_women = births,
    pregnant_on_art = births * context$hiv_prevalence * 0.85,#update
    newly_diagnosed_advanced = (plhiv - diagnosed) * 0.20  # UPDATE: Advanced disease %
  )
}

# ============================================================================
# DEFAULT BASELINE INTERVENTIONS (used when not in CSV)
# ============================================================================

default_baseline_interventions <- list(
  prep_oral = 5000, prep_lenacapavir = 0, vmmc = 30000,
  condoms = 200000, pep = 2000, infant_prophylaxis = 70,
  cotrimoxazole = 60,
  test_facility_targeted = 25000, test_facility_general = 25000,
  test_network_index = 5000, test_community = 20000,
  test_kpsti = 8000, hivst_facility = 10000, hivst_community = 5000,
  eid = 75, anc_hiv_testing = 88, pnc_hiv_testing = 70,
  vl_monitoring_routine = 60, vl_monitoring_targeted = 2000,
  oi_management = 50, mmd_3month = 40, mmd_6month = 20, mmd_12month = 5,
  adherence_counseling = 55, tracking_tracing = 40, anc_vl_testing = 68,
  cd4_testing = 92, ahd_package = 88
)

# ============================================================================
# BUILD COUNTRY PRESETS FROM CSV OR USE DEFAULTS
# ============================================================================

build_country_presets <- function(csv_data) {
  presets <- list()
  
  if (!is.null(csv_data) && nrow(csv_data) > 0) {
    # Build from CSV
    for (i in 1:nrow(csv_data)) {
      row <- csv_data[i, ]
      country_name <- row$country
      
      # Extract context
      context <- list(
        total_population = row$total_population,
        hiv_prevalence = row$hiv_prevalence / 100,  # Convert to proportion if needed
        new_infections_per_year = row$new_infections_per_year,
        current_diagnoses = row$current_diagnoses,
        percent_diagnosed=row$percent_diagnosed,
        percent_on_art = row$percent_on_art,
        percent_suppressed = row$percent_suppressed,
        aids_deaths_per_year = row$aids_deaths_per_year,
        birth_rate=row$birth_rate,
        prop_pop_male=row$prop_male,
        prop_pop_under_14=row$prop_under14
      )
      
      pops=calculate_populations(context)
      #print(pops)
    
      
      
      #Proprtional baselines - UPDATE - check tetsing for adult pop
      default_baseline_interventions <- list(
        prep_oral = 0.01*pops$total, prep_lenacapavir = 0, vmmc = 0.01*pops$uncircumcised_males,
        condoms = 0.6*pops$total, pep = 0.2*pops$recent_exposure, infant_prophylaxis = 70,
        cotrimoxazole = 60, 
        test_facility_targeted = 0.05*pops$adult_pop, test_facility_general = 0.05*pops$adult_pop, 
        test_network_index = 0.01*pops$adult_pop, test_community = 0.04*pops$adult_pop,
        test_kpsti = 0.02*pops$adult_pop, hivst_facility = 0.01*pops$adult_pop, hivst_community = 0.02*pops$total*pops$adult_pop,
        eid = 75, anc_hiv_testing = 88, pnc_hiv_testing = 70,
        vl_monitoring_routine = 60, vl_monitoring_targeted = 0.05*pops$on_art_suspected_failure,
        oi_management = 50, mmd_3month = 40, mmd_6month = 20, mmd_12month = 5,
        adherence_counseling = 55, tracking_tracing = 40, anc_vl_testing = 68,
        cd4_testing = 92, ahd_package = 88
      )
      
      
      
      
      # Extract baseline interventions if present in CSV
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
  
  # Add Custom Country option with proportional baselines
  custom_context <- list(
    total_population = 1000000,
    hiv_prevalence = 0.05,
    percent_diagnosed = 80,
    percent_on_art = 75,
    percent_suppressed = 85,
    new_infections_per_year = 5000,
    aids_deaths_per_year = 1000,
    birth_rate=24,
    prop_pop_male=49,
    prop_pop_under_14=40
  )
  
  # Calculate populations for Custom Country to get proportional baselines
  custom_pops <- calculate_populations(custom_context)
  
  # Create proportional baselines for Custom Country
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
    vl_monitoring_targeted = 0.05*custom_pops$on_art_suspected_failure,
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

# # Build presets
# regional_presets <- build_country_presets(country_data_csv)


# ============================================================================
# IMPACT CALCULATION FUNCTION
# ============================================================================

calculate_impact <- function(context, baseline, target, populations) {
  
  # QUICK CHECK: Are context values valid?
  if (is.null(context$percent_suppressed) || length(context$percent_suppressed) == 0) {
    stop("ERROR: context$percent_suppressed is NULL or length zero!")
  }
  if (is.null(context$percent_diagnosed) || length(context$percent_diagnosed) == 0) {
    stop("ERROR: context$percent_diagnosed is NULL or length zero!")
  }
  
  # Initialize outcome counters
  infections_averted <- 0
  infant_infections_averted <- 0
  deaths_averted <- 0
  new_pos_tests=0
  positive_tests= 0
  new_diagnoses <- 0
  re_engagement <- 0
  infant_diagnoses <- 0
  additional_suppressed <- 0
  art_initiations <- 0
  retention_improvement <- 0
  total_cost <- 0
  cost_savings <- 0
  tests_performed <- 0
  
  # Calculate dynamic testing yield
  # Yield = probability that a test is positive = (undiagnosed + ltfu) / sexually_active
  base_test_yield <- ((populations$undiagnosed + populations$ltfu) / populations$sexually_active)*0.9
  base_test_yield <- min(base_test_yield, 0.10)  # Cap at 10% for plausibility,  maximum of 90% of people could be found #Update
  
  
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
    base_value <- baseline[[int_key]]
    target_value <- target[[int_key]]
    
    if (is.null(base_value)) base_value <- 0
    if (is.null(target_value)) target_value <- 0
    
    delta <- target_value - base_value
    #print(paste("Delta: ", delta))
    if (delta == 0) next
    
    # Get eligible population
    eligible <- populations[[intervention$eligible_pop]]
    #print(paste("eligible: ", eligible))
    if (is.null(eligible)) eligible <- 0
    
    # Calculate number reached
    number_reached <- abs(delta)
    if (intervention$type == "coverage") {
      number_reached <- eligible * (abs(delta) / 100)
    }
    
    #To account for no additional impact past the eligible population - check?
    if(intervention$type=="absolute"){
      if(target_value>eligible){
        number_reached=eligible-base_value
      }
    }
    
    number_reached <- min(number_reached, eligible)
    #print(paste("# reached:", number_reached))
    
    # Sign for scale-up/down
    sign <- ifelse(delta > 0, 1, -1)
    
    # Calculate outcomes based on intervention type
    if ("testing" %in% intervention$outcomes) {
      #print(intervention)
      
      # Testing interventions: calculate yield and split new vs re-engagement
      test_yield <- base_test_yield
      if (!is.null(intervention$test_yield_multiplier)) {
        test_yield <- test_yield * as.numeric(intervention$test_yield_multiplier)
      }
      
      positive_tests <- number_reached * test_yield * intervention$efficacy
      tests_performed <- tests_performed + sign * number_reached
      
      # 50% are new diagnoses, 50% are re-engagement 
      new_dx <- positive_tests * 0.50 #UPDATE
      re_eng <- positive_tests * 0.50 #UPDATE
      
      print(paste("NEW dx",new_dx))
      
      new_pos_tests=new_pos_tests+positive_tests
      new_diagnoses <- new_diagnoses + sign * new_dx
      re_engagement <- re_engagement + sign * re_eng
      
      # ART initiations based on linkage rate
      linkage_rate <- intervention$linkage_rate
      #print(paste("Linkage_rate:", linkage_rate))
      linked <- positive_tests * linkage_rate
      art_initiations <- art_initiations + sign * linked
      
      #Additional virally suppressed based on current 95-targets minus 10%
      
      additional_suppressed <- additional_suppressed + sign * linked * ((context$percent_suppressed-10)/100)
      
      
      
      # Costs: test cost + linkage cost for those who link
      intervention_cost <- sign * (number_reached * intervention$unit_cost + 
                                     linked * intervention$linkage_cost)
      if (sign > 0) {
        total_cost <- total_cost + intervention_cost
      } else {
        cost_savings <- cost_savings + abs(intervention_cost)
      }
      
    } else if ("adult_infections" %in% intervention$outcomes) {
      # Prevention interventions
     
      incidence_rate <- context$new_infections_per_year / populations$hiv_negative
      infections_averted <- infections_averted + 
        sign * number_reached * incidence_rate * intervention$efficacy
      
      intervention_cost <- sign * number_reached * intervention$unit_cost
      if (sign > 0) {
        total_cost <- total_cost + intervention_cost
      } else {
        cost_savings <- cost_savings + abs(intervention_cost)
      }
      
    } else if ("infant_infections" %in% intervention$outcomes) {
      infant_incidence_rate <- 0.15  # UPDATE: MTCT rate
      infant_infections_averted <- infant_infections_averted + 
        sign * number_reached * infant_incidence_rate * intervention$efficacy
      
      intervention_cost <- sign * number_reached * intervention$unit_cost
      if (sign > 0) {
        total_cost <- total_cost + intervention_cost
      } else {
        cost_savings <- cost_savings + abs(intervention_cost)
      }
      
    } else if ("viral_suppression" %in% intervention$outcomes) {
      additional_suppressed <- additional_suppressed + 
        sign * number_reached * intervention$efficacy
      
      intervention_cost <- sign * number_reached * intervention$unit_cost
      if (sign > 0) {
        total_cost <- total_cost + intervention_cost
      } else {
        cost_savings <- cost_savings + abs(intervention_cost)
      }
      
    } else if ("retention" %in% intervention$outcomes) {
      retention_improvement <- retention_improvement + 
        sign * number_reached * intervention$efficacy
      
      intervention_cost <- sign * number_reached * intervention$unit_cost
      if (sign > 0) {
        total_cost <- total_cost + intervention_cost
      } else {
        cost_savings <- cost_savings + abs(intervention_cost)
      }
      
    } else if ("ahd_screening" %in% intervention$outcomes) {
      # CD4 testing - just add cost, screening happens for all
      intervention_cost <- sign * number_reached * intervention$unit_cost
      if (sign > 0) {
        total_cost <- total_cost + intervention_cost
      } else {
        cost_savings <- cost_savings + abs(intervention_cost)
      }
      
    } else if ("mortality" %in% intervention$outcomes) {
      mortality_rate <- context$aids_deaths_per_year / populations$plhiv
      
      deaths_averted <- deaths_averted + 
        sign * number_reached * mortality_rate * intervention$efficacy
      
      
      intervention_cost <- sign * number_reached * intervention$unit_cost
      if (sign > 0) {
        total_cost <- total_cost + intervention_cost
      } else {
        cost_savings <- cost_savings + abs(intervention_cost)
      }
      
    } else if ("pmtct" %in% intervention$outcomes) {
      mtct_rate <- 0.15
      infant_infections_averted <- infant_infections_averted + 
        sign * number_reached * mtct_rate * 0.30
      
      intervention_cost <- sign * number_reached * intervention$unit_cost
      if (sign > 0) {
        total_cost <- total_cost + intervention_cost
      } else {
        cost_savings <- cost_savings + abs(intervention_cost)
      }
    }
  }
  
  # ART provision cost (outcome-driven)
  total_needing_art <- populations$on_art + art_initiations
  art_provision_cost <- max(0, total_needing_art) * 200  # UPDATE: ART cost per person-year
  
  # Calculate new cascade after interventions
  new_diagnosed <- populations$diagnosed + new_diagnoses
  new_on_art <- populations$on_art + art_initiations + retention_improvement
  new_suppressed <- populations$suppressed + additional_suppressed
  new_deaths <- max(0, context$aids_deaths_per_year - deaths_averted)
  new_ltfu=populations$ltfu-retention_improvement
  
  # Calculate new infection values
  new_infections <- max(0, context$new_infections_per_year - infections_averted)
  baseline_infant_infections <- populations$hiv_exposed_infants * 0.15 ### UPDATE
  new_infant_infections <- max(0, baseline_infant_infections - infant_infections_averted)
  
  
  list(
    infections_averted = round(infections_averted),
    infant_infections_averted = round(infant_infections_averted),
    total_infections_averted = round(infections_averted + infant_infections_averted),
    deaths_averted = round(deaths_averted),
    new_positives=round(new_pos_tests),
    new_diagnoses = round(new_diagnoses),
    re_engagement = round(re_engagement),
    infant_diagnoses = round(infant_diagnoses),
    art_initiations = round(art_initiations),
    additional_suppressed = round(additional_suppressed),
    retention_improvement = round(retention_improvement),
    tests_performed = round(tests_performed),
    total_cost = round(total_cost),
    cost_savings = round(cost_savings),
    art_provision_cost = round(art_provision_cost),
    net_cost = round(total_cost - cost_savings + art_provision_cost),
    cost_per_infection_averted = ifelse(
      infections_averted + infant_infections_averted != 0,
      round(total_cost / (infections_averted + infant_infections_averted)),
      0
    ),
    cost_per_death_averted = ifelse(
      deaths_averted != 0,
      round(total_cost / deaths_averted),
      0
    ),
    # New cascade values
    new_diagnosed = round(new_diagnosed),
    new_on_art = round(new_on_art),
    new_suppressed = round(new_suppressed),
    new_deaths = round(new_deaths),
    new_ltfu=round(new_ltfu),
    new_infections = round(new_infections),
    new_infant_infections = round(new_infant_infections)
    
  )
}


# ============================================================================
# Run functions

intervention_params=load_intervention_params()
intervention_groups=build_intervention_groups(intervention_params)
regional_presets <- build_country_presets(country_data_csv)
# ============================================================================