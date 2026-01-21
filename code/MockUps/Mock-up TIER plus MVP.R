# ============================================================================
# HIV Intervention Impact Calculator - R Shiny Application
# ============================================================================
# This tool allows users to model the health and cost impacts of scaling
# HIV interventions up or down across prevention, testing, and treatment.
# ============================================================================

library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(httr)
library(readr)


dir="/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/"

#data/tier_app/basic_hiv_data.csv

# ============================================================================
# LOAD COUNTRY DATA FROM CSV
# ============================================================================
# Expected CSV columns:
# country, total_population, hiv_prevalence, new_infections_per_year,
# current_diagnoses, percent_on_art, percent_suppressed, aids_deaths_per_year
# Plus baseline intervention columns (optional)
# ============================================================================

load_country_data <- function(csv_file = paste(dir,"data/tier_app/basic_hiv_data.csv",sep="")) {
  if (!file.exists(csv_file)) {
    warning("Country data CSV not found. Using default data.")
    return(NULL)
  }
  
  tryCatch({
    data <- read.csv(csv_file, stringsAsFactors = FALSE)
    
    # Validate required columns
    required_cols <- c("country", "total_population", "hiv_prevalence", 
                       "new_infections_per_year", "current_diagnoses", "percent_diagnosed",
                       "percent_on_art", "percent_suppressed", "aids_deaths_per_year")
    
    
    
    missing_cols <- setdiff(required_cols, names(data))
    if (length(missing_cols) > 0) {
      warning(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
      return(NULL)
    }
    
    return(data)
  }, error = function(e) {
    warning(paste("Error loading country data:", e$message))
    return(NULL)
  })
}

# Load country data
#country_data_csv <- load_country_data()

response <- GET("https://1drv.ms/x/c/2ae90f5cbd0fd171/IQBCFFlfF2AaRLcGuaCvNAcJAbE-8Ak2_gDyNJnL0GQu8Ag?e=k5dAU1&download=1")

country_data_csv <- content(response, as = "parsed", type = "text/csv")

# ============================================================================
# INTERVENTION PARAMETERS DATABASE
# ============================================================================
# NOTE: All efficacy values, costs, and population assumptions need to be
# validated against the literature. See accompanying CSV template.
# UPDATE LOCATION: Lines 28-280
# ============================================================================

intervention_groups <- list(
  prevention = list(
    name = "Prevention",
    color = "#10b981",
    interventions = list(
      prep_oral = list(
        name = "PrEP (oral)",
        type = "absolute",
        unit_label = "people",
        efficacy = 0.86,  # UPDATE: Efficacy from RCTs/meta-analysis
        eligible_pop = "high_risk_negative",
        unit_cost = 180,  # UPDATE: Local/regional ART cost per person-year
        outcomes = c("adult_infections")
      ),
      prep_lenacapavir = list(
        name = "PrEP (Lenacapavir)",
        type = "absolute",
        unit_label = "people",
        efficacy = 0.96,  # UPDATE: Based on PURPOSE trials
        eligible_pop = "high_risk_negative",
        unit_cost = 450,  # UPDATE: Estimated cost (may vary)
        outcomes = c("adult_infections")
      ),
      vmmc = list(
        name = "VMMC",
        type = "absolute",
        unit_label = "people",
        efficacy = 0.60,  # UPDATE: RCT evidence
        eligible_pop = "uncircumcised_males",
        unit_cost = 75,   # UPDATE: Regional VMMC program costs
        outcomes = c("adult_infections")
      ),
      condoms = list(
        name = "Condom availability",
        type = "absolute",
        unit_label = "people reached",
        efficacy = 0.85,  # UPDATE: Consistent use efficacy
        eligible_pop = "sexually_active_negative",
        unit_cost = 0.25, # UPDATE: Cost per condom distributed
        outcomes = c("adult_infections")
      ),
      pep = list(
        name = "PEP",
        type = "absolute",
        unit_label = "people",
        efficacy = 0.81,  # UPDATE: PEP efficacy estimates
        eligible_pop = "recent_exposure",
        unit_cost = 120,  # UPDATE: 28-day PEP course cost
        outcomes = c("adult_infections")
      ),
      infant_prophylaxis = list(
        name = "Infant prophylaxis",
        type = "coverage",
        unit_label = "% of HIV-exposed infants",
        efficacy = 0.92,  # UPDATE: ARV prophylaxis for HIV-exposed infants
        eligible_pop = "hiv_exposed_infants",
        unit_cost = 45,   # UPDATE: Cost per infant treated
        outcomes = c("infant_infections")
      ),
      cotrimoxazole = list(
        name = "Cotrimoxazole prophylaxis",
        type = "coverage",
        unit_label = "% of PLHIV",
        efficacy = 0.70,  # UPDATE: Mortality reduction from cotrimoxazole
        eligible_pop = "plhiv",
        unit_cost = 12,   # UPDATE: Annual cost per person
        outcomes = c("mortality")
      )
    )
  ),
  
  testing = list(
    name = "Testing & Diagnosis",
    color = "#3b82f6",
    interventions = list(
      test_facility = list(
        name = "Testing: facility-based",
        type = "absolute",
        unit_label = "tests performed",
        efficacy = 0.99,  # UPDATE: Test sensitivity
        eligible_pop = "sexually_active",
        unit_cost = 15,   # UPDATE: Cost per test
        linkage_rate = 0.97,  # UPDATE: Linkage to care for facility-based
        linkage_cost = 25,    # UPDATE: Cost of linkage/initiation support
        outcomes = c("testing")
      ),
      test_community = list(
        name = "Testing: community-based",
        type = "absolute",
        unit_label = "tests performed",
        efficacy = 0.99,  # UPDATE: Test sensitivity
        eligible_pop = "sexually_active",
        unit_cost = 8,    # UPDATE: Cost per test
        linkage_rate = 0.97,  # UPDATE: Linkage to care
        linkage_cost = 25,
        outcomes = c("testing")
      ),
      test_kpsti = list(
        name = "Testing: key populations & STI services",
        type = "absolute",
        unit_label = "tests performed",
        efficacy = 0.99,  # UPDATE: Test sensitivity
        eligible_pop = "sexually_active",
        unit_cost = 12,   # UPDATE: Cost per test
        linkage_rate = 0.97,
        linkage_cost = 25,
        test_yield_multiplier = 1.5,  # UPDATE: Higher yield in key pops
        outcomes = c("testing")
      ),
      hivst_facility = list(
        name = "HIVST (Facility-based)",
        type = "absolute",
        unit_label = "tests distributed",
        efficacy = 0.97,  # UPDATE: Self-test sensitivity
        eligible_pop = "sexually_active",
        unit_cost = 5,    # UPDATE: Cost per self-test kit
        linkage_rate = 0.70,  # UPDATE: Lower linkage for HIVST
        linkage_cost = 25,
        outcomes = c("testing")
      ),
      hivst_community = list(
        name = "HIVST (Community-based)",
        type = "absolute",
        unit_label = "tests distributed",
        efficacy = 0.97,  # UPDATE: Self-test sensitivity
        eligible_pop = "sexually_active",
        unit_cost = 4,    # UPDATE: Cost per self-test kit
        linkage_rate = 0.70,
        linkage_cost = 25,
        outcomes = c("testing")
      ),
      eid = list(
        name = "EID (Early Infant Diagnosis)",
        type = "coverage",
        unit_label = "% of HIV-exposed infants",
        efficacy = 0.98,  # UPDATE: Test sensitivity for infants
        eligible_pop = "hiv_exposed_infants",
        unit_cost = 25,   # UPDATE: Cost per infant tested
        linkage_rate = 0.95,
        linkage_cost = 20,
        outcomes = c("testing")
      ),
      anc_hiv_testing = list(
        name = "ANC: HIV testing",
        type = "coverage",
        unit_label = "% of pregnant women",
        efficacy = 0.99,  # UPDATE: Test sensitivity
        eligible_pop = "pregnant_women",
        unit_cost = 8,    # UPDATE: Cost per test
        linkage_rate = 0.97,
        linkage_cost = 25,
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
        efficacy = 0.85,  # UPDATE: VL suppression improvement
        eligible_pop = "on_art",
        unit_cost = 25,   # UPDATE: Cost per VL test
        outcomes = c("viral_suppression")
      ),
      vl_monitoring_targeted = list(
        name = "VL monitoring: suspected failure",
        type = "absolute",
        unit_label = "people tested",
        efficacy = 0.90,  # UPDATE: Targeted VL efficacy
        eligible_pop = "on_art_suspected_failure",
        unit_cost = 35,   # UPDATE: Cost per test + followup
        outcomes = c("viral_suppression")
      ),
      oi_management = list(
        name = "OI screening & management",
        type = "coverage",
        unit_label = "% of new ART initiations",
        efficacy = 0.75,  # UPDATE: Mortality reduction from OI management
        eligible_pop = "new_art_initiations",
        unit_cost = 45,   # UPDATE: Cost per person screened/treated
        outcomes = c("mortality")
      ),
      mmd_3month = list(
        name = "MMD: 3-month dispensing",
        type = "coverage",
        unit_label = "% of stable clients",
        efficacy = 0.88,  # UPDATE: Retention improvement vs monthly
        eligible_pop = "on_art_stable",
        unit_cost = 5,    # UPDATE: Additional cost vs monthly
        outcomes = c("retention")
      ),
      mmd_6month = list(
        name = "MMD: 6-month dispensing",
        type = "coverage",
        unit_label = "% of stable clients",
        efficacy = 0.92,  # UPDATE: Retention improvement vs monthly
        eligible_pop = "on_art_stable",
        unit_cost = 8,    # UPDATE: Additional cost vs monthly
        outcomes = c("retention")
      ),
      mmd_12month = list(
        name = "MMD: 12-month dispensing",
        type = "coverage",
        unit_label = "% of stable clients",
        efficacy = 0.95,  # UPDATE: Retention improvement vs monthly
        eligible_pop = "on_art_stable",
        unit_cost = 12,   # UPDATE: Additional cost vs monthly
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
        efficacy = 0.80,  # UPDATE: Adherence improvement efficacy
        eligible_pop = "on_art",
        unit_cost = 30,   # UPDATE: Cost per person counseled
        outcomes = c("viral_suppression", "retention")
      ),
      tracking_tracing = list(
        name = "Tracking & tracing",
        type = "coverage",
        unit_label = "% of LTFU patients",
        efficacy = 0.75,  # UPDATE: Return-to-care rate
        eligible_pop = "ltfu",
        unit_cost = 20,   # UPDATE: Cost per person traced
        outcomes = c("retention")
      ),
      anc_vl_testing = list(
        name = "ANC: Viral Load Testing",
        type = "coverage",
        unit_label = "% of pregnant women on ART",
        efficacy = 0.88,  # UPDATE: VL suppression in pregnancy
        eligible_pop = "pregnant_on_art",
        unit_cost = 25,   # UPDATE: Cost per VL test
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
        efficacy = 1.0,  # Just a screening test
        eligible_pop = "new_art_initiations",
        unit_cost = 15,  # UPDATE: Cost per CD4 test
        outcomes = c("ahd_screening")
      ),
      ahd_package = list(
        name = "Full AHD package (LAM, CrAg, fluconazole)",
        type = "coverage",
        unit_label = "% of those with CD4<200",
        efficacy = 0.85,  # UPDATE: Mortality reduction from AHD package
        eligible_pop = "advanced_disease",
        proportion_advanced = 0.20,  # UPDATE: Proportion with CD4<200
        unit_cost = 50,   # UPDATE: Cost of LAM + CrAg + fluconazole (not including CD4)
        outcomes = c("mortality")
      )
    )
  )
)

# ============================================================================
# DEFAULT BASELINE INTERVENTIONS (used when not in CSV)
# ============================================================================

default_baseline_interventions <- list(
  prep_oral = 5000, prep_lenacapavir = 0, vmmc = 30000,
  condoms = 200000, pep = 2000, infant_prophylaxis = 70,
  cotrimoxazole = 60, test_facility = 50000, test_community = 20000,
  test_kpsti = 8000, hivst_facility = 10000, hivst_community = 5000,
  eid = 75, anc_hiv_testing = 88,
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
        aids_deaths_per_year = row$aids_deaths_per_year
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
  
  # Add Custom Country option
  presets[["Custom Country"]] <- list(
    description = "Enter your own parameters",
    context = list(
      total_population = 1000000,
      hiv_prevalence = 0.08,
      new_infections_per_year = 5000,
      current_diagnoses = 3500,
      percent_diagnosed=85,
      percent_on_art = 75,
      percent_suppressed = 85,
      aids_deaths_per_year = 800
    ),
    baseline = default_baseline_interventions
  )
  
  return(presets)
}

# Build presets
regional_presets <- build_country_presets(country_data_csv)

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
  births <- context$total_population * 0.035  # UPDATE: Birth rate
  hiv_positive_births <- births * context$hiv_prevalence * 1.5  # UPDATE: Prevalence multiplier
  
  list(
    total = context$total_population,
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
    uncircumcised_males = (hiv_negative * 0.50) * 0.25,  # UPDATE: Male proportion * uncircumcised rate
    sexually_active_negative = (hiv_negative * 0.60),  # UPDATE: Sexual activity
    recent_exposure = hiv_negative * 0.002,  # UPDATE: PEP need
    hiv_exposed_infants = hiv_positive_births,
    pregnant_women = births,
    pregnant_on_art = births * context$hiv_prevalence * 0.85,
    newly_diagnosed_advanced = (plhiv - diagnosed) * 0.20  # UPDATE: Advanced disease %
  )
}

# ============================================================================
# IMPACT CALCULATION FUNCTION
# ============================================================================

calculate_impact <- function(context, baseline, target, populations) {
  # Initialize outcome counters
  infections_averted <- 0
  infant_infections_averted <- 0
  deaths_averted <- 0
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
  base_test_yield <- (populations$undiagnosed + populations$ltfu) / populations$sexually_active
  base_test_yield <- min(base_test_yield, 0.10)  # Cap at 10% for plausibility
  
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
    if (delta == 0) next
    
    # Get eligible population
    eligible <- populations[[intervention$eligible_pop]]
    if (is.null(eligible)) eligible <- 0
    
    # Calculate number reached
    number_reached <- abs(delta)
    if (intervention$type == "coverage") {
      number_reached <- eligible * (abs(delta) / 100)
    }
    number_reached <- min(number_reached, eligible)
    
    # Sign for scale-up/down
    sign <- ifelse(delta > 0, 1, -1)
    
    # Calculate outcomes based on intervention type
    if ("testing" %in% intervention$outcomes) {
      # Testing interventions: calculate yield and split new vs re-engagement
      test_yield <- base_test_yield
      if (!is.null(intervention$test_yield_multiplier)) {
        test_yield <- test_yield * intervention$test_yield_multiplier
      }
      
      positive_tests <- number_reached * test_yield * intervention$efficacy
      tests_performed <- tests_performed + sign * number_reached
      
      # 50% are new diagnoses, 50% are re-engagement
      new_dx <- positive_tests * 0.50
      re_eng <- positive_tests * 0.50
      
      new_diagnoses <- new_diagnoses + sign * new_dx
      re_engagement <- re_engagement + sign * re_eng
      
      # ART initiations based on linkage rate
      linkage_rate <- intervention$linkage_rate
      linked <- positive_tests * linkage_rate
      art_initiations <- art_initiations + sign * linked
      
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
  
  list(
    infections_averted = round(infections_averted),
    infant_infections_averted = round(infant_infections_averted),
    total_infections_averted = round(infections_averted + infant_infections_averted),
    deaths_averted = round(deaths_averted),
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
    new_deaths = round(new_deaths)
  )
}

# ============================================================================
# USER INTERFACE
# ============================================================================

ui <- page_sidebar(
  title = "HIV Intervention Impact Calculator",
  sidebar = sidebar(
    width = 300,
    selectInput(
      "region",
      "Select Regional Profile:",
      choices = names(regional_presets),
      selected = "Eastern Africa - High Prevalence"
    ),
    hr(),
    h5("Epidemic Parameters"),
    numericInput("total_pop", "Total Population:", value = 5000000, min = 0),
    numericInput("prevalence", "HIV Prevalence (%):", value = 4.5, min = 0, max = 100, step = 0.1),
    numericInput("new_infections", "New Infections/Year:", value = 8500, min = 0),
    numericInput("current_dx", "Current Diagnoses/Year:", value = 7000, min = 0),
    numericInput("pct_diagnosed", "% of PLHIV Diagnosed:", value = 85, min = 0, max = 100),  
    numericInput("pct_on_art", "% Diagnosed on ART:", value = 78, min = 0, max = 100),
    numericInput("pct_suppressed", "% on ART Suppressed:", value = 82, min = 0, max = 100),
    numericInput("aids_deaths", "AIDS Deaths/Year:", value = 2200, min = 0)
  ),
  
  navset_card_tab(
    nav_panel(
      "Baseline Coverage",
      uiOutput("baseline_ui")
    ),
    
    nav_panel(
      "Scenarios",
      h4("Adjust intervention coverage for two scenarios"),
      p(strong("Note:"), " Scale up (increase) or scale down (decrease) interventions. Clear labels show whether inputs are absolute numbers (people) or percentages (%)."),
      uiOutput("scenario_ui")
    ),
    
    nav_panel(
      "Results Comparison",
      div(
        style = "height: 80vh; overflow-y: auto; padding-right: 15px;",
        
        # 95-95-95 Goals Tracker
        h3("Progress Toward 95-95-95 Goals", class = "mt-3 mb-3"),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_header(class = "bg-secondary text-white", "Baseline"),
            card_body(uiOutput("goals_baseline"))
          ),
          card(
            card_header(class = "bg-primary text-white", "Scenario 1"),
            card_body(uiOutput("goals_scenario1"))
          ),
          card(
            card_header(class = "bg-danger text-white", "Scenario 2"),
            card_body(uiOutput("goals_scenario2"))
          )
        ),
        
        # Epidemiological Outcomes Scorecard
        h3("Key Epidemiological Outcomes", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(4, 4, 4),
          card(
            card_header(class = "bg-secondary text-white", "Baseline"),
            card_body(uiOutput("epi_baseline"))
          ),
          card(
            card_header(class = "bg-primary text-white", "Scenario 1"),
            card_body(uiOutput("epi_scenario1"))
          ),
          card(
            card_header(class = "bg-danger text-white", "Scenario 2"),
            card_body(uiOutput("epi_scenario2"))
          )
        ),
        
        # Health Outcomes Row
        h3("Health Outcomes", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class = "bg-primary text-white", "Scenario 1 - Health Outcomes"),
            card_body(uiOutput("results_scenario1_health"))
          ),
          card(
            card_header(class = "bg-primary text-white", "Scenario 2 - Health Outcomes"),
            card_body(uiOutput("results_scenario2_health"))
          )
        ),
        
        # Cost Analysis Row
        h3("Cost Analysis", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header(class = "bg-info text-white", "Scenario 1 - Cost Analysis"),
            card_body(uiOutput("results_scenario1_cost"))
          ),
          card(
            card_header(class = "bg-info text-white", "Scenario 2 - Cost Analysis"),
            card_body(uiOutput("results_scenario2_cost"))
          )
        ),
        
        # Cascade Chart - Combined
        h3("HIV Care Cascade: Baseline vs Scenarios", class = "mt-4 mb-3"),
        card(
          card_header("Combined Cascade Comparison"),
          card_body(plotOutput("cascade_combined", height = "500px"))
        ),
        
        # Other Outcomes Row
        h3("Key Outcomes Summary", class = "mt-4 mb-3"),
        layout_columns(
          col_widths = c(6, 6),
          card(
            card_header("Scenario 1 - Impact Summary"),
            card_body(plotOutput("plot_scenario1", height = "400px"))
          ),
          card(
            card_header("Scenario 2 - Impact Summary"),
            card_body(plotOutput("plot_scenario2", height = "400px"))
          )
        ),
        
        # Add some bottom padding
        div(style = "height: 50px;")
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Store original baseline to scale proportionally
  original_population <- reactiveVal(5000000)
  original_baseline <- reactiveVal(NULL)
  
  # Load regional preset when selected
  observeEvent(input$region, {
    preset <- regional_presets[[input$region]]
    updateNumericInput(session, "total_pop", value = preset$context$total_population)
    updateNumericInput(session, "prevalence", value = preset$context$hiv_prevalence * 100)
    updateNumericInput(session, "new_infections", value = preset$context$new_infections_per_year)
    updateNumericInput(session, "current_dx", value = preset$context$current_diagnoses)
    updateNumericInput(session, "pct_diagnosed", value = preset$context$percent_diagnosed)  
    updateNumericInput(session, "pct_on_art", value = preset$context$percent_on_art)
    updateNumericInput(session, "pct_suppressed", value = preset$context$percent_suppressed)
    updateNumericInput(session, "aids_deaths", value = preset$context$aids_deaths_per_year)
    
    original_population(preset$context$total_population)
    original_baseline(preset$baseline)
  }, ignoreInit = FALSE)
  
  # Reactive context
  context <- reactive({
    list(
      total_population = input$total_pop,
      hiv_prevalence = input$prevalence / 100,
      new_infections_per_year = input$new_infections,
      current_diagnoses = input$current_dx,
      percent_on_art = input$pct_on_art,
      percent_suppressed = input$pct_suppressed,
      aids_deaths_per_year = input$aids_deaths,
      percent_diagnosed = input$pct_diagnosed
    )
    
    
  })
  
  # Calculate populations
  populations <- reactive({
    calculate_populations(context())
  })
  
  # Scaled baseline values
  baseline_values <- reactive({
    if (is.null(original_baseline())) return(list())
    
    scale_factor <- input$total_pop / original_population()
    baseline <- original_baseline()
    
    # Scale absolute interventions only
    scaled <- lapply(names(baseline), function(key) {
      # Find intervention
      for (group_key in names(intervention_groups)) {
        group <- intervention_groups[[group_key]]
        if (key %in% names(group$interventions)) {
          intervention <- group$interventions[[key]]
          if (intervention$type == "absolute") {
            return(round(baseline[[key]] * scale_factor))
          } else {
            return(baseline[[key]])  # Coverage stays same
          }
        }
      }
      return(baseline[[key]])
    })
    names(scaled) <- names(baseline)
    scaled
  })
  
  # Generate baseline UI
  output$baseline_ui <- renderUI({
    baseline <- baseline_values()
    if (length(baseline) == 0) return(NULL)
    
    ui_elements <- lapply(names(intervention_groups), function(group_key) {
      group <- intervention_groups[[group_key]]
      
      interventions_ui <- lapply(names(group$interventions), function(int_key) {
        intervention <- group$interventions[[int_key]]
        value <- ifelse(is.null(baseline[[int_key]]), 0, baseline[[int_key]])
        
        numericInput(
          paste0("baseline_", int_key),
          label = paste0(intervention$name, " (", intervention$unit_label, ")"),
          value = value,
          min = 0
        )
      })
      
      tagList(
        h4(group$name, style = paste0("color: ", group$color, "; border-left: 4px solid ", 
                                      group$color, "; padding-left: 10px;")),
        interventions_ui
      )
    })
    
    tagList(ui_elements)
  })
  
  # Generate scenario UI (side-by-side)
  output$scenario_ui <- renderUI({
    baseline <- baseline_values()
    if (length(baseline) == 0) return(NULL)
    
    scenario_columns <- lapply(names(intervention_groups), function(group_key) {
      group <- intervention_groups[[group_key]]
      
      interventions_ui <- lapply(names(group$interventions), function(int_key) {
        intervention <- group$interventions[[int_key]]
        base_value <- ifelse(is.null(baseline[[int_key]]), 0, baseline[[int_key]])
        
        # Special styling for MMD interventions
        is_mmd <- grepl("mmd_", int_key)
        
        layout_columns(
          col_widths = c(4, 4, 4),
          div(
            style = "padding-top: 25px; font-size: 0.9em;",
            strong(intervention$name),
            br(),
            span(style = "color: #666;", "Baseline: ", base_value),
            br(),
            span(style = "color: #999; font-size: 0.85em;", intervention$unit_label)
          ),
          div(
            numericInput(
              paste0("scenario1_", int_key),
              label = "Scenario 1",
              value = base_value,
              min = 0
            ),
            if (is_mmd) uiOutput(paste0("mmd_warning1_", int_key))
          ),
          div(
            numericInput(
              paste0("scenario2_", int_key),
              label = "Scenario 2",
              value = base_value,
              min = 0
            ),
            if (is_mmd) uiOutput(paste0("mmd_warning2_", int_key))
          )
        )
      })
      
      # Add MMD coverage indicator at end of monitoring section
      if (group_key == "treatment_monitoring") {
        interventions_ui <- c(interventions_ui, list(
          layout_columns(
            col_widths = c(4, 4, 4),
            div(),
            div(
              h6("MMD Coverage:", style = "margin-top: 15px;"),
              uiOutput("mmd_total_scenario1")
            ),
            div(
              h6("MMD Coverage:", style = "margin-top: 15px;"),
              uiOutput("mmd_total_scenario2")
            )
          )
        ))
      }
      
      tagList(
        h4(group$name, style = paste0("color: ", group$color, "; border-left: 4px solid ", 
                                      group$color, "; padding-left: 10px; margin-top: 20px;")),
        interventions_ui
      )
    })
    
    tagList(scenario_columns)
  })
  
  # MMD coverage warnings
  output$mmd_total_scenario1 <- renderUI({
    mmd3 <- input$scenario1_mmd_3month
    mmd6 <- input$scenario1_mmd_6month
    mmd12 <- input$scenario1_mmd_12month
    
    if (is.null(mmd3) || is.null(mmd6) || is.null(mmd12)) return(NULL)
    
    total <- mmd3 + mmd6 + mmd12
    color <- ifelse(total > 100, "red", ifelse(total > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(total, "% of stable clients")
    )
  })
  
  output$mmd_total_scenario2 <- renderUI({
    mmd3 <- input$scenario2_mmd_3month
    mmd6 <- input$scenario2_mmd_6month
    mmd12 <- input$scenario2_mmd_12month
    
    if (is.null(mmd3) || is.null(mmd6) || is.null(mmd12)) return(NULL)
    
    total <- mmd3 + mmd6 + mmd12
    color <- ifelse(total > 100, "red", ifelse(total > 90, "orange", "green"))
    
    tags$div(
      style = paste0("color: ", color, "; font-weight: bold;"),
      paste0(total, "% of stable clients")
    )
  })
  
  # Collect baseline values from inputs
  baseline_input_values <- reactive({
    baseline <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("baseline_", int_key)
        baseline[[int_key]] <- input[[input_id]]
      }
    }
    baseline
  })
  
  # Collect scenario 1 values
  scenario1_values <- reactive({
    scenario <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("scenario1_", int_key)
        scenario[[int_key]] <- input[[input_id]]
      }
    }
    scenario
  })
  
  # Collect scenario 2 values
  scenario2_values <- reactive({
    scenario <- list()
    for (group_key in names(intervention_groups)) {
      group <- intervention_groups[[group_key]]
      for (int_key in names(group$interventions)) {
        input_id <- paste0("scenario2_", int_key)
        scenario[[int_key]] <- input[[input_id]]
      }
    }
    scenario
  })
  
  # Calculate impacts
  impact_scenario1 <- reactive({
    req(populations())
    calculate_impact(context(), baseline_input_values(), scenario1_values(), populations())
  })
  
  impact_scenario2 <- reactive({
    req(populations())
    calculate_impact(context(), baseline_input_values(), scenario2_values(), populations())
  })
  
  # Calculate 95-95-95 metrics
  calculate_95goals <- function(populations, impact = NULL) {
    if (is.null(impact)) {
      # Baseline
      first_95 <- (populations$diagnosed / populations$plhiv) * 100
      second_95 <- (populations$on_art / populations$diagnosed) * 100
      third_95 <- (populations$suppressed / populations$on_art) * 100
    } else {
      # After intervention
      first_95 <- (impact$new_diagnosed / populations$plhiv) * 100
      second_95 <- (impact$new_on_art / impact$new_diagnosed) * 100
      third_95 <- (impact$new_suppressed / impact$new_on_art) * 100
    }
    
    list(
      first_95 = round(first_95, 1),
      second_95 = round(second_95, 1),
      third_95 = round(third_95, 1)
    )
  }
  
  # 95-95-95 Goals - Baseline
  output$goals_baseline <- renderUI({
    pops <- populations()
    goals <- calculate_95goals(pops)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(goals$first_95, "%"))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(goals$second_95, "%"))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(style = "font-size: 1.3em; font-weight: bold;",
               paste0(goals$third_95, "%"))
      )
    )
  })
  
  # 95-95-95 Goals - Scenario 1
  output$goals_scenario1 <- renderUI({
    pops <- populations()
    impact <- impact_scenario1()
    baseline_goals <- calculate_95goals(pops)
    goals <- calculate_95goals(pops, impact)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$first_95 > baseline_goals$first_95, "green",
                                  ifelse(goals$first_95 < baseline_goals$first_95, "red", "gray")), ";"),
            paste0(goals$first_95, "%")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$second_95 > baseline_goals$second_95, "green",
                                  ifelse(goals$second_95 < baseline_goals$second_95, "red", "gray")), ";"),
            paste0(goals$second_95, "%")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$third_95 > baseline_goals$third_95, "green",
                                  ifelse(goals$third_95 < baseline_goals$third_95, "red", "gray")), ";"),
            paste0(goals$third_95, "%")
          )
      )
    )
  })
  
  # 95-95-95 Goals - Scenario 2
  output$goals_scenario2 <- renderUI({
    pops <- populations()
    impact <- impact_scenario2()
    baseline_goals <- calculate_95goals(pops)
    goals <- calculate_95goals(pops, impact)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("1st 95:"), " % of PLHIV diagnosed"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$first_95 > baseline_goals$first_95, "green",
                                  ifelse(goals$first_95 < baseline_goals$first_95, "red", "gray")), ";"),
            paste0(goals$first_95, "%")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("2nd 95:"), " % of diagnosed on ART"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$second_95 > baseline_goals$second_95, "green",
                                  ifelse(goals$second_95 < baseline_goals$second_95, "red", "gray")), ";"),
            paste0(goals$second_95, "%")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("3rd 95:"), " % on ART suppressed"),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(goals$third_95 > baseline_goals$third_95, "green",
                                  ifelse(goals$third_95 < baseline_goals$third_95, "red", "gray")), ";"),
            paste0(goals$third_95, "%")
          )
      )
    )
  })
  
  # Epidemiological Outcomes - Baseline
  output$epi_baseline <- renderUI({
    ctx <- context()
    pops <- populations()
    
    # Calculate baseline infant infections
    baseline_infant_infections <- round(pops$hiv_exposed_infants * 0.15)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Adult Infections:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(ctx$new_infections_per_year, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Infant Infections:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(baseline_infant_infections, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("HIV-Related Deaths:")),
          span(style = "font-size: 1.3em; font-weight: bold;",
               format(ctx$aids_deaths_per_year, big.mark = ","))
      )
    )
  })
  
  # Epidemiological Outcomes - Scenario 1
  output$epi_scenario1 <- renderUI({
    ctx <- context()
    pops <- populations()
    impact <- impact_scenario1()
    
    baseline_infant_infections <- round(pops$hiv_exposed_infants * 0.15)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Adult Infections:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_infections < ctx$new_infections_per_year, "green",
                                  ifelse(impact$new_infections > ctx$new_infections_per_year, "red", "gray")), ";"),
            format(impact$new_infections, big.mark = ",")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Infant Infections:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_infant_infections < baseline_infant_infections, "green",
                                  ifelse(impact$new_infant_infections > baseline_infant_infections, "red", "gray")), ";"),
            format(impact$new_infant_infections, big.mark = ",")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("HIV-Related Deaths:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_deaths < ctx$aids_deaths_per_year, "green",
                                  ifelse(impact$new_deaths > ctx$aids_deaths_per_year, "red", "gray")), ";"),
            format(impact$new_deaths, big.mark = ",")
          )
      )
    )
  })
  
  # Epidemiological Outcomes - Scenario 2
  output$epi_scenario2 <- renderUI({
    ctx <- context()
    pops <- populations()
    impact <- impact_scenario2()
    
    baseline_infant_infections <- round(pops$hiv_exposed_infants * 0.15)
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Adult Infections:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_infections < ctx$new_infections_per_year, "green",
                                  ifelse(impact$new_infections > ctx$new_infections_per_year, "red", "gray")), ";"),
            format(impact$new_infections, big.mark = ",")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("New Infant Infections:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_infant_infections < baseline_infant_infections, "green",
                                  ifelse(impact$new_infant_infections > baseline_infant_infections, "red", "gray")), ";"),
            format(impact$new_infant_infections, big.mark = ",")
          )
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(strong("HIV-Related Deaths:")),
          span(
            style = paste0("font-size: 1.3em; font-weight: bold; color: ",
                           ifelse(impact$new_deaths < ctx$aids_deaths_per_year, "green",
                                  ifelse(impact$new_deaths > ctx$aids_deaths_per_year, "red", "gray")), ";"),
            format(impact$new_deaths, big.mark = ",")
          )
      )
    )
  })
  
  # Render results - Scenario 1 Health
  output$results_scenario1_health <- renderUI({
    impact <- impact_scenario1()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Tests Performed:"),
          span(style = "font-weight: bold;", format(impact$tests_performed, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("New Diagnoses:"),
          span(class = ifelse(impact$new_diagnoses >= 0, "text-primary", "text-warning"),
               style = "font-weight: bold;",
               format(impact$new_diagnoses, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Re-engagement in Care:"),
          span(class = ifelse(impact$re_engagement >= 0, "text-primary", "text-warning"),
               style = "font-weight: bold;",
               format(impact$re_engagement, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("ART Initiations:"),
          strong(class = ifelse(impact$art_initiations >= 0, "text-success", "text-danger"),
                 format(impact$art_initiations, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Adult Infections Averted:"),
          span(class = ifelse(impact$infections_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Infant Infections Averted:"),
          span(class = ifelse(impact$infant_infections_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$infant_infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("Total Infections Averted:"),
          strong(class = ifelse(impact$total_infections_averted >= 0, "text-success", "text-danger"),
                 format(impact$total_infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Deaths Averted:"),
          span(class = ifelse(impact$deaths_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$deaths_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Additional Suppressed:"),
          span(class = "text-primary", style = "font-weight: bold;",
               format(impact$additional_suppressed, big.mark = ","))
      )
    )
  })
  
  # Render results - Scenario 2 Health
  output$results_scenario2_health <- renderUI({
    impact <- impact_scenario2()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Tests Performed:"),
          span(style = "font-weight: bold;", format(impact$tests_performed, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("New Diagnoses:"),
          span(class = ifelse(impact$new_diagnoses >= 0, "text-primary", "text-warning"),
               style = "font-weight: bold;",
               format(impact$new_diagnoses, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Re-engagement in Care:"),
          span(class = ifelse(impact$re_engagement >= 0, "text-primary", "text-warning"),
               style = "font-weight: bold;",
               format(impact$re_engagement, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("ART Initiations:"),
          strong(class = ifelse(impact$art_initiations >= 0, "text-success", "text-danger"),
                 format(impact$art_initiations, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Adult Infections Averted:"),
          span(class = ifelse(impact$infections_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Infant Infections Averted:"),
          span(class = ifelse(impact$infant_infections_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$infant_infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3",
          strong("Total Infections Averted:"),
          strong(class = ifelse(impact$total_infections_averted >= 0, "text-success", "text-danger"),
                 format(impact$total_infections_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Deaths Averted:"),
          span(class = ifelse(impact$deaths_averted >= 0, "text-success", "text-danger"),
               style = "font-weight: bold;",
               format(impact$deaths_averted, big.mark = ","))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Additional Suppressed:"),
          span(class = "text-primary", style = "font-weight: bold;",
               format(impact$additional_suppressed, big.mark = ","))
      )
    )
  })
  
  # Render results - Scenario 1 Cost
  output$results_scenario1_cost <- renderUI({
    impact <- impact_scenario1()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Intervention Costs (scale-up):"),
          span(class = "text-primary", style = "font-weight: bold;",
               paste0("$", format(impact$total_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Savings (scale-down):"),
          span(class = "text-success", style = "font-weight: bold;",
               paste0("$", format(impact$cost_savings, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(style = "font-style: italic; font-size: 0.9em;", "ART Provision (outcome-driven):"),
          span(style = "font-weight: bold; color: #8b5cf6;",
               paste0("$", format(impact$art_provision_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3 bg-light p-2",
          strong("Net Budget Impact:"),
          strong(class = ifelse(impact$net_cost >= 0, "text-primary", "text-success"),
                 paste0(ifelse(impact$net_cost >= 0, "+", ""), "$", 
                        format(impact$net_cost, big.mark = ",")))
      ),
      if (impact$total_infections_averted > 0) {
        div(class = "d-flex justify-content-between border-bottom pb-2 mb-2 mt-3",
            span("Cost per Infection Averted:"),
            span(style = "font-weight: bold;",
                 paste0("$", format(impact$cost_per_infection_averted, big.mark = ",")))
        )
      },
      if (impact$deaths_averted > 0) {
        div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
            span("Cost per Death Averted:"),
            span(style = "font-weight: bold;",
                 paste0("$", format(impact$cost_per_death_averted, big.mark = ",")))
        )
      }
    )
  })
  
  # Render results - Scenario 2 Cost
  output$results_scenario2_cost <- renderUI({
    impact <- impact_scenario2()
    
    tagList(
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Intervention Costs (scale-up):"),
          span(class = "text-primary", style = "font-weight: bold;",
               paste0("$", format(impact$total_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span("Savings (scale-down):"),
          span(class = "text-success", style = "font-weight: bold;",
               paste0("$", format(impact$cost_savings, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
          span(style = "font-style: italic; font-size: 0.9em;", "ART Provision (outcome-driven):"),
          span(style = "font-weight: bold; color: #8b5cf6;",
               paste0("$", format(impact$art_provision_cost, big.mark = ",")))
      ),
      div(class = "d-flex justify-content-between border-bottom pb-2 mb-3 bg-light p-2",
          strong("Net Budget Impact:"),
          strong(class = ifelse(impact$net_cost >= 0, "text-primary", "text-success"),
                 paste0(ifelse(impact$net_cost >= 0, "+", ""), "$", 
                        format(impact$net_cost, big.mark = ",")))
      ),
      if (impact$total_infections_averted > 0) {
        div(class = "d-flex justify-content-between border-bottom pb-2 mb-2 mt-3",
            span("Cost per Infection Averted:"),
            span(style = "font-weight: bold;",
                 paste0("$", format(impact$cost_per_infection_averted, big.mark = ",")))
        )
      },
      if (impact$deaths_averted > 0) {
        div(class = "d-flex justify-content-between border-bottom pb-2 mb-2",
            span("Cost per Death Averted:"),
            span(style = "font-weight: bold;",
                 paste0("$", format(impact$cost_per_death_averted, big.mark = ",")))
        )
      }
    )
  })
  
  # COMBINED CASCADE PLOT
  output$cascade_combined <- renderPlot({
    impact1 <- impact_scenario1()
    impact2 <- impact_scenario2()
    pops <- populations()
    
    # Create data frame with all three scenarios
    cascade_data <- data.frame(
      Stage = rep(c("Diagnosed", "On ART", "Suppressed"), 3),
      Scenario = rep(c("Baseline", "Scenario 1", "Scenario 2"), each = 3),
      Value = c(
        # Baseline
        pops$diagnosed, pops$on_art, pops$suppressed,
        # Scenario 1
        impact1$new_diagnosed, impact1$new_on_art, impact1$new_suppressed,
        # Scenario 2
        impact2$new_diagnosed, impact2$new_on_art, impact2$new_suppressed
      )
    )
    
    cascade_data$Stage <- factor(cascade_data$Stage, levels = c("Diagnosed", "On ART", "Suppressed"))
    cascade_data$Scenario <- factor(cascade_data$Scenario, levels = c("Baseline", "Scenario 1", "Scenario 2"))
    
    ggplot(cascade_data, aes(x = Stage, y = Value, color = Scenario, group = Scenario, linetype = Scenario)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Baseline" = "#666666", "Scenario 1" = "#2563eb", "Scenario 2" = "#dc2626")) +
      scale_linetype_manual(values = c("Baseline" = "solid", "Scenario 1" = "solid", "Scenario 2" = "dashed")) +
      scale_y_continuous(labels = comma) +
      labs(title = "HIV Care Cascade Comparison",
           subtitle = "Gray = Baseline | Blue solid = Scenario 1 | Red dashed = Scenario 2",
           y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(legend.position = "top",
            legend.title = element_blank(),
            plot.title = element_text(size = 16, face = "bold"),
            plot.subtitle = element_text(size = 12),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 12))
  })
  
  # CASCADE LINE PLOTS (keep individual ones but remove deaths)
  output$cascade_scenario1 <- renderPlot({
    impact <- impact_scenario1()
    pops <- populations()
    
    cascade_data <- data.frame(
      Stage = factor(c("Diagnosed", "On ART", "Suppressed", "Annual Deaths"),
                     levels = c("Diagnosed", "On ART", "Suppressed", "Annual Deaths")),
      Baseline = c(pops$diagnosed, pops$on_art, pops$suppressed, context()$aids_deaths_per_year),
      Intervention = c(impact$new_diagnosed, impact$new_on_art, impact$new_suppressed, impact$new_deaths)
    )
    
    cascade_long <- cascade_data %>%
      pivot_longer(cols = c(Baseline, Intervention), names_to = "Scenario", values_to = "Value")
    
    ggplot(cascade_long, aes(x = Stage, y = Value, color = Scenario, group = Scenario)) +
      geom_line(linewidth = 1.5) +
      geom_point(size = 4) +
      scale_color_manual(values = c("Baseline" = "#666666", "Intervention" = "#2563eb")) +
      scale_y_continuous(labels = comma) +
      labs(title = "HIV Care Cascade: Baseline → Scenario 1",
           subtitle = "Blue line shows impact of intervention changes",
           y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 11))
  })
  
  output$cascade_scenario2 <- renderPlot({
    impact <- impact_scenario2()
    pops <- populations()
    
    cascade_data <- data.frame(
      Stage = factor(c("Diagnosed", "On ART", "Suppressed", "Annual Deaths"),
                     levels = c("Diagnosed", "On ART", "Suppressed", "Annual Deaths")),
      Baseline = c(pops$diagnosed, pops$on_art, pops$suppressed, context()$aids_deaths_per_year),
      Intervention = c(impact$new_diagnosed, impact$new_on_art, impact$new_suppressed, impact$new_deaths)
    )
    
    cascade_long <- cascade_data %>%
      pivot_longer(cols = c(Baseline, Intervention), names_to = "Scenario", values_to = "Value")
    
    ggplot(cascade_long, aes(x = Stage, y = Value, color = Scenario, group = Scenario)) +
      geom_line(linewidth = 1.5, linetype = "dashed") +
      geom_point(size = 4) +
      scale_color_manual(values = c("Baseline" = "#666666", "Intervention" = "#dc2626")) +
      scale_y_continuous(labels = comma) +
      labs(title = "HIV Care Cascade: Baseline → Scenario 2",
           subtitle = "Red dashed line shows impact of intervention changes",
           y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(legend.position = "top",
            plot.title = element_text(size = 14, face = "bold"),
            axis.text = element_text(size = 11))
  })
  
  # OTHER OUTCOMES BAR PLOTS (include deaths here)
  output$plot_scenario1 <- renderPlot({
    impact <- impact_scenario1()
    
    plot_data <- data.frame(
      Outcome = c("Infections\nAverted", "Deaths\nAverted", "ART\nInitiations"),
      Value = c(impact$total_infections_averted, impact$deaths_averted, impact$art_initiations)
    )
    
    ggplot(plot_data, aes(x = Outcome, y = Value, fill = Value >= 0)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#ef4444"), guide = "none") +
      scale_y_continuous(labels = comma) +
      labs(title = "Key Outcomes Summary", y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 14, face = "bold"))
  })
  
  output$plot_scenario2 <- renderPlot({
    impact <- impact_scenario2()
    
    plot_data <- data.frame(
      Outcome = c("Infections\nAverted", "Deaths\nAverted", "ART\nInitiations"),
      Value = c(impact$total_infections_averted, impact$deaths_averted, impact$art_initiations)
    )
    
    ggplot(plot_data, aes(x = Outcome, y = Value, fill = Value >= 0)) +
      geom_col(width = 0.7) +
      scale_fill_manual(values = c("TRUE" = "#10b981", "FALSE" = "#ef4444"), guide = "none") +
      scale_y_continuous(labels = comma) +
      labs(title = "Key Outcomes Summary", y = "Number of People", x = NULL) +
      theme_minimal() +
      theme(axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 11),
            plot.title = element_text(size = 14, face = "bold"))
  })
}

# ============================================================================
# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)