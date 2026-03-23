library(tidyverse)
library(readxl)
options(scipen=999)
rm(list=ls())
library(WDI)

#Add columns: circ_prevalence (e.g. 0.20 Ethiopia, 0.45 Uganda, 0.35 Kenya) and prop_high_risk (typically 0.05–0.10). rr_high is optional (default 8).

##UPDATE DIAGNOSES

data_dir="/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/"

est_data=read.csv(paste(data_dir,"hiv_unaids_estimates_2025.csv",sep=""))
est_data$label=paste(est_data$Indicator_GId,est_data$Subgroup,sep="_")

est_indicators=read_excel(paste(data_dir,"data_list.xlsx",sep=""),sheet="est")

est_data_filt=est_data %>% 
  semi_join(est_indicators, by = c("Indicator","Indicator_GId","Subgroup")) %>% 
  group_by(Indicator,Indicator_GId,Unit,Subgroup,Area) %>%  
  arrange(Area,Indicator,Indicator_GId,Unit,Subgroup,desc(Time.Period)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  unique() 


##gam DATA
gam_data=read.csv(paste(data_dir,"GAM_2025_en.csv",sep=""))
gam_data$label=paste(gam_data$Indicator_GId,gam_data$Subgroup)

gam_indicators=read_excel(paste(data_dir,"data_list.xlsx",sep=""),sheet="GAM")

gam_data_sub=gam_data %>% select(Area,Area.ID,Indicator,Indicator_GId,Subgroup,label,year=Time.Period,Data.value) %>% 
  filter(label%in%c("MALE_CIRCUMCISIONS_PERFORMED All ages","PREVALENCE_MALE_CIRCUMCISION Adults (15-49)","PEOPLE_ON_PREP Total",
                    "CONDOMS_DISTRIBUTED Male condoms Total","CONDOMS_DISTRIBUTED Female condoms Total",
                    "MSM_POPULATION_SIZE estimate","SEX_WORKERS_POPULATION_SIZE estimate","PWID_POPULATION_SIZE estimate","TG_POPULATION_SIZE estimate","POPULATION Total",
                    "COVERAGE_DSD_ART_MODELS Adults (15+)")) %>% 
group_by(Indicator,Indicator_GId,Subgroup,Area) %>%  
  arrange(Area,Indicator,Indicator_GId,Subgroup,desc(year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  unique() 

gam_data_circum=gam_data_sub %>% select(country=Area,Indicator_GId,Data.value) %>% filter(Indicator_GId=="PREVALENCE_MALE_CIRCUMCISION") %>% spread(Indicator_GId,Data.value) %>% 
  rename(circ_prevalence=PREVALENCE_MALE_CIRCUMCISION)


######worldbank data

pop_data= WDI(indicator =  c( "SP.POP.TOTL","SP.POP.TOTL.MA.ZS","SP.POP.0014.TO.ZS"),start=2024) %>% select(-year)
pop_data= pop_data %>% left_join(WDI(indicator =  c( "SP.DYN.CBRT.IN"),start=2023,end = 2023))
pop_data=pop_data %>% select(Area=country,Area.ID=iso3c,population=SP.POP.TOTL,prop_male=SP.POP.TOTL.MA.ZS,prop_under14=SP.POP.0014.TO.ZS,birth_rate=SP.DYN.CBRT.IN)

#inds=WDIsearch("pop")
###BAsic data

basic_data=est_data %>% 
  filter(label %in% c("PLHIV_KNOWLEDGE_OF_STATUS_All ages estimate","PERCENT_KNOW_STATUS_ON_ART_All ages estimate",
                      "PERCENT_ON_ART_VL_SUPPRESSED_All ages estimate","HIV_PREVALENCE_Adults (15+) estimate","NEW_INFECTIONS_All ages estimate","AIDS_DEATHS_All ages estimate"))%>% 
  select(Area,Area.ID,Indicator_GId,Time.Period,Data.value) %>% 
  group_by(Area,Indicator_GId) %>% 
  arrange(desc(Time.Period)) %>% 
  slice(1) %>% 
  select(Area,Area.ID,Indicator_GId,Data.value) %>% 
  filter(is.na(Data.value)==FALSE) %>% spread(Indicator_GId,Data.value) %>% left_join(pop_data,by="Area.ID") %>% 
  filter(is.na(population)==FALSE) 

basic_data$diagnoses=(basic_data$PLHIV_KNOWLEDGE_OF_STATUS/100)*basic_data$NEW_INFECTIONS ##UPDATE


basic_data=basic_data%>% 
  rename(country=Area.x,total_population=population,hiv_prevalence=`HIV_PREVALENCE`,new_infections_per_year=`NEW_INFECTIONS`,
         current_diagnoses=  `diagnoses`,percent_on_art=`PERCENT_KNOW_STATUS_ON_ART`,percent_suppressed=`PERCENT_ON_ART_VL_SUPPRESSED`,percent_diagnosed=PLHIV_KNOWLEDGE_OF_STATUS,
         aids_deaths_per_year=`AIDS_DEATHS`) %>% 
  select("country", "total_population", "hiv_prevalence", 
         "new_infections_per_year", "current_diagnoses", "percent_diagnosed",
         "percent_on_art", "percent_suppressed", "aids_deaths_per_year","prop_male","prop_under14","birth_rate")

basic_data$hiv_prevalence=round(basic_data$hiv_prevalence,1)
basic_data$new_infections_per_year=round(basic_data$new_infections_per_year,0)
basic_data$current_diagnoses=round(basic_data$current_diagnoses,0)
basic_data$percent_suppressed=round(basic_data$percent_suppressed,1)
basic_data$percent_on_art=round(basic_data$percent_on_art,1)
basic_data$aids_deaths_per_year=round(basic_data$aids_deaths_per_year,0)
basic_data$percent_diagnosed=round(basic_data$percent_diagnosed,1)

basic_data$percent_diagnosed[basic_data$percent_diagnosed==100]=98

basic_data$percent_suppressed[basic_data$percent_suppressed==100]=98
basic_data$percent_on_art[basic_data$percent_on_art==100]=98


basic_data=left_join(basic_data,gam_data_circum)
all_ctr=basic_data$country


sub_countries=c("Botswana","Côte d'Ivoire","Eswatini","Ghana","Kenya","Lesotho","Malawi","Mozambique","Nigeria","Congo","South Africa","South Sudan","United Republic of Tanzania","Uganda","Zambia","Zimbabwe")

basic_data=basic_data %>% filter(country%in%sub_countries)



########KP data -UPDATE

kp_raw=read.csv(paste(data_dir,"KPAtlasDB_2025.csv",sep=""),quote = "")

kp_raw=kp_raw %>% rename(country=Area) %>% filter(country %in% sub_countries)%>% 
  filter(Indicator_GId %in%c("MSM_POPULATION_SIZE","SEX_WORKERS_POPULATION_SIZE","PWID_POPULATION_SIZE","TG_POPULATION_SIZE")) %>% filter(Subgroup=="estimate") %>% 
  select(country,Indicator_GId,Subgroup,Time.Period,Data.value) %>% group_by(country,Indicator_GId,Subgroup) %>% arrange(desc(Time.Period))%>% slice(1) %>% 
  select(country,Indicator_GId,Subgroup,Data.value)

kp_wide=kp_raw%>% 
  spread(Indicator_GId,Data.value)

kp_wide$kp_pop_estimate=rowSums(kp_wide[,c("MSM_POPULATION_SIZE","SEX_WORKERS_POPULATION_SIZE","PWID_POPULATION_SIZE","TG_POPULATION_SIZE")], na.rm=TRUE)
kp_wide=kp_wide %>% left_join(basic_data %>% select(country,total_population))

kp_wide$high_risk_pop=3*kp_wide$kp_pop_estimate #Assumes each KP indiviudal has a partner as part of teh network
kp_wide$pop_sexually_active=kp_wide$total_population*0.6 ##UPDATE
kp_wide$prop_high_risk=kp_wide$high_risk_pop/kp_wide$pop_sexually_active


kp_wide_final=kp_wide %>% ungroup() %>% select(country,prop_high_risk)
kp_wide_final$rr_high=8
#####Fix diagnoses

basic_data=left_join(basic_data, kp_wide_final)

write.csv(basic_data,"/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/basic_hiv_data.csv")

# 
# 
# ##################
# # ============================================================================
# # ESTIMATE prop_high_risk BY COUNTRY FROM UNAIDS KP ATLAS 2025
# # ============================================================================
# #
# # WHAT THIS DOES:
# #   1. Extracts country-level KP population size estimates (MSM, FSW, PWID, TG)
# #      using the most recent available year per KP type per country
# #   2. Sums KP counts, applying a partner multiplier to account for the
# #      immediate sexual partners of KP who bridge into the general population
# #      (KP size alone undercounts the "high-risk" stratum)
# #   3. Denominates by sexually active adults (15-49) to get prop_high_risk
# #   4. Joins with your country CSV so the output is ready to paste in
# #
# # INTERPRETATION NOTE:
# #   prop_high_risk = fraction of HIV-negative sexually active adults who sit
# #   in the high-risk transmission stratum (KP + close partners). This feeds
# #   directly into the FOI model's stratified infection calculation.
# #
# # KEY ASSUMPTION — PARTNER MULTIPLIER:
# #   KP size estimates capture the KP themselves, not their non-KP partners.
# #   A multiplier of 2.0 means "for every KP person, assume 1 additional
# #   partner in the high-risk network." Literature range is 1.5–3.0.
# #   Lower (1.5) = conservative; higher (3.0) = broader network assumption.
# #   Default = 2.0, consistent with models like Goals/Spectrum.
# #
# # PRISONERS NOTE:
# #   Prisoners are excluded from the KP sum. The sexual transmission pathway
# #   for prisoners is largely captured by MSM estimates, and including them
# #   would double-count. They matter for testing/care, not for the FOI stratum.
# #
# # REQUIRED INPUT FILES:
# #   1. KPAtlasDB_2025_en_3.csv  — UNAIDS KP Atlas download
# #   2. Your country CSV          — must have columns: country, total_population,
# #                                  prop_pop_under_14 (to derive adult pop)
# #
# # OUTPUT:
# #   prop_high_risk_by_country.csv — one row per country with prop_high_risk
# #                                   estimate, confidence bounds, and diagnostics
# # ============================================================================
# 
# library(dplyr)
# library(tidyr)
# library(readr)
# 
# # ============================================================================
# # PARAMETERS — ADJUST THESE
# # ============================================================================
# 
# #KP_ATLAS_FILE  <- "KPAtlasDB_2025_en_3.csv"   # path to KP Atlas CSV
# COUNTRY_CSV    <- "/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/basic_hiv_data.csv"      # path to your TIER country CSV
# 
# PARTNER_MULTIPLIER <- 2.0   # KP + partners multiplier (see note above)
# # Sensitivity range: 1.5 (low) to 3.0 (high)
# 
# # Proportion of total population that is sexually active adults (15-49)
# # Used as denominator when your country CSV lacks this directly.
# # If your CSV has prop_pop_under_14, we derive it; otherwise this default is used.
# DEFAULT_PROP_SEXUALLY_ACTIVE <- 0.6
# 
# # Minimum year to use (ignore very old estimates)
# MIN_YEAR <- 2015
# 
# # ============================================================================
# # STEP 1: LOAD AND FILTER KP ATLAS
# # ============================================================================
# 
# kp_raw=read.csv(paste(data_dir,"KPAtlasDB_2025.csv",sep=""),quote = "")
# 
# # Standardise column names (KP Atlas uses spaces)
# names(kp_raw) <- make.names(names(kp_raw))
# 
# # Keep only:
# #   - Country-level rows (3-character ISO Area ID)
# #   - Central ESTIMATE (not lower/upper bounds — handled separately below)
# #   - KP types relevant to sexual transmission
# #   - Recent years only
# kp_estimates <- kp_raw %>%
#   filter(
#     nchar(Area.ID) == 3,                                    # country level only
#     Subgroup_Val_GId == "ESTIMATE",
#     Indicator_GId %in% c(
#       "MSM_POPULATION_SIZE",
#       "SEX_WORKERS_POPULATION_SIZE",
#       "PWID_POPULATION_SIZE",
#       "TG_POPULATION_SIZE"
#       # PRISONERS_POPULATION_SIZE excluded — see note above
#     ),
#     !is.na(Data.value),
#     Data.value != "",
#     as.integer(Time.Period) >= MIN_YEAR
#   ) %>%
#   mutate(
#     value = as.numeric(Data.value),
#     year  = as.integer(Time.Period),
#     kp_type = case_when(
#       Indicator_GId == "MSM_POPULATION_SIZE"         ~ "msm",
#       Indicator_GId == "SEX_WORKERS_POPULATION_SIZE" ~ "fsw",
#       Indicator_GId == "PWID_POPULATION_SIZE"        ~ "pwid",
#       Indicator_GId == "TG_POPULATION_SIZE"          ~ "tg"
#     ),
#     country     = Area,
#     country_iso = Area.ID
#   ) %>%
#   filter(!is.na(value), value > 0) %>%
#   select(country, country_iso, kp_type, year, value)
# 
# 
# # ============================================================================
# # STEP 2: TAKE MOST RECENT YEAR PER COUNTRY × KP TYPE
# # ============================================================================
# 
# kp_latest <- kp_estimates %>%
#   group_by(country, country_iso, kp_type) %>%
#   slice_max(order_by = year, n = 1, with_ties = FALSE) %>%
#   ungroup()
# 
# # ============================================================================
# # STEP 3: SUM ACROSS KP TYPES PER COUNTRY
# # ============================================================================
# # Note on overlap: MSM and FSW estimates can overlap (MSM who sell sex).
# # There is no standard correction for this in country-level data.
# # We sum directly — this likely leads to slight overestimation (~5-10%).
# # For a conservative estimate, set PARTNER_MULTIPLIER = 1.5.
# 
# kp_total <- kp_latest %>%
#   group_by(country, country_iso) %>%
#   summarise(
#     kp_sum          = sum(value, na.rm = TRUE),
#     kp_types_present = paste(sort(kp_type), collapse = "+"),
#     n_kp_types      = n_distinct(kp_type),
#     years_used      = paste(sort(unique(year)), collapse = "/"),
#     .groups = "drop"
#   )
# 
# 
# # ============================================================================
# # STEP 4: LOAD COUNTRY CSV AND DERIVE SEXUALLY ACTIVE ADULT DENOMINATOR
# # ============================================================================
# 
# # --- Try loading your country CSV ---
# # If it doesn't exist yet, we fall back to a warning and use the default
# if (file.exists(COUNTRY_CSV)) {
#   country_data <- read_csv(COUNTRY_CSV, show_col_types = FALSE)
#   
#   # Derive sexually active adults = total pop × (1 - prop_under14/100) × 0.75
#   # (roughly: adults × proportion who are sexually active)
#   # Adjust the 0.75 if your context uses a different sexually-active fraction
#   country_data <- country_data %>%
#     mutate(
#       adult_pop           = total_population * (1 - prop_under14 / 100),
#       sexually_active_pop = adult_pop * 0.75
#     ) %>%
#     select(country, total_population, adult_pop, sexually_active_pop)
#   
# } else {
#   warning(
#     "Country CSV not found at '", COUNTRY_CSV, "'.\n",
#     "Denominator will be estimated as total_population × ", DEFAULT_PROP_SEXUALLY_ACTIVE,
#     " using KP Atlas area data. Accuracy will be lower.\n",
#     "Set COUNTRY_CSV to your TIER country data file for best results."
#   )
#   
#   # Fallback: use KP Atlas country names; user must supply population later
#   country_data <- kp_total %>%
#     select(country, country_iso) %>%
#     mutate(
#       total_population    = NA_real_,
#       adult_pop           = NA_real_,
#       sexually_active_pop = NA_real_
#     )
# }
# 
# # ============================================================================
# # STEP 5: CALCULATE prop_high_risk
# # ============================================================================
# 
# prop_hr <- kp_total %>%
#   left_join(country_data, by = "country") %>%
#   mutate(
#     # KP + their partners (the high-risk network)
#     high_risk_network      = kp_sum      * PARTNER_MULTIPLIER,
#     
#     # prop_high_risk = high_risk_network / sexually active adults
#     prop_high_risk      = high_risk_network      / sexually_active_pop,
#   
#     # Clip to [0, 0.30] — anything above 30% implies data or assumption problem
#     prop_high_risk      = pmin(pmax(prop_high_risk,      0, na.rm = TRUE), 0.30),
#    
#     # Flag countries where estimate looks suspicious
#     flag = case_when(
#       is.na(prop_high_risk)         ~ "missing_pop_denominator",
#       prop_high_risk > 0.20         ~ "unusually_high_check_inputs",
#       prop_high_risk < 0.01         ~ "unusually_low_check_kp_data",
#       n_kp_types < 2                ~ "only_one_kp_type_available",
#       TRUE                          ~ "ok"
#     )
#   ) %>%
#   select(
#     country, country_iso,
#     kp_sum, kp_types_present, n_kp_types, years_used,
#     high_risk_network,
#     sexually_active_pop,
#     prop_high_risk,
#     flag
#   ) %>%
#   arrange(country) %>% filter(country %in%sub_countries)
# 
# # ============================================================================
# # STEP 6: PRINT SUMMARY AND SAVE
# # ============================================================================
# 
# cat("\n=== prop_high_risk estimation complete ===\n")
# cat(sprintf("Countries with estimates:  %d\n", sum(!is.na(prop_hr$prop_high_risk))))
# cat(sprintf("Countries flagged:         %d\n", sum(prop_hr$flag != "ok", na.rm = TRUE)))
# cat(sprintf("Partner multiplier used:   %.1f×\n", PARTNER_MULTIPLIER))
# cat(sprintf("Minimum year filter:       %d\n\n", MIN_YEAR))
# 
# # Print distribution
# quantiles <- quantile(prop_hr$prop_high_risk, probs = c(0.1, 0.25, 0.5, 0.75, 0.9), na.rm = TRUE)
# cat("Distribution of prop_high_risk:\n")
# print(round(quantiles, 3))
# 
# # Print flagged countries
# flagged <- prop_hr %>% filter(flag != "ok")
# if (nrow(flagged) > 0) {
#   cat("\nFlagged countries:\n")
#   print(flagged %>% select(country, prop_high_risk, n_kp_types, flag))
# }
# 
# # # Save output
# # write_csv(prop_hr, "prop_high_risk_by_country.csv")
# # cat("\nSaved: prop_high_risk_by_country.csv\n")
# 
# # ============================================================================
# # STEP 7: SENSITIVITY CHECK — compare multipliers 1.5, 2.0, 3.0
# # ============================================================================
# 
# sensitivity <- kp_total %>%
#   left_join(country_data %>% select(country, sexually_active_pop), by = "country") %>%
#   mutate(
#     prop_hr_low_mult  = pmin((kp_sum * 1.5) / sexually_active_pop, 0.30),
#     prop_hr_mid_mult  = pmin((kp_sum * 2.0) / sexually_active_pop, 0.30),
#     prop_hr_high_mult = pmin((kp_sum * 3.0) / sexually_active_pop, 0.30)
#   ) %>%
#   select(country, country_iso, prop_hr_low_mult, prop_hr_mid_mult, prop_hr_high_mult) %>% filter(country %in%sub_countries)
# 
# #write_csv(sensitivity, "prop_high_risk_sensitivity.csv")
# cat("Saved: prop_high_risk_sensitivity.csv\n\n")
# 
# 
# 
# ######basic data
