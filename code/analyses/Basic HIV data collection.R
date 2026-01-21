library(tidyverse)
library(readxl)
options(scipen=999)
rm(list=ls())

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



gam_data=read.csv(paste(data_dir,"GAM_2025_en.csv",sep=""))
gam_indicators=read_excel(paste(data_dir,"data_list.xlsx",sep=""),sheet="GAM")

pop_data=read.csv(paste(data_dir,"pop_data_worldBank.csv",sep=""))

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

basic_data$diagnoses=100000

basic_data=basic_data%>% 
  rename(country=Area.x,total_population=population,hiv_prevalence=`HIV_PREVALENCE`,new_infections_per_year=`NEW_INFECTIONS`,
         current_diagnoses=  `diagnoses`,percent_on_art=`PERCENT_KNOW_STATUS_ON_ART`,percent_suppressed=`PERCENT_ON_ART_VL_SUPPRESSED`,percent_diagnosed=PLHIV_KNOWLEDGE_OF_STATUS,
         aids_deaths_per_year=`AIDS_DEATHS`) %>% 
  select("country", "total_population", "hiv_prevalence", 
         "new_infections_per_year", "current_diagnoses", "percent_diagnosed",
         "percent_on_art", "percent_suppressed", "aids_deaths_per_year")

basic_data$hiv_prevalence=round(basic_data$hiv_prevalence,1)
basic_data$new_infections_per_year=round(basic_data$new_infections_per_year,0)
basic_data$current_diagnoses=round(basic_data$current_diagnoses,0)
basic_data$percent_suppressed=round(basic_data$percent_suppressed,1)
basic_data$percent_on_art=round(basic_data$percent_on_art,1)
basic_data$aids_deaths_per_year=round(basic_data$aids_deaths_per_year,0)
basic_data$percent_diagnosed=round(basic_data$percent_diagnosed,1)


sub_countries=c("Eswatini","Lesotho","Botswana","Mozambique","Zimbabwe","Zambia","Namibia","Malawi","Uganda","Kenya")

basic_data=basic_data %>% filter(country%in%sub_countries)

#####Fix diagnoses

write.csv(basic_data,"/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/basic_hiv_data.csv")


