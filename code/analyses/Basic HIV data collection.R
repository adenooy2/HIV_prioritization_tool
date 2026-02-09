library(tidyverse)
library(readxl)
options(scipen=999)
rm(list=ls())
library(WDI)

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
                    "COVERAGE_DSD_ART_MODELS Adults (15+)")) %>% 
group_by(Indicator,Indicator_GId,Subgroup,Area) %>%  
  arrange(Area,Indicator,Indicator_GId,Subgroup,desc(year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  unique() 


gam_diagnoses=gam_data %>% select(Area,Area.ID,Indicator,Indicator_GId,Subgroup,label,year=Time.Period,Data.value) %>%
filter(label%in%c("HIV_TESTS_VOL Total","HIV_POS_RATE Total")) %>% group_by(Indicator,Indicator_GId,Subgroup,Area) %>%  
  arrange(Indicator,Indicator_GId,Subgroup,Area,desc(year))  %>% slice(1) %>% 
  ungroup() %>% 
  unique() %>% 
  select(Area,Area.ID,label,year,Data.value) %>% spread(label,Data.value) %>% mutate(diagnoses=`HIV_POS_RATE Total`*`HIV_TESTS_VOL Total`/100)

######worldbank data

pop_data= WDI(indicator =  c( "SP.POP.TOTL","SP.POP.TOTL.MA.ZS","SP.POP.0014.TO.ZS"),start=2024) %>% select(-year)
pop_data= pop_data %>% left_join(WDI(indicator =  c( "SP.DYN.CBRT.IN"),start=2023,end = 2023))
pop_data=pop_data %>% select(Area=country,Area.ID=iso3c,population=SP.POP.TOTL,prop_male=SP.POP.TOTL.MA.ZS,prop_under14=SP.POP.0014.TO.ZS,birth_rate=SP.DYN.CBRT.IN)

inds=WDIsearch("pop")
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

basic_data$diagnoses=(basic_data$PLHIV_KNOWLEDGE_OF_STATUS/100)*basic_data$NEW_INFECTIONS. ##UPDATE

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


sub_countries=c("Eswatini","Lesotho","Botswana","Mozambique","Zimbabwe","Zambia","Namibia","Malawi","South Africa","Uganda","Kenya","Central African Republic")

basic_data=basic_data %>% filter(country%in%sub_countries)

#####Fix diagnoses

write.csv(basic_data,"/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/basic_hiv_data.csv")


