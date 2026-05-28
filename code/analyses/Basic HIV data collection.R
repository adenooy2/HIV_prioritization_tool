library(tidyverse)
library(readxl)
options(scipen=999)
rm(list=ls())
library(WDI)

prop_high_risk=0.05
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

est_data_AHD=est_data_filt %>% filter(label=="LATE_HIV_DIAGNOSIS_All ages CD4 <200") %>% select(Area,Time.Period, prop_ahd=Data.value)

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


country_codes=gam_data %>% select(Area,Area.ID) %>% unique()

######worldbank data

pop_data= WDI(indicator =  c( "SP.POP.TOTL","SP.POP.TOTL.MA.ZS","SP.POP.0014.TO.ZS"),start=2024) %>% select(-year)
pop_data= pop_data %>% left_join(WDI(indicator =  c( "SP.DYN.CBRT.IN"),start=2023,end = 2023))
pop_data=pop_data %>% select(Area=country,Area.ID=iso3c,population=SP.POP.TOTL,prop_male=SP.POP.TOTL.MA.ZS,prop_under14=SP.POP.0014.TO.ZS,birth_rate=SP.DYN.CBRT.IN)

#inds=WDIsearch("pop")
###BAsic data

basic_data=est_data %>% 
  filter(label %in% c("PLWH_All ages estimate","PLHIV_KNOWLEDGE_OF_STATUS_All ages estimate","PERCENT_KNOW_STATUS_ON_ART_All ages estimate",
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
         current_diagnoses=  `diagnoses`,plhiv=PLWH, percent_on_art=`PERCENT_KNOW_STATUS_ON_ART`,percent_suppressed=`PERCENT_ON_ART_VL_SUPPRESSED`,percent_diagnosed=PLHIV_KNOWLEDGE_OF_STATUS,
         aids_deaths_per_year=`AIDS_DEATHS`) %>% 
  select("country", "total_population", "hiv_prevalence", 
         "new_infections_per_year", "current_diagnoses", "percent_diagnosed","plhiv",
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
kp_wide=kp_wide %>% left_join(basic_data %>% select(country,total_population,prop_under14))
kp_wide$adult_pop=kp_wide$total_population*(1-kp_wide$prop_under14/100)
kp_wide$high_risk_pop=2*kp_wide$kp_pop_estimate #Assumes each KP indiviudal has a partner as part of teh network
kp_wide$pop_sexually_active=kp_wide$adult_pop*0.85 ##UPDATE
kp_wide$prop_high_risk=kp_wide$high_risk_pop/kp_wide$pop_sexually_active

#kp_wide$prop_high_risk=0.03 #1% MSM, 1.2% FSW 

# 
kp_wide_final=kp_wide %>% ungroup() %>% select(country,prop_high_risk)
kp_wide_final$rr_high=8.6 #(11.5 #https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(24)00270-5/fulltext)
basic_data=left_join(basic_data, kp_wide_final)


basic_data$prop_high_risk=prop_high_risk
########testing data (avg volume and positiivity)
test_data=gam_data %>% filter(Indicator_GId %in% c("HIV_TESTS_VOL","HIV_POS_RATE")) %>% filter(Subgroup=="Total") %>% select(Area,Indicator_GId,Time.Period,Data.value) %>% 
  arrange(Area,Indicator_GId,desc(Time.Period)) %>% group_by(Area,Indicator_GId) %>% slice(1) %>% spread(Indicator_GId,Data.value)

test_data=test_data %>% select(country=Area,avg_test_yield = HIV_POS_RATE,total_tests_prev_year=HIV_TESTS_VOL) %>% filter(country %in% sub_countries)

basic_data=left_join(basic_data, test_data)

####REtest props

dir="/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/who_hts_dashboard/"
files=list.files(path = dir)

hts_data=data.frame()
length(files)
for(i in 1: length(files)){
  data=read.csv(paste(dir,files[i],sep=""))
  cntry=files[i]
  cntry=gsub("WHO HTS Data - ","",cntry)
  cntry=gsub(".csv","",cntry)
  data$Area.ID=cntry
  
  hts_data=rbind(hts_data,data)
  
}


hts_data=hts_data %>% filter(year>=2023) %>%  group_by(Area.ID, chart,indicator,sex,age) %>% mutate(max_year=max(year)) %>% filter(year==max_year)
hts_data$label=paste(hts_data$chart,hts_data$indicator,sep="&")


retest_data=hts_data %>% ungroup() %>% filter(chart=="HIV-positive tests - new diagnoses and retests") %>% 
  select(Area.ID,indicator,value) %>% spread(indicator,value) %>% 
  mutate(total=retests+new_diagnoses,prop_retest=retests/total) %>% left_join(country_codes) %>% 
  select(country=Area,prop_retest)


basic_data=left_join(basic_data,retest_data)

### ANC multipliers

anc_mults=read.csv("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/ANC multiplier.csv")
colnames(anc_mults)[1]="country"

basic_data=left_join(basic_data,anc_mults %>% select(country,ANC_multiplier))

######writes data

sub_countries2=c("Botswana","Côte d'Ivoire","Eswatini","Ghana","Kenya","Lesotho","Malawi","Mozambique","Nigeria","South Africa","United Republic of Tanzania","Uganda","Zambia","Zimbabwe")

basic_data=basic_data %>% filter(country%in%sub_countries2)


write.csv(basic_data,"/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/basic_hiv_data.csv")




