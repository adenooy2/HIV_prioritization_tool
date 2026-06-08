rm(list=ls())

library(readr)
library(stringr)
grouped_mods=read_excel("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/pepfar_modalities.xlsx")

#AMFAR?PEPFAR https://data.pepfar.gov/datasets
data <- read_delim("/Users/adenooy/Downloads/Enhanced_Geographical_Analysis.zip", 
                                             delim = "\t", escape_double = FALSE, 
                                             trim_ws = TRUE)

data$Country=gsub("Tanzania","United Republic of Tanzania",data$Country)
data$Country=gsub("Cote d'Ivoire","Côte d'Ivoire",data$Country)


data=left_join(data,grouped_mods)




temp=data %>% filter(Indicator %in% c("HTS_TST","HTS_TST_POS"))
temp=temp %>% filter(is.na(tier_testing)==FALSE, tier_testing!="other")
sub_countries=c("Botswana","Côte d'Ivoire","Eswatini","Ghana","Kenya","Lesotho","Malawi","Mozambique","Nigeria","Congo","South Africa","South Sudan","United Republic of Tanzania","Uganda","Zambia","Zimbabwe")

temp=temp %>% filter(Country%in%sub_countries)


df_long_totals <- temp |>
  select(Country, Indicator, matches("Quarter \\d Results")) |>
  pivot_longer(
    cols = matches("Quarter \\d Results"),
    names_to = "col",
    values_to = "value"
  ) |>
  mutate(
    year = as.integer(str_extract(col, "\\d{4}")),
    value = as.numeric(value)
  ) |>
  group_by(Country, Indicator,year) |>
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>% spread(Indicator, total) %>% mutate(pos=HTS_TST_POS/HTS_TST*100) %>% 
  rename(test_pos=HTS_TST_POS,test_total=HTS_TST) 

#write.csv(df_long_total,"/Users/adenooy/Downloads/")

coeff <- 0.1

ggplot(df_long_totals %>% filter(year>2016), aes(x=year)) +
  geom_line( aes(y=test_total,color="Volume")) + geom_point(aes(y=test_total,color="Volume"))+
  geom_line( aes(y=test_pos / coeff,color="Positive")) + geom_point(aes(y=test_pos / coeff,color="Positive"))+# Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "HIV Tests",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Positive")
  )+facet_wrap(.~Country,scales="free_y")+theme_bw()

coeff <- 0.00001
ggplot(df_long_totals %>% filter(year>2016), aes(x=year)) +
  geom_line( aes(y=test_total,color="Volume")) + geom_point(aes(y=test_total,color="Volume"))+
  geom_line( aes(y=pos / coeff,color="Positivity rate")) + geom_point(aes(y=pos / coeff,color="Positivity rate"))+# Divide by 10 to get the same range than the temperature
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "HIV Tests",
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="Positivity rate")
  )+facet_wrap(.~Country,scales="free_y")+theme_bw()



df_long <- temp |>
  select(Country, Indicator,tier_testing, matches("Quarter \\d Results")) |>
  pivot_longer(
    cols = matches("Quarter \\d Results"),
    names_to = "col",
    values_to = "value"
  ) |>
  mutate(
    year = as.integer(str_extract(col, "\\d{4}")),
    value = as.numeric(value)
  ) |>
  group_by(Country, Indicator, tier_testing,year) |>
  summarise(total_mod = sum(value, na.rm = TRUE), .groups = "drop") %>% spread(Indicator,total_mod) %>% 
  mutate(pos_mod=HTS_TST_POS/HTS_TST*100) %>% rename(test_pos_mod=HTS_TST_POS,test_total_mod=HTS_TST)



df_combined=left_join(df_long,df_long_totals)
df_combined$test_prop=df_combined$test_total_mod/df_combined$test_total
df_combined$multiplier=df_combined$pos_mod/df_combined$pos

props_summary=df_combined %>%filter(year>2022) %>%  group_by(Country,tier_testing) %>% summarise(mean_prop=mean(test_prop)) %>% 
  filter(is.na(tier_testing)==FALSE) %>%spread(tier_testing,mean_prop)



multiplier_summary=df_combined%>%filter(year>2022)  %>% group_by(Country,tier_testing) %>% filter(is.nan(multiplier)==FALSE)%>% 
  summarise(country_mult=mean(multiplier)) 

multiplier_summary$tier_testing=paste("yield_mult_",multiplier_summary$tier_testing,sep="")


multiplier_summary_avg=multiplier_summary %>% group_by(tier_testing) %>% summarise(avg_mult=mean(country_mult,na.rm=TRUE))


multiplier_summary=multiplier_summary%>% spread(tier_testing,country_mult)


avg_lookup <- setNames(multiplier_summary_avg$avg_mult, multiplier_summary_avg$tier_testing)

multiplier_summary_final <- multiplier_summary %>%
  mutate(across(
    all_of(names(avg_lookup)),
    ~ ifelse(is.na(.), avg_lookup[cur_column()], .)
  ))

multiplier_summary_final=multiplier_summary_final %>% rename(country=Country)

multiplier_summary_final$yield_mult_hivst_community=multiplier_summary_final$yield_mult_test_community
multiplier_summary_final$yield_mult_hivst_facility=multiplier_summary_final$yield_mult_test_facility_general


##Available baseline data
baseline_data=read_excel("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/country_baselines/Updated/country_baselines_summarised.xlsx")

#baseline_data$country[baseline_data$country=="Mozambique"]="Moz"


baseline_data=left_join(multiplier_summary_final,baseline_data)
#######baseline testing numbers
data_dir="/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/"
gam_data=read.csv(paste(data_dir,"GAM_2025_en.csv",sep=""))
gam_data$label=paste(gam_data$Indicator_GId,gam_data$Subgroup)

test_data=gam_data %>% filter(Indicator_GId %in% c("HIV_TESTS_VOL","HIV_POS_RATE")) %>% filter(Subgroup=="Total") %>% select(Area,Indicator_GId,Time.Period,Data.value) %>% 
  arrange(Area,Indicator_GId,desc(Time.Period)) %>% group_by(Area,Indicator_GId) %>% slice(1) %>% spread(Indicator_GId,Data.value)

test_data=test_data %>% select(Country=Area,avg_test_yield = HIV_POS_RATE,total_tests_prev_year=HIV_TESTS_VOL) %>% filter(Country %in% sub_countries)

test_data_props=test_data %>% left_join(props_summary) %>% gather("modality","prop",4:10) %>% 
  mutate(baseline_tests=round(total_tests_prev_year*prop,-3)) %>% select(-prop) %>% spread(modality,baseline_tests)

test_data_props=test_data_props %>% rename(country=Country)

#baseline_tetsing=left_join(test_data_props,multiplier_summary_final)

baseline_data=rows_patch(baseline_data, test_data_props %>% select(-avg_test_yield,-total_tests_prev_year,-anc_hiv_testing,-pnc_hiv_testing), by = "country")
baseline_data=left_join(baseline_data,test_data_props %>% select(avg_test_yield,total_tests_prev_year))

##HIVST numbers
hivst_com_prop=0.65
hivst_data=gam_data %>% filter(label=="HIV_SELF_TESTS Distributed")%>% select(Area,Indicator_GId,Time.Period,Data.value) %>% 
  arrange(Area,Indicator_GId,desc(Time.Period)) %>% group_by(Area,Indicator_GId) %>% slice(1) %>% spread(Indicator_GId,Data.value) %>% 
  mutate(hivst_community=round(hivst_com_prop*HIV_SELF_TESTS,-3),hivst_facility=round((1-hivst_com_prop)*HIV_SELF_TESTS,-3)) %>% 
  select(country=Area,hivst_community,hivst_facility)



hivst_data=hivst_data %>% filter(country %in%sub_countries)

baseline_data=rows_patch(baseline_data, hivst_data)
baseline_data$hivst_community[is.na(baseline_data$hivst_community)==TRUE]=0
baseline_data$hivst_facility[is.na(baseline_data$hivst_facility)==TRUE]=0

##EID
#https://data.unicef.org/topic/hivaids/paediatric-treatment-and-care/
eid_data=read_excel(paste(data_dir,"EID_PMTCT_UNICEF_data.xlsx",sep=""),sheet="eid_coverage")

eid_data=eid_data %>% select(country=Country,eid=Value) %>% mutate(eid=as.numeric(eid))

baseline_data=rows_patch(baseline_data, eid_data,unmatched = "ignore")

av_eid=mean(baseline_data$eid,na.rm = TRUE)
baseline_data$eid[is.na(baseline_data$eid)==TRUE]=av_eid



# GAM DATA BASELINE - PREVNTION -------------------------------------------
#"PEOPLE_ON_PREP Total",

#Condoms
gam_data_condoms=gam_data %>% select(Area,Area.ID,Indicator,Indicator_GId,Subgroup,label,year=Time.Period,Data.value) %>% 
  filter(label%in%c(
                    "CONDOMS_DISTRIBUTED Male condoms Total","CONDOMS_DISTRIBUTED Female condoms Total")) %>% 
  group_by(Indicator,Indicator_GId,Subgroup,Area) %>%  
  arrange(Area,Indicator,Indicator_GId,Subgroup,desc(year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  unique() %>% select(Area,Area.ID,Subgroup,year,Data.value) %>% spread(Subgroup,Data.value)

gam_data_condoms$`Female condoms Total`[is.na(gam_data_condoms$`Female condoms Total`)==TRUE]=0
gam_data_condoms$`Male condoms Total`[is.na(gam_data_condoms$`Male condoms Total`)==TRUE]=0

gam_data_condoms$condoms=round(gam_data_condoms$`Female condoms Total`+gam_data_condoms$`Male condoms Total`,-3)

gam_data_condoms=gam_data_condoms %>% select(country=Area,year,condoms) %>% group_by(country) %>% arrange(country, desc(year)) %>% slice(1) %>% 
  select(country,condoms)

#Prep
gam_data_prep=gam_data %>% select(Area,Area.ID,Indicator,Indicator_GId,Subgroup,label,year=Time.Period,Data.value) %>% 
  filter(label%in%c( "PEOPLE_ON_PREP Total")) %>% 
  group_by(Indicator,Indicator_GId,Subgroup,Area) %>%  
  arrange(Area,Indicator,Indicator_GId,Subgroup,desc(year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  unique() %>% select(Area,Area.ID,Subgroup,year,Data.value) %>% spread(Subgroup,Data.value) %>% select(country=Area, prep_oral=Total) %>% 
  mutate(prep_oral=round(prep_oral,-2))

gam_data_prep$prep_lenacapavir=0

gam_data_prev=full_join(gam_data_condoms,gam_data_prep)  
gam_data_prev$prep_oral[is.na(gam_data_prev$prep_oral)==TRUE]=0
gam_data_prev$prep_lenacapavir[is.na(gam_data_prev$prep_lenacapavir)==TRUE]=0

##VMMC

gam_data_vmmc=gam_data %>% select(Area,Area.ID,Indicator,Indicator_GId,Subgroup,label,year=Time.Period,Data.value) %>% 
  filter(label%in%c( "MALE_CIRCUMCISIONS_PERFORMED All ages")) %>% 
  group_by(Indicator,Indicator_GId,Subgroup,Area) %>%  
  arrange(Area,Indicator,Indicator_GId,Subgroup,desc(year)) %>% 
  slice(1) %>% 
  ungroup() %>% 
  unique() %>% select(Area,Area.ID,Subgroup,year,Data.value) %>% spread(Subgroup,Data.value) %>% select(country=Area,vmmc='All ages') %>% 
  mutate(vmmc=round(vmmc,-2))
  
gam_data_prev=full_join(gam_data_prev,gam_data_vmmc)
gam_data_prev$vmmc[is.na(gam_data_prev$vmmc)==TRUE]=0


#baseline_data=left_join(baseline_tetsing,gam_data_prev)

baseline_data=rows_patch(baseline_data, gam_data_prev,unmatched = "ignore")

# IeDea Data --------------------------------------------------------------
Iedea_data=read.csv(paste(data_dir,"iedea_indicators/indicator_enroll.csv",sep=""))
reg=unique(Iedea_data$Region)

Region=data.frame(reg,c("Asia-Pacific","Central Africa","CCASAnet (Latin America)","East Africa", "NA-ACCORD (North America)","Southern Africa","West Africa"))
colnames(Region)=c("Region","reg_name")

# cd4_data=left_join(cd4_data,Region)
# 
# cd4_data_filt=cd4_data %>% filter(Year!="ALL",Sex=="ALL",Age=="ALL")%>%
#   mutate(Year=as.numeric(Year),cd4_enroll_perc=as.numeric(cd4_enroll_perc)) %>% select(Region, reg_name,Year,cd4_enroll_perc) %>%
#   filter(is.na(cd4_enroll_perc)==FALSE)%>% group_by(Region) %>% arrange(desc(Year)) %>% slice(1)
# 
# enroll_data_latest$ahd=round(enroll_data_latest$cd4_enr_0_200/enroll_data_latest$cd4_enroll_n,2)


##IEDEA 2022 website
cd4_data=data.frame(Region, cd4_testing=c(83,21,58,41,NA,25,26))


sub_reg_countries=data.frame(country=sub_countries, 
                             Region=c("SA","WA","SA","WA","EA","SA","SA","SA","WA","CA","SA","EA","EA","EA","SA","SA"))



temp=left_join(baseline_data %>% select(country),sub_reg_countries)
temp=left_join(temp,cd4_data,by="Region")

baseline_data=rows_patch(baseline_data, temp %>% select(country,cd4_testing),unmatched = "ignore")
##############DSD Assumptions -find

baseline_data$mmd_3month[is.na(baseline_data$mmd_3month)==TRUE]=30
baseline_data$mmd_6month[is.na(baseline_data$mmd_6month)==TRUE]=60
baseline_data$mmd_12month[is.na(baseline_data$mmd_12month)==TRUE]=0
baseline_data$community_pickup[is.na(baseline_data$community_pickup)==TRUE]=5


######### FIND
baseline_data$tracking_tracing[is.na(baseline_data$tracking_tracing)==TRUE]=70
baseline_data$adherence_counseling[is.na(baseline_data$adherence_counseling)]=50

baseline_data$anc_hiv_testing[is.na(baseline_data$anc_hiv_testing)==TRUE]=70
baseline_data$pnc_hiv_testing[is.na(baseline_data$pnc_hiv_testing)==TRUE]=40

baseline_data$anc_vl_testing[is.na(baseline_data$anc_vl_testing)==TRUE]=70
baseline_data$pnc_vl_testing[is.na(baseline_data$pnc_vl_testing)==TRUE]=20

#baseline_data$pep[is.na(baseline_data$pep)==TRUE]=100000
baseline_data$infant_prophylaxis[is.na(baseline_data$infant_prophylaxis)==TRUE]=90

baseline_data$vl_monitoring_routine[is.na(baseline_data$vl_monitoring_routine)==TRUE]=70

baseline_data$ahd_package[is.na(baseline_data$ahd_package)==TRUE]=80

######
write.csv(baseline_data,"/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/baseline_testing.csv")

#temp=gam_data %>% select(Indicator,Indicator_GId) %>% unique()
