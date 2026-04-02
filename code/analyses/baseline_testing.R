rm(list=ls())

library(readr)
grouped_mods=read_excel("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/pepfar_modalities.xlsx")

data <- read_delim("Downloads/Enhanced_Geographical_Analysis.zip", 
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



multiplier_summary=df_combined%>%filter(year>2022)  %>% group_by(Country,tier_testing) %>% filter(is.nan(multiplier)==FALSE) %>% 
  summarise(mean_mult=mean(multiplier)) %>% spread(tier_testing,mean_mult)

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


write.csv(test_data_props,"/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/tier_app/baseline_testing.csv")

