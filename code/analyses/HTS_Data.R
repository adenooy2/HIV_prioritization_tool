library(tidyverse)
library(readxl)
options(scipen=999)
rm(list=ls())
library(WDI)

pop_data= WDI(indicator =  c( "SP.POP.TOTL","SP.POP.TOTL.MA.ZS","SP.POP.0014.TO.ZS"),start=2024,end=2024) 
pop_data=pop_data %>% select(Area=country,Area.ID=iso3c,population=SP.POP.TOTL,prop_under14=SP.POP.0014.TO.ZS)
pop_data$adult_pop=pop_data$population*(1-pop_data$prop_under14/100)
pop_data=pop_data %>% select(Area,Area.ID,population,adult_pop)

dir="/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/data/who_hts_dashboard/"
files=list.files(path = dir)

all_data=data.frame()
length(files)
for(i in 1: length(files)){
data=read.csv(paste(dir,files[i],sep=""))
cntry=files[i]
cntry=gsub("WHO HTS Data - ","",cntry)
cntry=gsub(".csv","",cntry)
data$Area.ID=cntry

all_data=rbind(all_data,data)

}


all_data=all_data %>% filter(year>=2023) %>%  group_by(Area.ID, chart,indicator,sex,age) %>% mutate(max_year=max(year)) %>% filter(year==max_year)
all_data$label=paste(all_data$chart,all_data$indicator,sep="&")

label=c("HIV tests conducted and positivity, by sex&Den Age-All","HIV tests conducted and positivity, by sex&Per Age-All",
                     "HIV tests conducted and positivity at community level&Den Community-Community All","HIV tests conducted and positivity at community level&Per Community-Community All",
        "HIV tests conducted and positivity at facility level&Den Facility-Facility All","HIV tests conducted and positivity at facility level&Per Facility-Facility All",
        "HIV tests conducted and positivity for provider-assisted referral / index testing&Total Index tests",
        "HIV tests conducted and positivity for provider-assisted referral / index testing&Positivity - Index Testing Total")

variable=c("total","total","community","community","facility","facility","index","index")
type=c("tests","positivity","tests","positivity","tests","positivity","tests","positivity")

var_list=data.frame(label,variable,type)

all_data_sub=left_join(var_list,all_data)

all_data_sub_simple=all_data_sub %>% select(Area.ID,variable,type,year,value)%>% left_join(pop_data)


all_data_sub_tests=all_data_sub_simple %>% filter(type=="tests")
all_data_sub_tests$prop_tests=all_data_sub_tests$value/all_data_sub_tests$adult_pop

sub_countries=c("Botswana","Côte d'Ivoire","Eswatini","Ghana","Kenya","Lesotho","Malawi","Mozambique","Nigeria","Congo","South Africa","South Sudan","United Republic of Tanzania","Uganda","Zambia","Zimbabwe")

all_data_sub_tests=all_data_sub_tests %>% filter(Area %in% sub_countries)
