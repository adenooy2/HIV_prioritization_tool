data=read_excel("/Users/adenooy/Library/CloudStorage/OneDrive-Personal/AMC/HIV Prioritization tool/HIV_prioritization_tool/Submissions/CQUINN/DSD and Annual .xlsx")

data=data %>% filter(Country!="Ghana") %>% 
  filter(Scenario %in%c("Scale_up_all","Baseline_annual","Scale_up_all_annual")) %>% mutate(cost_savings=-1*`Cost (difference from baseline)`) %>% 
  select(Country,Scenario,cost_savings)


ggplot(data,aes(Country, cost_savings,fill=Scenario))+geom_bar(position="dodge",stat="identity")+  scale_y_continuous(labels = scales::label_number())+
  ylab("Cost savings ($)")+
  scale_fill_manual(values = c(
    "#e0021c",  # Blue
    "#013b4f",  # Orange
    "#0070c0"   # Green
  ),
  labels = c(
    "Baseline_annual" = "Annual Clinic Visits",
    "Scale_up_all" = "All 6MMD",
    "Scale_up_all_annual" = "Annual Clinic Visits + All 6MMD"
  ))+
  scale_y_continuous(
    labels = scales::label_number(scale = 1e-6, suffix = "M")
  )
 