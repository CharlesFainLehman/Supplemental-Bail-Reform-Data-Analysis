require(tidyverse)
theme_set(theme_minimal())

#don't forget to download the data
#https://www.criminaljustice.ny.gov/crimnet/ojsa/stats.htm
supp <- read.csv("Supplemental_Pretrial_Release_Data.csv") %>%
  filter(larg_yr >= 2019, #I use the lower, rather than upper, court arraignment year because the former is available in 99% of cases, while the latter is available in only 13%
         !larg_rel_decision %in% c("", "Disposed at arraign", "Unknown")) %>% #drop those without an outcome or disposed at arraignment) 
  mutate(rearrested_180 = ifelse(rearr_vfo_180 == "Yes" | rearr_nonvfo_180 == "Yes" | rearr_misd_180 == "Yes" | rearr_fire_180 == "Yes", "Yes", "No"), #combined rearrested in 180 days flag
         rearrested_ever = ifelse(rearr_vfo == "Yes" | rearr_nonvfo == "Yes" | rearr_misd == "Yes" | rearr_fire == "Yes", "Yes", "No")) #combined ever rearrested flag
  
#table 1
supp %>%
  group_by(larg_yr, larg_rel_decision) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  pivot_wider(id_cols = larg_yr, names_from = larg_rel_decision, values_from = p)

#table 2 is a composite of data from the following four analyses

#Rearrested within 180 days by release type
supp %>%
  filter(!(larg_yr == 2021 & larg_mo %in% 10:12)) %>% #drop the last 3 months of 2021
  group_by(larg_yr, larg_rel_decision, rearrested_180) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  filter(rearrested_180 == "Yes") %>%
  pivot_wider(id_cols = larg_yr, names_from = larg_rel_decision, values_from = p)

#Overall rearrest rates within 180 days
supp %>%
  filter(!(larg_yr == 2021 & larg_mo %in% 10:12)) %>% #drop the last 3 months of 2021
  group_by(larg_yr, rearrested_180) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  filter(rearrested_180 == "Yes")

#ever rearrested by release type
supp %>%
  filter(!(larg_yr == 2021 & larg_mo %in% 10:12)) %>% #drop the last 3 months of 2021
  group_by(larg_yr, larg_rel_decision, rearrested_ever) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  filter(rearrested_ever == "Yes") %>%
  pivot_wider(id_cols = larg_yr, names_from = larg_rel_decision, values_from = p)

#Overall ever rearrested rate
supp %>%
  filter(!(larg_yr == 2021 & larg_mo %in% 10:12)) %>% #drop the last 3 months of 2021
  group_by(larg_yr, rearrested_ever) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  filter(rearrested_ever == "Yes")

#Figure 1

supp %>%
  filter(!(larg_yr == 2021 & larg_mo %in% 10:12)) %>% #drop the last 3 months of 2021
  group_by(larg_yr, larg_mo, rearrested_180) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  filter(rearrested_180 == "Yes") %>%
  mutate(date = as.Date(paste(larg_yr, larg_mo, "01", sep = "-"))) %>%
  ggplot(aes(x=date, y = p)) + 
  geom_line() + 
  geom_point() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b '%y") +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Monthly Rearrest Rate", subtitle = "2019 - Sept. 2021", x = "Arraignment Year and Month", y = "6-mo. Rearrest Rate", caption = "Source: DCJS")

ggsave("plot1.png", width = 8, height = 5, bg = 'white')

#table 3 is a composite of the two below

#rearrest within 180 days, by region
#note that I use the region of disposition. There is, unfortunately, no 
#flag for region of arraignment. There is a region of arrest flag, but the
#disposition region seems likely to more closely approximate arraignment region
supp %>%
  filter(!(larg_yr == 2021 & larg_mo %in% 10:12)) %>% #drop the last 3 months of 2021
  group_by(larg_yr, disposition_region, rearrested_180) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  filter(rearrested_180 == "Yes",
         disposition_region != "") %>% #drop those without a region, because it upsets pivot_wider
  pivot_wider(id_cols = larg_yr, names_from = disposition_region, values_from = p)

#ever rearrested, by region
supp %>%
  filter(!(larg_yr == 2021 & larg_mo %in% 10:12)) %>% #drop the last 3 months of 2021
  group_by(larg_yr, disposition_region, rearrested_ever) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  filter(rearrested_ever == "Yes",
         disposition_region != "") %>%
  pivot_wider(id_cols = larg_yr, names_from = disposition_region, values_from = p)

#table 4
supp %>%
  group_by(larg_yr, disposition_region, larg_rel_decision) %>%
  summarise(n = n()) %>%
  mutate(p = n/sum(n)) %>%
  pivot_wider(id_cols = c(larg_yr, disposition_region), names_from = larg_rel_decision, values_from = p) %>%
  arrange(disposition_region)
