rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(haven)
library(tidyverse)
library(lubridate)

# read data
bea_emp_czone <- read_dta("Data/Clean/bea_emp_czone.dta")
seer_pop_czone <- read_dta("Data/Clean/seer_wrkagepop_czone.dta")
bls_unrate <- read_csv("Data/Raw/BEA_Emp/BLS_UNRATE.csv")

bls_unrate_yearly <- bls_unrate %>%
  mutate(year = year(ymd(observation_date))) %>%
  group_by(year) %>%
  summarise(avg_unemp_rate = mean(UNRATE, na.rm = TRUE)) %>% 
  mutate(avg_unemp_rate = avg_unemp_rate / 100) %>% 
  filter(year>=1969)

emp_pop_ratio <- bea_emp_czone %>% 
  mutate(year = as.integer(year)) %>% 
  filter(!is.na(year)) %>% 
  left_join(seer_pop_czone) %>% 
  mutate(emp_pop_ratio = emp_tot / population)

emp_pop_ratio_us <- emp_pop_ratio %>% 
  group_by(year) %>% 
  summarize(emp_pop_ratio = stats::weighted.mean(emp_pop_ratio, w=emp_tot))

bea_bls_comp <- emp_pop_ratio_us %>% 
  left_join(bls_unrate_yearly) %>% 
  pivot_longer(2:3, names_to = 'data_source', values_to = "value")

# chatgpt

p <- ggplot(bea_bls_comp, aes(x = year, y = value, color = data_source)) + 
  geom_line(size = 1) + 
  scale_color_manual(
    values = c("emp_pop_ratio" = "darkred","avg_unemp_rate" = "blue3"),
    labels = c("Unemployment Rate", "Employment-to-Population Ratio")
  ) +
  labs(x=NULL,y = NULL,color = NULL) +
  theme_bw() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    legend.margin = margin(b = -5)
  )

p

ggsave("Output/Figures/emppopratio_unemprate_comparison.png",
       height = 4, width = 7, p)

emp_pop_ratio_cohort_1 <- emp_pop_ratio %>% 
  filter(year %in% 1975:1980) %>% 
  mutate(cohort='7580')

emp_pop_ratio_cohort_2 <- emp_pop_ratio %>% 
  mutate(cohort = case_match(year,
                             1969:1974 ~ 'pre1975',
                             1980:1990 ~ '8090',
                             1991:1999 ~ '9199',
                             2000:2004 ~ '0004',
                             2005:2020 ~ 'post2004')) %>% 
  filter(!is.na(cohort))

emp_pop_ratio_cohort <- emp_pop_ratio_cohort_1 %>% 
  bind_rows(emp_pop_ratio_cohort_2)

emp_pop_ratio_cohort %>% filter(year == 1980) %>% 
  count(year,cohort)

emp_pop_ratio_cohort <- emp_pop_ratio_cohort %>% 
  group_by(cohort,czone) %>% 
  summarize(emp_pop_ratio = mean(emp_pop_ratio))

write_dta(emp_pop_ratio_cohort, "Data/Clean/emppopratio_czone_cohort.dta")
