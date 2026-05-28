rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# read refarrivals data from Dreher et al
refarrivals_czone <- read_dta("Data/Clean/refugee_arrivals_czone_dreher23.dta")

# need to duplicate 1980 arrivals since 1980 arrivals are not uniquely identified in ipums
# so i assign these arrivals to both 1975-1980 cohort and 1980-1990 cohort

refarrivals_czone_1980 <- refarrivals_czone %>% 
  filter(year == 1980) %>% 
  mutate(cohort = '8090', .after = year)

refarrivals_cohort_czone <- refarrivals_czone %>% 
  mutate(cohort = case_match(year, 
                             1975:1980 ~ "7580",
                             1980:1990 ~ "8090", 
                             1991:1999 ~ "9199",
                             2000:2004 ~ "0004",
                             2005:2008 ~ "0508"), .after = year) %>% 
  bind_rows(refarrivals_czone_1980) %>% 
  group_by(citizenship_stable,cohort,czone,region) %>% 
  summarize(refugees = sum(refugees))

# refugee arrivals to commuting zones

refarrivals_cohort_czone %>% 
  filter(cohort %in% c('7580','8090','9199','0004')) %>% 
  pull(czone) %>% unique() %>% length()

refarrivals_cohort_czone %>% 
  filter(cohort %in% c('7580','8090','9199','0004')) %>% 
  group_by(citizenship_stable,czone,region) %>% 
  summarise(refugees=sum(refugees)) %>% 
  ungroup() %>% 
  count(citizenship_stable)


write_dta(refarrivals_cohort_czone, "Data/Clean/refugee_arrivals_cohort_czone_dreher23.dta")
