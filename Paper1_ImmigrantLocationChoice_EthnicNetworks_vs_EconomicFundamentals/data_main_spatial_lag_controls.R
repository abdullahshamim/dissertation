rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

## read data

refarrivals_cohort_czone <- read_dta("Data/Clean/refugee_arrivals_cohort_czone_insample.dta")
originpop_czone <- read_dta("Data/Clean/refugee_pop_czone_ipums.dta")
czone_spatial_linkage_measures <- read_dta("Data/Clean/czone_spatial_linkage_measures.dta")

# rename vars for comparability

refarrivals_cohort_czone <- refarrivals_cohort_czone %>% 
  filter(cohort %in% c('7580','8090','9199','0004')) %>%  
  rename(origin=citizenship_stable)

originpop_czone <- originpop_czone %>% 
  mutate(cntryname=tolower(cntryname)) %>% 
  rename(origin = cntryname, originpop = refpop_ipums) %>% 
  filter(year!=2015)

### spatial lags ethnic networks

## calculate refarrivals in nearby czones; this is to control for the ease of secondary migration between czones #

refarrivals_cohort_czone_wide <- refarrivals_cohort_czone %>% 
  arrange(czone, cohort) %>% 
  select(cohort,origin,czone,refarrivals_dreher) %>% # identifies all distinct observations
  pivot_wider(names_from = czone, 
              names_prefix = "refarrivals_cz", 
              values_from = refarrivals_dreher,
              values_fill = 0)

# aligning reference czone with refarrivals from other czones
refarrivals_cohort_czone_matrix <- refarrivals_cohort_czone %>% 
  arrange(czone, cohort) %>% 
  select(cohort,origin,czone) %>% 
  left_join(refarrivals_cohort_czone_wide) %>% 
  rename(refer_cz = czone)

refarrivals_cohort_czone_matrix_long <- refarrivals_cohort_czone_matrix %>% 
  pivot_longer(starts_with("refarrivals"),
               names_to = "dest_cz",
               names_prefix = "refarrivals_cz",
               values_to = "refarrivals_dreher")

# inverse distance-weighted arrivals in czones within threshold
nearby_refarrivals <- refarrivals_cohort_czone_matrix_long %>% 
  left_join(czone_spatial_linkage_measures) %>% 
  group_by(cohort,origin,refer_cz) %>% 
  summarize(nearby_arrivals_sum = sum(within_threshold * refarrivals_dreher),
            nearby_arrivals_idw = sum(threshold_idw * refarrivals_dreher),
            nearby_arrivals_idw_norm = sum(threshold_idw_norm * refarrivals_dreher)) %>% 
  rename(czone = refer_cz)

## lagged origin pop in nearby czones; needs more careful construction

originpop_czone_wide <- originpop_czone %>% 
  arrange(czone,year) %>% 
  pivot_wider(names_from = czone, names_prefix = 'originpop_cz', values_from = originpop)
  
# reference czones; reference czones are a subset of destination czones
originpop_czone_matrix <- originpop_czone %>%
  arrange(czone, year) %>%
  select(year,origin,czone) %>%
  left_join(originpop_czone_wide)

originpop_czone_matrix_long <- originpop_czone_matrix %>%
  rename(refer_cz = czone) %>%
  pivot_longer(starts_with("originpop"),
               names_to = "dest_cz",
               names_prefix = "originpop_cz",
               values_to = "originpop") %>% 
  mutate(originpop = ifelse(is.na(originpop), 0, originpop))

# inverse distance-weighted arrivals in czones within threshold
nearby_originpop <- originpop_czone_matrix_long %>%
  left_join(czone_spatial_linkage_measures) %>% 
  group_by(year,origin,refer_cz) %>%
  summarize(nearby_originpop_sum = sum(within_threshold * originpop),
            nearby_originpop_idw = sum(threshold_idw * originpop),
            nearby_originpop_idw_norm = sum(threshold_idw_norm * originpop)) %>% 
  rename(czone = refer_cz)

lagged_nearby_originpop <- nearby_originpop %>% 
  group_by(czone,origin) %>% 
  mutate(lagged_nearby_originpop_sum = lag(nearby_originpop_sum, default = 0),
         lagged_nearby_originpop_idw = lag(nearby_originpop_idw, default = 0),
         lagged_nearby_originpop_idw_norm = lag(nearby_originpop_idw_norm, default = 0)) %>% 
  select(year,origin,czone,contains('lagged')) %>% 
  filter(year != 1970)

# assign cohort
lagged_nearby_originpop <- lagged_nearby_originpop %>% 
  mutate(cohort = case_match(year,
                             1980 ~ '7580',
                             1990 ~ '8090',
                             2000 ~ '9199',
                             2007 ~ '0004'),
         .after = year) %>% 
  select(-year)

## join refarrivals and lagged pop in nearby czones
spatial_lag_controls <- nearby_refarrivals %>% 
  left_join(lagged_nearby_originpop) %>% 
  mutate(across(contains('lagged'), ~ replace_na(., 0)))
  
  
# join spatial lag controls with main data
write_dta(spatial_lag_controls, 'Data/Clean/data_main_spatial_lag_controls.dta')
