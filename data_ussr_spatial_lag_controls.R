rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

## read data

refarrivals_ussr_cohort_czone <- read_dta("Data/Clean/refugee_arrivals_ussr_cohort_czone.dta")
ussrpop_czone <- read_dta("Data/Clean/ussrpop_czone_controls.dta") %>% filter(year!=2015)
czone_spatial_linkage_measures <- read_dta("Data/Clean/czone_spatial_linkage_measures.dta")

# rename vars for comparability

# refarrivals_ussr_cohort_czone <- refarrivals_ussr_cohort_czone %>% 
#   rename(origin=citizenship_stable)

# ussrpop_czone <- ussrpop_czone %>% 
#   mutate(cntryname=tolower(cntryname)) %>% 
#   rename(origin = cntryname, originpop = refpop_ipums) %>% 
#   filter(year!=2015)

### spatial lags ethnic networks

## calculate refarrivals in nearby czones; this is to control for the ease of secondary migration between czones #

refarrivals_ussr_cohort_czone_wide <- refarrivals_ussr_cohort_czone %>% 
  arrange(czone, cohort) %>% 
  pivot_wider(names_from = czone, 
              names_prefix = "refarrivals_cz", 
              values_from = refugees,
              values_fill = 0)

# aligning reference czone with refarrivals from other czones
refarrivals_ussr_cohort_czone_matrix <- refarrivals_ussr_cohort_czone %>% 
  arrange(czone, cohort) %>% 
  select(cohort,czone) %>% 
  left_join(refarrivals_ussr_cohort_czone_wide) %>% 
  rename(refer_cz = czone)

refarrivals_ussr_cohort_czone_matrix_long <- refarrivals_ussr_cohort_czone_matrix %>% 
  pivot_longer(starts_with("refarrivals"),
               names_to = "dest_cz",
               names_prefix = "refarrivals_cz",
               values_to = "refarrivals_dreher")

# inverse distance-weighted arrivals in czones within threshold
nearby_refarrivals <- refarrivals_ussr_cohort_czone_matrix_long %>% 
  left_join(czone_spatial_linkage_measures) %>% 
  group_by(cohort,refer_cz) %>% 
  summarize(nearby_arrivals_sum = sum(within_threshold * refarrivals_dreher),
            nearby_arrivals_idw = sum(threshold_idw * refarrivals_dreher),
            nearby_arrivals_idw_norm = sum(threshold_idw_norm * refarrivals_dreher)) %>% 
  rename(czone = refer_cz)

## lagged origin pop in nearby czones; needs more careful construction

ussrpop_czone_wide <- ussrpop_czone %>% 
  arrange(czone,year) %>% 
  pivot_wider(names_from = czone, 
              names_prefix = 'ussrpop_cz', 
              values_from = ussrpop, 
              values_fill = 0) 

# reference czones; reference czones are a subset of destination czones
ussrpop_czone_matrix <- ussrpop_czone %>%
  arrange(czone, year) %>%
  select(year,czone) %>%
  left_join(ussrpop_czone_wide)

ussrpop_czone_matrix_long <- ussrpop_czone_matrix %>%
  rename(refer_cz = czone) %>%
  pivot_longer(starts_with("ussrpop"),
               names_to = "dest_cz",
               names_prefix = "ussrpop_cz",
               values_to = "ussrpop") %>% 
  mutate(ussrpop = ifelse(is.na(ussrpop), 0, ussrpop))

# inverse distance-weighted arrivals in czones within threshold
nearby_ussrpop <- ussrpop_czone_matrix_long %>%
  left_join(czone_spatial_linkage_measures) %>% 
  group_by(year,refer_cz) %>%
  summarize(nearby_ussrpop_sum = sum(within_threshold * ussrpop),
            nearby_ussrpop_idw = sum(threshold_idw * ussrpop),
            nearby_ussrpop_idw_norm = sum(threshold_idw_norm * ussrpop)) %>% 
  rename(czone = refer_cz)

lagged_nearby_ussrpop <- nearby_ussrpop %>% 
  arrange(year) %>% 
  filter(year != 1990) %>% 
  group_by(czone) %>% 
  mutate(lagged_nearby_ussrpop_sum = lag(nearby_ussrpop_sum, default = 0),
         lagged_nearby_ussrpop_idw = lag(nearby_ussrpop_idw, default = 0),
         lagged_nearby_ussrpop_idw_norm = lag(nearby_ussrpop_idw_norm, default = 0)) %>% 
  filter(year != 1970) %>% 
  select(year,czone,contains('lagged'))

# assign cohort
lagged_nearby_ussrpop <- lagged_nearby_ussrpop %>% 
  mutate(cohort = case_match(year,
                             1980 ~ '7580',
                             2000 ~ '8894',
                             2007 ~ '0004'),
         .after = year) %>% 
  select(-year)

## join refarrivals and lagged pop in nearby czones
spatial_lag_controls <- nearby_refarrivals %>% 
  left_join(lagged_nearby_ussrpop) %>% 
  mutate(across(contains('lagged'), ~ replace_na(., 0)))


# join spatial lag controls with main data
write_dta(spatial_lag_controls, 'Data/Clean/data_ussr_spatial_lag_controls.dta')
