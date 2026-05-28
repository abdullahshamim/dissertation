rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

## read data

immarrivals_cohort_czone <- read_dta("Data/Clean/nonrefugee_immigrant_pop_cohort_origin_czone_ipums.dta")
originpop_czone <- read_dta("Data/Clean/nonrefugee_immigrant_pop_origin_czone_ipums.dta")
czone_spatial_linkage_measures <- read_dta("Data/Clean/czone_spatial_linkage_measures.dta")

### spatial lags ethnic networks

## contemporary origin-cohort arrivals in nearby CZs

immarrivals_cohort_czone_wide <- immarrivals_cohort_czone %>% 
  arrange(czone, cohort) %>% 
  pivot_wider(names_from = czone, 
              names_prefix = "immarrivals_cz", 
              values_from = pop,
              values_fill = 0)

# aligning reference czone with refarrivals from other czones
immarrivals_cohort_czone_matrix <- immarrivals_cohort_czone %>% 
  arrange(czone, cohort) %>% 
  select(year,cohort,origin,continent,czone) %>% 
  left_join(immarrivals_cohort_czone_wide) %>% 
  rename(refer_cz = czone)

immarrivals_cohort_czone_matrix_long <- immarrivals_cohort_czone_matrix %>% 
  pivot_longer(starts_with("immarrivals"),
               names_to = "dest_cz",
               names_prefix = "immarrivals_cz",
               values_to = "immarrivals")

# inverse distance-weighted arrivals in czones within threshold
nearby_immarrivals <- immarrivals_cohort_czone_matrix_long %>% 
  left_join(czone_spatial_linkage_measures) %>% 
  group_by(cohort, origin, refer_cz) %>% 
  summarize(nearby_arrivals_sum = sum(within_threshold * immarrivals),
            nearby_arrivals_idw = sum(threshold_idw * immarrivals),
            nearby_arrivals_idw_norm = sum(threshold_idw_norm * immarrivals)) %>% 
  rename(czone = refer_cz)

## lagged origin pop in nearby czones

originpop_czone_wide <- originpop_czone %>% 
  arrange(czone,year) %>% 
  pivot_wider(names_from = czone, 
              names_prefix = 'originpop_cz', 
              values_from = originpop, 
              values_fill = 0) 

# reference czones; reference czones are a subset of destination czones
originpop_czone_matrix <- originpop_czone %>%
  arrange(czone, year) %>%
  select(year,origin,continent,czone) %>%
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
  arrange(year) %>% 
  group_by(origin,czone) %>% 
  mutate(lagged_nearby_originpop_sum = lag(nearby_originpop_sum, default = 0),
         lagged_nearby_originpop_idw = lag(nearby_originpop_idw, default = 0),
         lagged_nearby_originpop_idw_norm = lag(nearby_originpop_idw_norm, default = 0)) %>% 
  filter(year != 1970) %>% 
  select(year,origin,czone,contains('lagged'))

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
spatial_lag_controls <- nearby_immarrivals %>% 
  left_join(lagged_nearby_originpop) %>% 
  mutate(across(contains('lagged'), ~ replace_na(., 0)))


# join spatial lag controls with main data
write_dta(spatial_lag_controls, 'Data/Clean/data_nonrefugee_immigrant_spatial_lag_controls.dta')
