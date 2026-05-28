rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(fixest)
library(stargazer)
library(RcppRoll)
library(readxl)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

## read data

refarrivals_cohort_czone_insample <- read_dta("Data/Clean/refugee_arrivals_cohort_czone_insample.dta")
refpop_cohort_czone_insample <- read_dta("Data/Clean/refugee_pop_cohort_czone_insample.dta")
originpop_czone_ipums <- read_dta("Data/Clean/refugee_pop_czone_ipums.dta")
contpop_czone_ipums <- read_dta("Data/Clean/continentpop_czone_controls_ipums.dta")
fbpop_czone_ipums <- read_dta("Data/Clean/aggimmpop_czone_controls_ipums.dta")

totpop_czone_nhgis <- read_dta('Data/Clean/czone_characteristics_census19702000_ACS520102022.dta') %>% 
  select(year,czone,pop_tot) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(year = ifelse(year == 2010, 2007, year)) %>% 
  filter(year %in% c(1970:2000, 2007)) %>% 
  rename(totpop_nhgis = pop_tot)
  
# rename vars for comparability

refarrivals_cohort_czone_insample <- refarrivals_cohort_czone_insample %>% 
  filter(cohort %in% c('7580','8090','9199','0004')) %>%  
  rename(origin=citizenship_stable)

refpop_cohort_czone_insample <- refpop_cohort_czone_insample %>% 
  filter(cohort %in% c('7580','8090','9199','0004')) %>% 
  rename(origin = cntryname)

originpop_czone_ipums <- originpop_czone_ipums %>% 
  mutate(cntryname=tolower(cntryname)) %>% 
  rename(origin = cntryname, originpop_ipums = refpop_ipums)

## construct outcome variable

# measure cohort population in most contemporary census/acs
refpop_cohort_czone_insample <- refpop_cohort_czone_insample %>%
  filter( (year == 1970 & cohort == 'pre1975') |
            (year == 1980 & cohort == '7580') | 
            (year == 1990 & cohort == '8090') |
            (year == 2000 & cohort == '9199') |
            (year == 2007 & cohort == '0004')) %>% 
  ungroup() %>% arrange(origin,cohort) # %>% select(-year)

# construct net inflow measures
outcome_var <- refarrivals_cohort_czone_insample %>% filter(refarrivals_dreher > 0) %>% # analysis of intensive margin
  left_join(refpop_cohort_czone_insample) %>% 
  mutate(refpop_ipums = ifelse(is.na(refpop_ipums), 0, refpop_ipums)) %>% 
  group_by(cohort) %>% mutate(year = mean(year, na.rm = T)) %>% ungroup() %>% 
  mutate(net_inflow = refpop_ipums - refarrivals_dreher) # outcome variable

## ethnic network controls

# lagged ethnic networks

originpop_czone_ipums <- originpop_czone_ipums %>% 
  group_by(origin,czone) %>% 
  mutate(lagged_originpop_ipums = lag(originpop_ipums)) # %>% select(-year)

contpop_czone_ipums <- contpop_czone_ipums %>% 
  group_by(bpl_continent,czone) %>% 
  mutate(lagged_contpop_ipums = lag(contpop_ipums)) # %>% select(-year)

fbpop_czone_ipums <- fbpop_czone_ipums %>% 
  group_by(czone) %>% 
  mutate(lagged_fbpop_ipums = lag(fbpop_ipums)) # %>% select(-year)

totpop_czone_nhgis <- totpop_czone_nhgis %>% 
  group_by(czone) %>% 
  mutate(lagged_totpop_nhgis = lag(totpop_nhgis)) # %>% select(-year)

# assign continent to refugee origins

asia_cntries <- c("afghanistan", "burma", "cambodia", "iran", "iraq", "laos", "vietnam")
e_europ_cntries <- c("poland", "romania", "ussr", "yugoslavia")
afric_cntries <- c("ethiopia", "liberia", "somalia", "sudan")
s_americ_cntries <- "cuba"

outcome_var <- outcome_var %>%
  mutate(origin_continent = case_match(origin,
                                asia_cntries ~ "asia",
                                e_europ_cntries ~ "e_europ",
                                afric_cntries ~ "afric",
                                s_americ_cntries ~ "s_americ"),
         .after = origin)

# join refugee origins with relavant ethnic networks 
ethnic_controls <- outcome_var %>% 
  select(year,origin,origin_continent,czone) %>% 
  left_join(originpop_czone_ipums) %>% 
  left_join(contpop_czone_ipums, join_by(year, origin_continent==bpl_continent, czone)) %>% 
  left_join(fbpop_czone_ipums) %>% 
  left_join(totpop_czone_nhgis) %>% 
  mutate(across(contains("ipums"), ~replace_na(., 0)))

ethnic_controls <- ethnic_controls %>% 
  mutate(resid_contpop_ipums = contpop_ipums - originpop_ipums,
         resid_fbpop_ipums = fbpop_ipums - contpop_ipums,
         resid_totpop_nhgis = totpop_nhgis - fbpop_ipums) %>% 
  mutate(resid_lagged_contpop = lagged_contpop_ipums - lagged_originpop_ipums,
         resid_lagged_fbpop = lagged_fbpop_ipums - lagged_contpop_ipums,
         resid_lagged_totpop = lagged_totpop_nhgis - lagged_fbpop_ipums)

# join outcome var and ethnic network vars
data <- outcome_var %>% 
  left_join(ethnic_controls) %>% 
  relocate(cohort, .before = everything()) %>% 
  relocate(in_sample, .after = everything()) %>% 
  select(-year)

# final cleaning
data <- data %>%
  rename_with(~ gsub("ref", "", .x)) %>%
  rename_with(~ gsub("_ipums", "", .x)) %>%
  rename_with(~ gsub("_nhgis", "", .x)) %>% 
  rename(placements = arrivals_dreher) %>% 
  select(cohort,origin,origin_continent,
         czone,placements,pop,net_inflow,
         contains('originpop'),
         contains('contpop'),
         contains('fbpop'),
         contains('totpop'),in_sample)

write_dta(data, 'Data/Clean/data_main_outcome_ethnic_network_vars.dta')
