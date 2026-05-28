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
# should i carry out the analysis matching refugee characteristics with imm characteristics?

# read data

# filtered out 2015 data from both datasets below
immarrivals_cohort_czone <- read_dta("Data/Clean/nonrefugee_immigrant_pop_cohort_origin_czone_ipums.dta")
originpop_czone <- read_dta("Data/Clean/nonrefugee_immigrant_pop_origin_czone_ipums.dta")

# broader pop controls

contpop_czone_ipums <- read_dta("Data/Clean/continentpop_czone_controls_ipums.dta") 
fbpop_czone_ipums <- read_dta("Data/Clean/aggimmpop_czone_controls_ipums.dta")

totpop_czone_nhgis <- read_dta('Data/Clean/czone_characteristics_census19702000_ACS520102022.dta') %>% 
  select(year,czone,pop_tot) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(year = ifelse(year == 2010, 2007, year)) %>% 
  filter(year %in% c(1970:2000, 2007)) %>% 
  rename(totpop_nhgis = pop_tot)

# # construct net inflow measures
# outcome_var <- refarrivals_origin_cohort_czone %>% filter(refugees > 0) %>% # analysis of intensive margin
#   left_join(originpop_cohort_czone) %>% 
#   mutate(year = 2000) %>% relocate(year, .before = cohort) %>% 
#   mutate(pop = ifelse(is.na(pop), 0, pop)) %>% 
#   mutate(net_inflow = pop - refugees) # outcome variable

## ethnic network controls

# lagged ethnic networks

originpop_czone <- originpop_czone %>% 
  group_by(origin,czone) %>% 
  mutate(lagged_originpop = lag(originpop, default = 0)) # %>% select(-year)

contpop_czone_ipums <- contpop_czone_ipums %>% 
  rename(continent = bpl_continent) %>% 
  group_by(continent,czone) %>% 
  mutate(lagged_contpop_ipums = lag(contpop_ipums, default = 0)) # %>% select(-year)

fbpop_czone_ipums <- fbpop_czone_ipums %>% 
  group_by(czone) %>% 
  mutate(lagged_fbpop_ipums = lag(fbpop_ipums, default = 0)) # %>% select(-year)

totpop_czone_nhgis <- totpop_czone_nhgis %>% 
  group_by(czone) %>% 
  mutate(lagged_totpop_nhgis = lag(totpop_nhgis, default = 0)) # %>% select(-year)

# join refugee origins with relavant ethnic networks 
data <- immarrivals_cohort_czone %>% 
  left_join(originpop_czone) %>% 
  left_join(contpop_czone_ipums) %>% 
  left_join(fbpop_czone_ipums) %>% 
  left_join(totpop_czone_nhgis) %>% 
  mutate(across(contains("pop"), ~replace_na(., 0)))

data <- data %>% 
  mutate(resid_contpop_ipums = contpop_ipums - originpop,
         resid_fbpop_ipums = fbpop_ipums - contpop_ipums,
         resid_totpop_nhgis = totpop_nhgis - fbpop_ipums) %>% 
  mutate(resid_lagged_contpop = lagged_contpop_ipums - lagged_originpop,
         resid_lagged_fbpop = lagged_fbpop_ipums - lagged_contpop_ipums,
         resid_lagged_totpop = lagged_totpop_nhgis - lagged_fbpop_ipums)

# final cleaning
data <- data %>% 
  rename(origin_continent = continent) %>% 
  rename_with(~ gsub("_ipums", "", .x)) %>%
  rename_with(~ gsub("_nhgis", "", .x)) %>% 
  select(cohort,origin,origin_continent, czone,pop,
         contains('originpop'), contains('contpop'),
         contains('fbpop'), contains('totpop'))

write_dta(data, 'Data/Clean/data_nonrefugee_immigrant_outcome_ethnic_network_vars.dta')
