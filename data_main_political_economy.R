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

# czone-level; census/acs
popov25_college <- read_dta('Data/Clean/czone_characteristics_census19702000_ACS520102022.dta') %>%
  mutate(year = as.numeric(year)) %>%  # last sample: 2008-2012 acs
  filter(year %in% 1970:2000) %>% 
  select(year, czone, pop_ov25_4ycol)

# state-level; annual; semi-annual 
voteshare_state_senate <- read_dta("Data/Clean/state_voteshare_senate.dta")
voteshare_state_presid <- read_dta("Data/Clean/state_voteshare_president.dta")
expend_state <- read_dta("Data/Clean/state_expenditure_pcinc_normalized.dta")

# cpi inflation adjustor
cpi_1999base <- read_dta("Data/Clean/cpi_1999base.dta")

# David Dorn's ipums-commuting zone correspondence
puma_state_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta") %>%
  filter(year == 1990) %>% select(-year) %>%
  mutate(puma_equiv = str_pad(puma_equiv, width = 6, pad = "0")) %>%
  mutate(statefp = str_sub(puma_equiv, start = 1, end = 2))

## clean

# U.S. senate voteshare; state-level; biannual
voteshare_state_senate <- voteshare_state_senate %>% # bi-yearly; last year: 2021
  filter(year <= 2004, statefp %notin% c('02','15','72')) %>%
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1980:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004')) %>%
  group_by(decade,statefp,statename,party_simplified) %>%
  summarize(voteshare_senate = mean(voteshare_senate)) %>%
  pivot_wider(names_from = party_simplified, names_prefix = "voteshare_senate_", values_from = voteshare_senate)

# U.S. president voteshare; state-level
voteshare_state_presid <- voteshare_state_presid %>% # four year intervals; last year: 2020
  filter(year <= 2004, statefp %notin% c('02','15','72')) %>%
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1980:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004')) %>%
  group_by(decade,statefp,statename,party_simplified) %>%
  summarize(voteshare_president = mean(voteshare_president)) %>%
  pivot_wider(names_from = party_simplified, names_prefix = "voteshare_president_", values_from = voteshare_president)

# state-level expenditure; 1972, 1977-2009 annual; convert to 1999 dollars
expend_state <- expend_state %>% 
  # left_join(cpi_1999base) %>% 
  # mutate(expend_pcpi_norm = expend_pcpi_norm / cpi_1999base) %>% 
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1980:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004')) %>%
  group_by(decade,statefp,statename) %>%
  summarize(expend_pcpi_norm = mean(expend_pcpi_norm))

# join
state_voteshare_expend <- voteshare_state_presid %>% 
  left_join(voteshare_state_senate) %>% 
  left_join(expend_state) %>% 
  select(!contains('republican')) 

# assign state-level vars to czones
state_czone_voteshare_expend <- state_voteshare_expend %>% 
  left_join(puma_state_czone_cross) %>% 
  group_by(decade,czone) %>% 
  summarize(across(contains(c('voteshare','expend')), ~stats::weighted.mean(., w = afactor)))

# number of people over 25 with 4year college
popov25_college <- popov25_college %>% 
  mutate(decade = case_match(year, 1970 ~ '7580', 1980 ~ '8090', 1990 ~ '9199', 2000 ~ '0004' )) %>% 
  rename(lagged_popov25_4ycol = pop_ov25_4ycol) %>% 
  select(-year)

# join college population to other vars
state_czone_political_economy <- state_czone_voteshare_expend %>% 
  left_join(popov25_college) %>% 
  rename(cohort = decade)

# join quality: only missing senate voteshare for Washington DC

state_czone_political_economy %>% ungroup() %>% 
  filter(czone != 11304) %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna>0)

state_czone_political_economy %>% ungroup() %>% 
  filter(czone == 11304) %>% 
  select(-voteshare_senate_democrat) %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna>0)

#### end join data ####

write_dta(state_czone_political_economy, "Data/Clean/data_main_political_economy.dta")
