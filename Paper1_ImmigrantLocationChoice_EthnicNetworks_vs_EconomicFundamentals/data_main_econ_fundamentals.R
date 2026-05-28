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

## read economic fundamentals data

hp_czone <- read_dta("Data/Clean/hp_fixed_effects_czone_year_attempt2.dta") # last sample: 2011-2015 acs; contains 2005-2007 acs
wage_czone <- read_dta("Data/Clean/wage_fixed_effects_czone_year.dta") # last sample: 2011-2015 acs; contains 2005-2007 acs
emp_pop_czone <- read_dta("Data/Clean/emp_pop_ratio_czone.dta")
unemp_state <- read_dta("Data/Clean/state_unemp_rate_clean.dta")

czone_characteristics <- read_dta('Data/Clean/czone_characteristics_census19702000_ACS520102022.dta') %>% 
  mutate(year = as.numeric(year)) %>%  # last sample: 2008-2012 acs
  mutate(year = ifelse(year==2010,2007,year)) %>% 
  filter(year %in% c(1970:2000, 2007, 2015))

# inflation
cpi_1999base <- read_dta("Data/Clean/cpi_1999base.dta")

# David Dorn's ipums-commuting zone correspondence
puma_state_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta") %>%
  filter(year == 1990) %>% select(-year) %>%
  mutate(puma_equiv = str_pad(puma_equiv, width = 6, pad = "0")) %>%
  mutate(statefp = str_sub(puma_equiv, start = 1, end = 2))

## economic fundamentals from census/acs

econ_fund_census <- czone_characteristics %>% 
  filter(year <= 2000) %>% 
  select(year,czone,pcinc,medhhinc) %>% 
  left_join(hp_czone) %>% 
  left_join(wage_czone %>% filter(educ==1) %>% select(-educ)) %>% 
  mutate(decade = case_match(year, 1970 ~ '7580', 1980 ~ '8090', 1990 ~ '9199', 2000 ~ '0004')) # %>% select(-year)

## economic fundamentals from yearly data

# state level unemployment assigned to czones

unemp_state_1980 <- unemp_state %>%
  filter(year == 1980) %>%
  slice(rep(1:n(), each = 2)) %>%
  mutate(decade = rep(c("7580", "8090"), times = nrow(.) / 2))

unemp_state <- unemp_state %>% 
  filter(year != 1980, year >= 1970, year <= 2004) %>% 
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1981:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004')) %>% 
  bind_rows(unemp_state_1980) %>% 
  group_by(decade,statefp,state_name) %>% 
  summarize(unemp_rate = mean(unemp_rate)) %>%
  rename(statename = state_name)

unemp_state_czone <- unemp_state %>% 
  left_join(puma_state_czone_cross) %>%
  rename(unemp_state = unemp_rate) %>% 
  relocate(c(unemp_state, afactor), .after = czone) %>% 
  group_by(czone,decade) %>% 
  summarize(unemp_state_czone = stats::weighted.mean(unemp_state, w = afactor))
  
# czone employment-population ratio

emp_pop_czone_1980 <- emp_pop_czone %>%
  filter(year == 1980) %>%
  slice(rep(1:n(), each = 2)) %>%
  mutate(decade = rep(c("7580", "8090"), times = nrow(.) / 2))

emp_pop_czone <- emp_pop_czone %>% 
  filter(year != 1980, year >= 1970, year <= 2004) %>% 
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1981:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004')) %>% 
  bind_rows(emp_pop_czone_1980) %>% 
  group_by(decade,czone) %>% 
  summarize(emp_pop_ratio = mean(emp_pop_ratio))

# final employment rate fundamentals (state-level unemp rate + czone-level emp-pop ratio)
econ_fund_emp <- emp_pop_czone %>% 
  left_join(unemp_state_czone %>% select(decade,czone,unemp_state_czone))

# join fudamentals from census and emp
econ_fund <- econ_fund_census %>% 
  left_join(econ_fund_emp) %>% 
  relocate(decade, .after = year) %>% 
  rename(cohort=decade)

# convert to 1999 dollars; comment out for nominal dollars
econ_fund <- econ_fund %>%
  left_join(cpi_1999base %>% select(year,cpi_1999base)) %>%
  mutate(across(c(pcinc, medhhinc, hp, wage), ~ .x / cpi_1999base))

# checks for join matches; only missing pcinc and medhhinc in 1970

econ_fund %>% 
  filter(year == 1970) %>% 
  select(-pcinc,-medhhinc) %>% 
  ungroup() %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna > 0)

econ_fund %>% 
  filter(year != 1970) %>% 
  ungroup() %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna > 0)

# leave out redundant year var; can keep also
econ_fund <- econ_fund %>% select(-year)

# save
write_dta(econ_fund, "Data/Clean/data_main_econ_fundamentals.dta")
