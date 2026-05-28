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

# read data

refarrivals_ussr_czone <- read_dta("Data/Clean/refugee_arrivals_czone_dreher23.dta") %>% 
  filter(citizenship_stable == 'ussr') %>% 
  rename(origin = citizenship_stable)

data_ussr <- read_dta("Data/Clean/data_ipums_refugee.dta") %>% 
  filter(cntryname == "Ussr") %>% 
  rename(origin = cntryname) %>% 
  mutate(origin = tolower(origin))

# dorn puma-commuting zone crosswalk
puma_equiv_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta")

katrina_affected_puma <- tibble(year = 2007, puma_equiv = 2277777, czone = 3300, afactor = 1) # pumas recoded due to population loss in the aftermath of Hurricane Katrina

puma_equiv_czone_cross <- puma_equiv_czone_cross %>% 
  bind_rows(katrina_affected_puma) %>% 
  arrange(year, puma_equiv)

# duplicate 2007 crosswalk for 2015; this is to join multyear 2012 in ACS 2015 5 percent
puma_equiv_czone_cross <- puma_equiv_czone_cross %>% 
  filter(year == 2007) %>% 
  mutate(year = 2015) %>% 
  bind_rows(puma_equiv_czone_cross)

# contrast refugee arrivals in dreher with population stocks in census

refarrivals_ussr_us <- refarrivals_ussr_czone %>% 
  group_by(year) %>% 
  summarize(arrivals_dreher = sum(refugees))

ussrpop_us <- data_ussr %>%
    filter(year == 2000, yrimmig %in% 1985:2000) %>%
    group_by(yrimmig) %>%
    summarize(arrivals_ipums = sum(perwt))

# refugee prob around 80 percent for arrivals b/w 1988-1994
# more so given that arrivals for some months in 1990 missing
refprob_ussr <- refarrivals_ussr_us %>% 
  left_join(ussrpop_us, join_by(year==yrimmig)) %>% 
  filter(year >= 1985) %>% 
  mutate(refprob = arrivals_dreher / arrivals_ipums)

# get overall czone arrivals for 1988-94 cohort

refarrivals_ussr_cohort_czone <- refarrivals_ussr_czone %>% 
  filter(year %in% 1988:1994) %>% 
  group_by(czone) %>% 
  summarize(refugees = sum(refugees)) %>% 
  mutate(cohort = '8894', .before = everything())

# refpop for the cohort 8894 in 2000

ussrpop_cohort_puma <- data_ussr %>% 
  filter(year==2000, yrimmig %in% 1988:1994) %>% 
  group_by(year, puma_equiv) %>% 
  summarize(pop = sum(perwt)) %>% 
  mutate(cohort = '8894', .after = year)

ussrpop_cohort_czone <- ussrpop_cohort_puma %>% 
  left_join(puma_equiv_czone_cross) %>% 
  group_by(year,cohort,czone) %>% 
  summarize(pop = weighted.sum(pop, w = afactor))

# get overall ussr pop stocks

ussrpop_puma <- data_ussr %>% 
  group_by(year, puma_equiv) %>% 
  summarize(ussrpop = sum(perwt))

ussrpop_czone <- ussrpop_puma %>% 
  left_join(puma_equiv_czone_cross) %>% 
  group_by(year,czone) %>% 
  summarize(ussrpop = weighted.sum(ussrpop, w = afactor))

# complete czone coverage after first appearance

first_year <- ussrpop_czone %>%
  group_by(czone) %>%
  summarize(min_year = min(year), .groups = "drop")

# Expand year but filter out years before first arrival
ussrpop_czone_complete <- ussrpop_czone %>% ungroup() %>% 
  expand(czone, year) %>% 
  left_join(ussrpop_czone) %>% 
  left_join(first_year) %>% 
  filter(year >= min_year) %>% 
  mutate(ussrpop = replace_na(ussrpop, 0)) %>% 
  select(-min_year)

# broader pop controls

contpop_czone_ipums <- read_dta("Data/Clean/continentpop_czone_controls_ipums.dta") %>% 
  filter(bpl_continent == "e_europ")
fbpop_czone_ipums <- read_dta("Data/Clean/aggimmpop_czone_controls_ipums.dta")

totpop_czone_nhgis <- read_dta('Data/Clean/czone_characteristics_census19702000_ACS520102022.dta') %>% 
  select(year,czone,pop_tot) %>% 
  mutate(year = as.numeric(year)) %>% 
  mutate(year = ifelse(year == 2010, 2007, year)) %>% 
  filter(year %in% c(1970:2000, 2007)) %>% 
  rename(totpop_nhgis = pop_tot)

# construct net inflow measures
outcome_var <- refarrivals_ussr_cohort_czone %>% filter(refugees > 0) %>% # analysis of intensive margin
  left_join(ussrpop_cohort_czone) %>% 
  mutate(year = 2000) %>% relocate(year, .before = cohort) %>% 
  mutate(pop = ifelse(is.na(pop), 0, pop)) %>% 
  mutate(net_inflow = pop - refugees) # outcome variable

## ethnic network controls

# lagged ethnic networks

ussrpop_czone <- ussrpop_czone %>% 
  filter(year != 1990) %>% # ensures lag pop in 2000 connects to 1980
  group_by(czone) %>% 
  mutate(lagged_ussrpop = lag(ussrpop)) # %>% select(-year)

contpop_czone_ipums <- contpop_czone_ipums %>% 
  filter(year != 1990) %>% 
  group_by(czone) %>% 
  mutate(lagged_contpop_ipums = lag(contpop_ipums)) # %>% select(-year)

fbpop_czone_ipums <- fbpop_czone_ipums %>% 
  filter(year != 1990) %>% 
  group_by(czone) %>% 
  mutate(lagged_fbpop_ipums = lag(fbpop_ipums)) # %>% select(-year)

totpop_czone_nhgis <- totpop_czone_nhgis %>% 
  filter(year != 1990) %>% 
  group_by(czone) %>% 
  mutate(lagged_totpop_nhgis = lag(totpop_nhgis)) # %>% select(-year)

# join refugee origins with relavant ethnic networks 
ethnic_controls <- outcome_var %>% 
  left_join(ussrpop_czone) %>% 
  left_join(contpop_czone_ipums) %>% 
  left_join(fbpop_czone_ipums) %>% 
  left_join(totpop_czone_nhgis) %>% 
  mutate(across(contains("pop"), ~replace_na(., 0)))

ethnic_controls <- ethnic_controls %>% 
  mutate(resid_contpop_ipums = contpop_ipums - ussrpop,
         resid_fbpop_ipums = fbpop_ipums - contpop_ipums,
         resid_totpop_nhgis = totpop_nhgis - fbpop_ipums) %>% 
  mutate(resid_lagged_contpop = lagged_contpop_ipums - lagged_ussrpop,
         resid_lagged_fbpop = lagged_fbpop_ipums - lagged_contpop_ipums,
         resid_lagged_totpop = lagged_totpop_nhgis - lagged_fbpop_ipums)

# join outcome var and ethnic network vars
data <- outcome_var %>% 
  left_join(ethnic_controls) %>% 
  relocate(cohort, .before = everything()) %>% 
  select(-year)

# final cleaning
data <- data %>%
  mutate(origin = 'ussr') %>% 
  rename(origin_continent = bpl_continent) %>% 
  rename(placements = refugees) %>% 
  rename_with(~ gsub("ussr", "origin", .x)) %>%
  rename_with(~ gsub("ref", "", .x)) %>%
  rename_with(~ gsub("_ipums", "", .x)) %>%
  rename_with(~ gsub("_nhgis", "", .x)) %>% 
  select(cohort,origin,origin_continent,
         czone,placements,pop,net_inflow,
         contains('originpop'),
         contains('contpop'),
         contains('fbpop'),
         contains('totpop'))


write_dta(data, 'Data/Clean/data_ussr_outcome_ethnic_network_vars.dta')
write_dta(refarrivals_ussr_cohort_czone, "Data/Clean/refugee_arrivals_ussr_cohort_czone.dta")
write_dta(ussrpop_cohort_czone, 'Data/Clean/refugee_pop_ussr_cohort_czone.dta')
write_dta(ussrpop_czone_complete, 'Data/Clean/ussrpop_czone_controls.dta')
