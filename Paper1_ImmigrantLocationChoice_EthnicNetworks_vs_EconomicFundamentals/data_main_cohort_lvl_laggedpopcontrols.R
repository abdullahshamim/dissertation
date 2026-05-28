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

#### read data ####

# czone-level; refugee country/continent + aggregate immigrant population
refarrivals_cohort_czone_insample <- read_dta("Data/Clean/refugee_arrivals_cohort_czone_insample.dta") %>% filter(cohort!='0508')
refpop_cohort_czone_insample <- read_dta("Data/Clean/refugee_pop_cohort_czone_insample.dta")
refpop_czone_ipums <- read_dta("Data/Clean/refugee_pop_czone_ipums.dta") %>% mutate(cntryname=tolower(cntryname))
contpop_czone_ipums <- read_dta("Data/Clean/continentpop_czone_controls_ipums.dta")
fbpop_czone_ipums <- read_dta("Data/Clean/aggimmpop_czone_controls_ipums.dta")

# czone-level; nhgis/ipums; 1970-2000 census; ACS 5% 2010, 2015
czone_characteristics <- read_dta('Data/Clean/czone_characteristics_census19702000_ACS520102022.dta') %>% 
  mutate(year = as.numeric(year)) %>%  # last sample: 2008-2012 acs
  mutate(year = ifelse(year==2010,2007,year)) %>% 
  filter(year %in% c(1970:2000, 2007, 2015))
hp_czone <- read_dta("Data/Clean/hp_fixed_effects_czone_year_attempt2.dta") # last sample: 2011-2015 acs; contains 2005-2007 acs
wage_czone <- read_dta("Data/Clean/wage_fixed_effects_czone_year.dta") # last sample: 2011-2015 acs; contains 2005-2007 acs
emp_pop_ratio <- read_dta("Data/Clean/emppopratio_czone_cohort.dta")


# state-level; annual; semi-annual 
unemp_state <- read_dta("Data/Clean/state_unemp_rate_clean.dta")
voteshare_state_senate <- read_dta("Data/Clean/state_voteshare_senate.dta")
voteshare_state_presid <- read_dta("Data/Clean/state_voteshare_president.dta")
expend_state <- read_dta("Data/Clean/state_expenditure_pcinc_normalized.dta")

# state-county-level; time-invariant
climate_country_czone <- read_dta("Data/Clean/climate_distance_country_czone.dta")

# cpi inflation adjustor
cpi_1999base <- read_dta("Data/Clean/cpi_1999base.dta")

#### end read data ####

#### clean ####

# state unemployment; annual

unemp_state <- unemp_state %>% # yearly data - last year: 2010
  filter(year < 2009) %>% 
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1980:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004', 2005:2008 ~ '0508')) %>% 
  group_by(decade,statefp,state_name) %>% 
  summarize(unemp_rate = mean(unemp_rate)) %>% 
  rename(statename = state_name)

# U.S. senate voteshare; state-level; biannual

voteshare_state_senate <- voteshare_state_senate %>% # bi-yearly; last year: 2021
  filter(year < 2009) %>%
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1980:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004', 2005:2008 ~ '0508')) %>%
  group_by(decade,statefp,statename,party_simplified) %>%
  summarize(voteshare_senate = mean(voteshare_senate)) %>%
  pivot_wider(names_from = party_simplified, names_prefix = "voteshare_senate_", values_from = voteshare_senate)

# U.S. president voteshare; state-level

voteshare_state_presid <- voteshare_state_presid %>% # four year intervals; last year: 2020
  filter(year < 2009) %>%
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1980:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004', 2005:2008 ~ '0508')) %>%
  group_by(decade,statefp,statename,party_simplified) %>%
  summarize(voteshare_president = mean(voteshare_president)) %>%
  pivot_wider(names_from = party_simplified, names_prefix = "voteshare_president_", values_from = voteshare_president)

# state-level expenditure; 1972, 1977-2009 annual

# convert to 1999 dollars
expend_state <- expend_state %>% 
  left_join(cpi_1999base) %>% 
  mutate(expend_pcpi_norm = expend_pcpi_norm / cpi_1999base)

# average decadal expenditure normalized by per-capita income
expend_state <- expend_state %>% # mostly annual data; last year: 2011
  filter(year < 2009) %>% 
  mutate(decade = case_match(year, 1970:1979 ~ '7580', 1980:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004', 2005:2008 ~ '0508')) %>% 
  group_by(decade,statefp,statename) %>% 
  summarize(expend_pcpi_norm_decade = mean(expend_pcpi_norm))

# state-level climate; time-invariant

# 1. need to convert country names to lowercase
# 2. create overall ussr + sudan category by aggregating constituent states
# 3. change bosnia and herzegovina to yugoslavia, myanmar (burma) to burma

# rename countries for consistency with refarrivals data
climate_country_czone <- climate_country_czone %>% 
  mutate(country_name = tolower(country_name)) %>% 
  mutate(country_name = ifelse(country_name == 'bosnia & herzegovina', 'yugoslavia', country_name)) %>% 
  mutate(iso3 = ifelse(iso3 == 'BIH', 'YUG', iso3)) %>% 
  mutate(country_name = ifelse(country_name == 'myanmar (burma)', 'burma', country_name))

# composition of refs from ussr
ussr_comp <- read_dta("Data/Clean/refs_ussr_composition.dta")

# climate of ussr constructed by weighted sum of composition states
climate_ussr_czone <- climate_country_czone %>% 
  filter(country_name %in% ussr_comp$citizenship) %>% # matches all countries
  left_join(ussr_comp, join_by(country_name == citizenship)) %>% # matches all countries %>% 
  group_by(czone) %>% 
  summarize(across(temp_czone:precip_euclid_dist, ~ stats::weighted.mean(.x, w=freq))) %>% 
  mutate(iso3 = "SUN", country_name = "ussr")

# climate of sudan constructed by weighted sum of sudan and south sudan
climate_sudan_czone <- climate_country_czone %>% 
  filter(country_name %in% c('sudan','south sudan')) %>% 
  mutate(wt = ifelse(country_name == 'sudan', 48, 11)) %>% # population in 2023
  group_by(czone) %>% 
  summarize(across(temp_czone:precip_euclid_dist, ~ stats::weighted.mean(.x, w=wt))) %>% 
  mutate(iso3 = "SDN", country_name = "sudan")

climate_country_czone <- climate_country_czone %>% 
  filter(country_name %notin% c('sudan','south sudan')) %>% 
  bind_rows(climate_ussr_czone) %>% 
  bind_rows(climate_sudan_czone) # %>% rename(statename = state_name) # for conformity with other data

# state-level vars to czone-level: recover puma-level measures from state-level and then aggregate to czone-level

# # read David Dorn's ipums-commuting zone correspondence
# puma_state_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta") %>% 
#   filter(year == 1990) %>% select(-year) %>% 
#   mutate(puma_equiv = str_pad(puma_equiv, width = 6, pad = "0")) %>% 
#   mutate(statefp = str_sub(puma_equiv, start = 1, end = 2)) 
# 
# # climate
# 
# refugee_cntries <- c('afghanistan','burma','cambodia','cuba','ethiopia','iran','iraq','laos',
#                      'liberia','poland','romania','somalia','sudan','ussr','vietnam','yugoslavia')
# 
# # get puma-country level climate similarity 
# country_puma_czone_climate <- tidyr::crossing(country = refugee_cntries, puma_state_czone_cross)%>% 
#   left_join(state_country_climate, join_by(statefp, country==country_name)) %>% 
#   select(-iso3) %>% relocate(afactor, .after = everything())
# 
# # averages climate from multiple states where czone straddles state boundaries
# country_czone_climate <- country_puma_czone_climate %>% 
#   group_by(country,czone) %>% 
#   summarize(across(contains(c("precip","temp","humidity")), mean)) %>% 
#   select(country, czone, contains("dist"))

# czone-level unemployment, expenditure, voteshare calculated below

# additional data cleaning
# 
# # czone-level; nhgis/ipums; 1970-2000 census; ACS 5% 2010, 2015
# czone_characteristics %>% 
#   left_join(hp_czone) %>% 
#   left_join(wage_czone %>% select(-educ)) 
#   
# hp_czone <- read_dta("Data/Clean/hp_fixed_effects_czone_year_attempt2.dta") # last sample: 2011-2015 acs; contains 2005-2007 acs
# wage_czone <- read_dta("Data/Clean/wage_fixed_effects_czone_year.dta") # last sample: 2011-2015 acs; contains 2005-2007 acs
# 
# # state-level; annual; semi-annual 
# unemp_state <- read_dta("Data/Clean/state_unemp_rate_clean.dta")
# voteshare_state_senate <- read_dta("Data/Clean/state_voteshare_senate.dta")
# voteshare_state_presid <- read_dta("Data/Clean/state_voteshare_president.dta")
# expend_state <- read_dta("Data/Clean/state_revenue_expenditure.dta")

# czone population data

asia_cntries <- c("afghanistan", "burma", "cambodia", "iran", "iraq", "laos", "vietnam")
e_europ_cntries <- c("poland", "romania", "ussr", "yugoslavia")
afric_cntries <- c("ethiopia", "liberia", "somalia", "sudan")
s_americ_cntries <- "cuba"

refarrivals_cohort_czone_insample <- refarrivals_cohort_czone_insample %>%
  mutate(continent = case_match(citizenship_stable,
                                asia_cntries ~ "asia",
                                e_europ_cntries ~ "e_europ",
                                afric_cntries ~ "afric",
                                s_americ_cntries ~ "s_americ"))

refpop_cohort_czone_insample <- refpop_cohort_czone_insample %>%
  filter( (year == 1970 & cohort == 'pre1975') |
            (year == 1980 & cohort == '7580') | 
            (year == 1990 & cohort == '8090') |
            (year == 2000 & cohort == '9199') |
            (year == 2007 & cohort == '0004')) %>% 
  ungroup() %>% 
  # select(-year) %>% 
  arrange(cntryname,cohort)

#### end data cleaning ####

#### joining outcome and control vars ####

# for joining current cohort-level vars with lagged population vars

refpop_czone_ipums <- refpop_czone_ipums %>% 
  group_by(cntryname,czone) %>% 
  mutate(lagged_refpop_ipums = lag(refpop_ipums)) # %>% 
  # select(-year) %>% 
  # rename(lagged_refpop_ipums = refpop_ipums) %>% 
  # left_join(refpop_czone_ipums, join_by(lead_year==year,countryname,czone))

contpop_czone_ipums <- contpop_czone_ipums %>% 
  group_by(bpl_continent,czone) %>% 
  mutate(lagged_contpop_ipums = lag(contpop_ipums)) # %>% select(-year)

fbpop_czone_ipums <- fbpop_czone_ipums %>% 
  group_by(czone) %>% 
  mutate(lagged_fbpop_ipums = lag(fbpop_ipums)) # %>% select(-year)

totpop_czone_nhgis <- czone_characteristics %>% 
  select(year,czone,pop_tot) %>% 
  rename(totpop_nhgis = pop_tot) %>% 
  group_by(czone) %>% 
  mutate(lagged_totpop_nhgis = lag(totpop_nhgis)) # %>% select(-year)

# first two lines ensures only czones receiving refugees are retained in sample

outcome_var <- refarrivals_cohort_czone_insample %>% 
  filter(refarrivals_dreher > 0) %>% # analysis of intensive margin
  left_join(refpop_cohort_czone_insample, join_by(citizenship_stable==cntryname, cohort, czone, in_sample)) %>% 
  # filter(cohort %in% c('7580','8090','9199','0004')) %>% 
  group_by(cohort) %>% mutate(year = mean(year, na.rm = T)) %>% ungroup() %>% 
  mutate(refpop_ipums = ifelse(is.na(refpop_ipums), 0, refpop_ipums)) %>% 
  mutate(net_inflow = refpop_ipums - refarrivals_dreher)

origin_specific_control_vars <- outcome_var %>% 
  select(year,citizenship_stable,continent,czone) %>% 
  left_join(refpop_czone_ipums, join_by(year, citizenship_stable==cntryname, czone)) %>% 
  left_join(contpop_czone_ipums, join_by(year, continent==bpl_continent, czone)) %>% 
  left_join(fbpop_czone_ipums, join_by(year, czone)) %>% 
  left_join(totpop_czone_nhgis, join_by(year, czone)) %>% 
  mutate(across(contains("ipums"), ~replace_na(., 0))) %>% 
  left_join(climate_country_czone, join_by(citizenship_stable==country_name, czone))

origin_specific_control_vars <- origin_specific_control_vars %>% 
  mutate(resid_contpop_ipums = contpop_ipums - refpop_ipums,
         resid_fbpop_ipums = fbpop_ipums - contpop_ipums,
         resid_totpop_nhgis = totpop_nhgis - fbpop_ipums) %>% 
  mutate(resid_lagged_contpop = lagged_contpop_ipums - lagged_refpop_ipums,
         resid_lagged_fbpop = lagged_fbpop_ipums - lagged_contpop_ipums,
         resid_lagged_totpop = lagged_totpop_nhgis - lagged_fbpop_ipums)

control_vars_census <- czone_characteristics %>% 
  select(year,czone,pop_tot,pop_fb,pop_unemp,pop_ov25_4ycol,pcinc,medhhinc) %>% 
  left_join(hp_czone) %>% 
  left_join(wage_czone %>% filter(educ==1) %>% select(-educ)) %>% 
  mutate(decade = case_match(year, 1970 ~ '7580', 1980 ~ '8090', 1990 ~ '9199', 2000 ~ '0004')) # %>% select(-year)

# convert monetary values to 1999 dollars; comment out for nominal dollars
control_vars_census <- control_vars_census %>%
  left_join(cpi_1999base %>% select(year,cpi_1999base)) %>%
  mutate(across(c(pcinc, medhhinc,hp,wage), ~ .x / cpi_1999base))

control_vars_state <- unemp_state %>% 
  left_join(voteshare_state_presid) %>% 
  left_join(voteshare_state_senate) %>% 
  left_join(expend_state)

control_vars_census %>% 
  select(-pcinc,-medhhinc) %>% 
  ungroup() %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna > 0)

control_vars_state %>% 
  select(!contains('senate')) %>% 
  ungroup() %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna > 0)

# calculate czone-level measures of state-level variables
control_vars_state_czone <- control_vars_state %>% 
  left_join(puma_state_czone_cross) %>% 
  group_by(decade,czone) %>% 
  summarize(across(unemp_rate:expend_pcpi_norm_decade, ~stats::weighted.mean(., w = afactor))) %>% 
  select(!contains("republican"))

# rename vars for comparability

outcome_var <- outcome_var %>% 
  rename(origin=citizenship_stable, origin_continent=continent, refcohortpop_ipums=refpop_ipums)

control_vars_census <- control_vars_census %>% rename(cohort = decade)

control_vars_state_czone <- control_vars_state_czone %>% rename(cohort = decade)

origin_specific_control_vars <- origin_specific_control_vars %>% 
  rename(origin=citizenship_stable, origin_continent=continent,
         originpop_ipums = refpop_ipums, lagged_originpop_ipums = lagged_refpop_ipums)

# join outcome and all control vars
data <- outcome_var %>% 
  left_join(origin_specific_control_vars) %>% select(-year) %>% 
  left_join(control_vars_census %>% select(-year)) %>% 
  left_join(control_vars_state_czone)

data %>% 
  left_join(cpi_)
  mutate(across)

# join quality: only missing in pcinc, medhhinc, and senate voteshare for Washington DC

data %>% ungroup() %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna>0, cohort != '7580')

data %>% filter(cohort == '7580', czone != 11304) %>% 
  select(-pcinc,-medhhinc) %>% 
  ungroup() %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna>0)

#### end join data ####

# final cleaning
data <- data %>% 
  relocate(origin_continent, .after = origin) %>% 
  relocate(in_sample, .after = everything()) %>% 
  rename_with(~ gsub("ref", "", .x)) %>% 
  rename_with(~ gsub("_ipums", "", .x)) %>% 
  rename_with(~ gsub("_nhgis", "", .x))

# data <- data %>% 
#   mutate(cohort = factor(cohort, levels = c('7580','8090','9199','0004')))

write_dta(data, "Data/Clean/data_main_cohort_lvl_laggedpopcontrols.dta")
