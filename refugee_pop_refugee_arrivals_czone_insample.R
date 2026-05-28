# get arrivals from dreher and ref stocks from ipums at czone-origin-cohort level; 
# keep origin-cohort pairs satisfying sample selection (refprob > 0.7 and at least 15,000 arrivals)

rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# refugee origin and cohorts in satisfying sample selection 
refcohorts_insample <- read_dta("Data/Clean/refugee_origin_cohorts_all_insample.dta") 
  
# number of people by arrival cohort from 16 ref origin countries
refpop_cohort_czone_ipums <- read_dta("Data/Clean/refugee_pop_cohort_czone_ipums.dta") %>% 
  mutate(cntryname = tolower(cntryname))

# dreher refugee arrivals at czone
refarrivals_cohort_czone <- read_dta("Data/Clean/refugee_arrivals_cohort_czone_dreher23.dta")

# refpop in czone for cohort-origin satisfying sample selection criteria
refpop_czone_insample <- refpop_cohort_czone_ipums %>% 
  left_join(refcohorts_insample) # %>% filter(in_sample == 1)

# refarrival to czone for cohort-origin satisfying sample selection criteria
refarrivals_czone_insample <- refarrivals_cohort_czone %>% 
  left_join(refcohorts_insample, join_by(cohort, citizenship_stable == cntryname)) # %>% filter(in_sample == 1)
  
refpop_czone_insample %>% filter(is.na(in_sample))
refarrivals_czone_insample %>% filter(is.na(in_sample))

# prepare data for saving
refpop_czone_simple_insample <- refpop_czone_insample %>% 
  select(year,cohort,cntryname,czone,refpop_ipums,in_sample) %>% 
  mutate(in_sample = ifelse(is.na(in_sample), 0, in_sample))

refarrivals_czone_simple_insample <- refarrivals_czone_insample %>% 
  select(citizenship_stable,cohort,czone,refarrivals_dreher=refugees, in_sample) %>% 
  mutate(in_sample = ifelse(is.na(in_sample), 0, in_sample))

# save
write_dta(refpop_czone_simple_insample, "Data/Clean/refugee_pop_cohort_czone_insample.dta")
write_dta(refarrivals_czone_simple_insample, "Data/Clean/refugee_arrivals_cohort_czone_insample.dta")

# # older code
# 
# # dreher refugee arrivals at czone
# refarrivals_czone1 <- read_dta("Data/Clean/refugee_arrivals_czone_dreher23.dta")
# 
# # ref arrivals (dreher) in czone for cohort-origin satisfying sample selection
# refarrivals_cohort_czone1 <- refarrivals_czone1 %>%
#   group_by(year,citizenship_stable,czone) %>%
#   summarize(refugees = sum(refugees)) %>%
#   group_by(citizenship_stable,czone) %>%
#   mutate(cum_refugees = cumsum(refugees))
# 
# refarrivals_cohort_czone1 <- refarrivals_cohort_czone1 %>%
#   filter(year %in% c(1979,1990,1999,2004)) %>% # cohort ending years
#   group_by(citizenship_stable, czone) %>%
#   mutate(refugees = cum_refugees-lag(cum_refugees, default = 0)) %>% # cohort level arrivals
#   bind_rows(refarrivals_cohort_czone1 %>% filter(year == 1980) %>% mutate(refugees = cum_refugees)) %>%
#   filter(year != 1979) %>%
#   mutate(cohort = case_match(year,
#                              1980 ~ "7580",
#                              1990 ~ "8090",
#                              1999 ~ "9199",
#                              2004 ~ "0004"), .after = year) %>%
#   select(-year,-cum_refugees) %>%
#   arrange(citizenship_stable, cohort)
# 
# refarrivals_cohort_czone1 <- refarrivals_cohort_czone1 %>%
#   group_by(citizenship_stable,cohort,czone) %>%
#   summarize(refugees=sum(refugees))
