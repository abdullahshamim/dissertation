rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# read refarrivals by cohort from dreher 23
refarrivals_cohort_czone_dreher <- read_dta("Data/Clean/refugee_arrivals_cohort_czone_dreher23.dta")

refarrivals_cohort_us_dreher <- refarrivals_cohort_czone_dreher %>% 
  group_by(cohort,citizenship_stable) %>% 
  summarize(refugees = sum(refugees))

# read refpop by cohort from ipums data
refpop_cohort_czone_ipums <- read_dta("Data/Clean/refugee_pop_cohort_czone_ipums.dta") %>% 
  mutate(cntryname = tolower(cntryname))

refpop_cohort_us_ipums <- refpop_cohort_czone_ipums %>%
  group_by(year,cohort,cntryname) %>%
  summarize(refpop_ipums=sum(refpop_ipums)) %>% 
  filter( (year == 1970 & cohort == 'pre1975') |
            (year == 1980 & cohort == '7580') | 
            (year == 1990 & cohort == '8090') |
            (year == 2000 & cohort == '9199') |
            (year == 2007 & cohort == '0004')) %>% 
  ungroup() %>% 
  # select(-year) %>% 
  arrange(cntryname,cohort)

refprob_cohort <- refpop_cohort_us_ipums %>% 
  left_join(refarrivals_cohort_us_dreher, join_by(cohort, cntryname==citizenship_stable)) %>% 
  rename(arrivals_ipums = refpop_ipums, arrivals_dreher = refugees) %>% 
  mutate(refprob = arrivals_dreher / arrivals_ipums)

refcohorts_all_insample <- refprob_cohort %>% 
  mutate(across(c(arrivals_dreher,refprob), ~replace_na(., 0))) %>% 
  mutate(in_sample = ifelse(arrivals_dreher > 15000 & refprob > 0.7, 1, 0))

refcohorts_all_insample_2 <- refprob_cohort %>% 
  mutate(across(c(arrivals_dreher,refprob), ~replace_na(., 0))) %>% 
  mutate(in_sample = ifelse(refprob > 0.7, 1, 0))

# for more intuitive arrangement
refcohorts_all_insample <- refcohorts_all_insample %>% 
  # mutate(cohort=factor(cohort,levels=c('pre1975','7580','8090','9199','0004'))) %>% 
  arrange(cntryname,cohort)

refcohorts_insample <- refcohorts_all_insample %>% 
  filter(in_sample == 1) %>% select(-in_sample)

# table showing origins and cohorts in my sample
# along with arrivals as per Dreher23, ipums, and the refugee probability
knitr::kable(refcohorts_all_insample %>% 
               filter(in_sample==1) %>% 
               select(-year),
             format = 'latex', booktabs = T)

# the sample selection criteria retains over 61 percent of all refugees in the time period I study (arrivals b/w 1975-2004) 
sum(refcohorts_insample$arrivals_dreher) / sum(refcohorts_all_insample$arrivals_dreher)

# save sample selection origin and cohort
write_dta(refcohorts_insample, "Data/Clean/refugee_origin_cohorts_insample.dta")
write_dta(refcohorts_all_insample, "Data/Clean/refugee_origin_cohorts_all_insample.dta")


# ussr, sudan

# data_refugee <- read_dta("Data/Clean/data_ipums_refugee.dta")
# 
# refarrivals_us <- read_dta("Data/Clean/refugee_arrivals_czone_dreher23.dta") %>%
#   group_by(citizenship_stable,year) %>%
#   summarize(refugees=sum(refugees))
# 
# test_ussr_sudan <- data_refugee %>%
#   filter(cntryname %in% c('Ussr',"Sudan"), year == 2000, yrimmig %in% 1985:2000) %>%
#   group_by(year,cntryname,yrimmig) %>%
#   summarize(arrivals_ipums = sum(perwt))
# 
# refarrivals_ussr_sudan <- refarrivals_us %>%
#   filter(year %in% 1985:2000, citizenship_stable %in% c('ussr','sudan')) %>%
#   mutate(citizenship_stable = str_to_title(citizenship_stable))
# 
# # 1988-1994 ussr arrivals around 80% refugees (note that refugee arrivals data for 1990 is undercounted due to missing data);
# # over 4000 sudanese refugees in 2000 as per dreher23 but only 1000 as per ipums
# 
# ussr_sudan_prob <- test_ussr_sudan %>%
#   left_join(refarrivals_ussr_sudan, join_by(yrimmig==year, cntryname==citizenship_stable)) %>%
#   ungroup() %>% select(-year) %>%
#   rename(arrivals_dreher=refugees) %>%
#   mutate(refprob = arrivals_dreher / arrivals_ipums)
# 
# ussr_sudan_prob %>% 
#   filter(yrimmig %in% 1988:1994, cntryname == "Ussr") %>% 
#   summarize(arrivals_dreher = sum(arrivals_dreher),
#             arrivals_ipums = sum(arrivals_ipums)) %>% 
#   mutate(refprob = arrivals_dreher / arrivals_ipums)

