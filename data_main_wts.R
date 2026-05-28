rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# read data

refarrivals_origin_cohort_tot <- read_dta("Data/Clean/refugee_origin_cohorts_all_insample.dta")

wts <- refarrivals_origin_cohort_tot %>% 
  rename(popwt_origin_cohort_ipums = arrivals_ipums,
         popwt_origin_cohort_dreher = arrivals_dreher,
         origin = cntryname) %>% 
  select(-year,-in_sample) 

write_dta(wts, "Data/Clean/data_main_wts.dta")
