rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

county_cz_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_cty_czone/cw_cty_czone.dta")
climate_county <- read_dta('Data/Clean/climate_county.dta')


climate_county <- climate_county %>% 
  mutate(fp = as.numeric(fp)) %>% 
  mutate(fp = ifelse(fp==12086, 12025, fp)) %>% 
  mutate(fp = ifelse(fp==46102, 46113, fp))

climate_county_czone <- climate_county %>% 
  left_join(county_cz_cross, join_by(fp == cty_fips)) %>% 
  mutate(czone = ifelse(fp==8014, 28900, czone))

climate_county_czone %>% filter(is.na(czone)) # all matched

climate_czone <- climate_county_czone %>% 
  group_by(year, month, czone) %>% 
  summarize(precip = sum(precip), 
            across(contains(c('temp', 'days')), mean))

write_dta(climate_czone, 'Data/Clean/climate_czone.dta')
