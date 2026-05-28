rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(tigris)
library(sf)

source("Code/functions.R")

# Load David Dorn's 1990 county to 1990 commuting zone crosswalk 
county_cz_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_cty_czone/cw_cty_czone.dta")

# 2010 County boundaries Shapefile
countysf <- counties(cb = TRUE, year = 2010) %>% 
  filter(!STATEFP %in% c("02", "15", "60", "66", "69", "72", "78")) %>% 
  rename_with(tolower)

customsf <- countysf %>% 
  mutate(fp = as.numeric(str_c(statefp,countyfp))) %>% 
  left_join(county_cz_cross, join_by(fp == cty_fips))

# Manual Corrections:
customsf$czone[customsf$fp==8014] <- 28900 # Broomfield County, Colorad
customsf$czone[customsf$fp==12086] <- 7000 # Miami-Dade County, Florida

customsf %>% filter(is.na(czone))
county_cz_cross %>% filter(cty_fips %notin% unique(customsf$fp)) %>% print(n=50) # mostly Alaska, Hawaii; Miami-Dade, 1 county in Virginia absorbed by other county, same for 2 South Dakota counties 

czonesf <- customsf %>% 
  group_by(czone) %>% 
  summarize(geometry=st_union(geometry))

saveRDS(czonesf, "Data/Clean/czone_sf.RDS")

