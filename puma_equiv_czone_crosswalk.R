rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(ipumsr)
library(fixest)

# read raw data

cntygrp70_czone_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_ctygrp1970_czone/cw_ctygrp1970_czone_corr.dta")
cntygrp80_czone_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_ctygrp1980_czone/cw_ctygrp1980_czone_corr.dta")
puma90_czone_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_puma1990_czone/cw_puma1990_czone.dta") 
puma00_czone_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_puma2000_czone/cw_puma2000_czone.dta") 
puma10_czone_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_puma2010_czone/cw_puma2010_czone.dta")

cntygrp70_czone_cross <- cntygrp70_czone_cross %>% 
  mutate(year = 1970) %>% rename(puma_equiv = cty_grp70, afactor = afact)

cntygrp80_czone_cross <- cntygrp80_czone_cross %>% 
  mutate(year = 1980) %>% rename(puma_equiv = ctygrp1980)

puma90_czone_cross <- puma90_czone_cross %>% 
  mutate(year = 1990) %>% rename(puma_equiv = puma1990)

puma00_czone_cross <- puma00_czone_cross %>% 
  mutate(year = 2000) %>% rename(puma_equiv = puma2000)

# to match 2007 ACS
puma07_czone_cross <- puma00_czone_cross %>% 
  mutate(year = 2007)

# to match 2015 ACS
puma15_czone_cross <- puma10_czone_cross %>% 
  mutate(year = 2015) %>% rename(puma_equiv = puma2010)

puma_equiv_czone_cross <- cntygrp70_czone_cross %>% 
  bind_rows(cntygrp80_czone_cross) %>% 
  bind_rows(puma90_czone_cross) %>% 
  bind_rows(puma00_czone_cross) %>% 
  bind_rows(puma07_czone_cross) %>% 
  bind_rows(puma15_czone_cross) %>% 
  relocate(year, .before = everything()) %>% 
  arrange(year, puma_equiv)

write_dta(puma_equiv_czone_cross, "Data/Clean/puma_equiv_czone_crosswalk.dta")
