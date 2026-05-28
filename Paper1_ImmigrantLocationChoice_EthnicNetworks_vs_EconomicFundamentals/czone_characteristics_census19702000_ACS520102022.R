rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(tigris)
library(haven)

source("Code/functions.R")

# Load county characteristics data
county_characteristics <- read_dta("Data/Clean/county_characteristics_census19702000_ACS520102022.dta")

# Load county-czone crosswalk 
county_cz_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_cty_czone/cw_cty_czone.dta")

county_characteristics <- county_characteristics %>%
  filter(statefp %notin% c("02","15","72")) %>% 
  mutate(fp = str_c(statefp,countyfp))

fp <- tibble(fp = unique(county_characteristics$fp))

fp_cz <- fp %>% 
  mutate(fp=as.numeric(fp)) %>% 
  left_join(county_cz_cross, join_by(fp == cty_fips))

fp_cz %>% filter(is.na(czone))

czone_characteristics <- county_characteristics %>% 
  mutate(fp=as.numeric(fp)) %>% 
  left_join(county_cz_cross, join_by(fp == cty_fips))

# Manual Corrections:
czone_characteristics$czone[czone_characteristics$fp==8014] <- 28900 # Broomfield County, Colorado
czone_characteristics$czone[czone_characteristics$fp==12086] <- 7000 # Miami-Dade County, Florida
czone_characteristics$czone[czone_characteristics$statefp == "09"] <- 20901 # All Connecticut
czone_characteristics$czone[czone_characteristics$fp==46102] <- 27704 # Shannon County, South Dakota
czone_characteristics$czone[czone_characteristics$fp==46131] <- 27603 # Washabough County, South Dakota
czone_characteristics$czone[czone_characteristics$fp==51123] <- 2000 # Nansemond County, merged into Suffolk

czone_characteristics %>% filter(is.na(czone)) # all matched

## get commuting zone-level characteristics 
## Note: land and water area are defined for 2010 geographies

# czones with missing characteristics because of separation from
# existing counties or merging with existing counties
cz_boundary_change <- c(2000, 2200, 2300, 2500, 7000, 11304, 17300,
                        20901, 27603, 27704, 28900, 34402, 34901, 38300)

missing <- czone_characteristics %>% 
  ungroup() %>% 
  select(!contains(c("rural","urban"))) %>% # rural/urban status not available after 2010
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(year > 1970, czone %notin% cz_boundary_change) %>% # some lab. market vars not available in 1970
  filter(numna > 0)

# small correction to rural/urban pop
czone_characteristics$pop_rural[czone_characteristics$fp==55001 & czone_characteristics$year==1970] <- czone_characteristics$pop_tot
czone_characteristics$pop_urban[czone_characteristics$fp==55001 & czone_characteristics$year==1970] <- 0

# counties with missing values
cz_missing_val <- c(unique(missing$czone))

# some vars I can simply add
czone_characteristics <- czone_characteristics %>% 
  # filter(!str_detect(county, "Region")) %>% 
  # filter(year < 2020) %>% 
  group_by(year, czone)

# For most czones, no need to remove NAs
cz_vars1 <- czone_characteristics %>% 
  filter(czone %notin% c(cz_boundary_change,cz_missing_val)) %>% 
  summarize(across(contains(c("pop","area","blwpov")), sum)) %>% 
  select(-popshare_rural,-pop_density)

# For boundary changes and other czones with few missing values, ignore NAs
cz_missing_val_vars1 <- czone_characteristics %>% 
  filter(czone %in% c(cz_boundary_change, cz_missing_val)) %>% 
  summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
  select(-popshare_rural,-pop_density)

# # Correction for missing value
# # assigned tot pop as rural b/c very low pop density (Wisconsin)
# cz_missing_val_vars1 <- czone_characteristics %>%
#   filter(czone==cz_missing_val) %>%
#   mutate(pop_rural = ifelse(fp == 55001 & year == 1970, pop_tot, pop_rural),
#          pop_urban = ifelse(fp == 55001 & year == 1970, 0, pop_urban)) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>%
#   select(-popshare_rural,-pop_density)

cz_vars1 <- cz_vars1 %>% 
  # bind_rows(cz_boundary_change_vars1) %>%
  bind_rows(cz_missing_val_vars1)

test1 <- cz_vars1 %>% ungroup() %>% mutate(numna = rowSums(across(everything(), is.na))) %>% filter(numna>0)

# others need to be pop weighted; weighting var = pop_tot
cz_vars2 <- czone_characteristics %>% 
  filter(year != 1970) %>% # all 3 vars below missing for 1970 %>% 
  filter(czone %notin% c(cz_boundary_change,cz_missing_val)) %>%
  group_by(year, czone) %>%
  summarize(pcinc = stats::weighted.mean(pcinc, w = pop_tot),
            medhhinc = stats::weighted.mean(medhhinc, w = pop_tot),
            medhp = stats::weighted.mean(medhp, w = pop_tot))

cz_vars2 %>% filter(is.na(pcinc)|is.na(medhhinc)|is.na(medhp))

cz_boundary_change_vars2 <- czone_characteristics %>% 
  filter(year != 1970) %>% # all 3 vars below missing for 1970 %>% 
  filter(czone %in% c(cz_boundary_change,cz_missing_val)) %>%
  group_by(year, czone) %>%
  summarize(pcinc = stats::weighted.mean(pcinc, w = pop_tot, na.rm = T),
            medhhinc = stats::weighted.mean(medhhinc, w = pop_tot, na.rm = T),
            medhp = stats::weighted.mean(medhp, w = pop_tot, na.rm = T))

cz_vars2 <- cz_vars2 %>% bind_rows(cz_boundary_change_vars2)

cz_vars2 %>% ungroup() %>% mutate(numna = rowSums(is.na(.))) %>% filter(numna > 0) # no missing vars

cz_vars <- cz_vars1 %>% 
  left_join(cz_vars2) %>% 
  mutate(across(c("land_area", "water_area","pcinc","medhhinc"), round, digits=2))

cz_vars <- cz_vars %>% 
  mutate(share_fb = pop_fb / (pop_nat + pop_fb)) %>% 
  mutate(pop_density = pop_tot / land_area) %>% 
  mutate(popshare_rural = pop_rural / (pop_rural + pop_urban)) %>% 
  mutate(share_unemp = pop_unemp / (pop_emp + pop_unemp)) %>% 
  mutate(share_ov25_4ycol = pop_ov25_4ycol / (pop_ov25_4ycol + pop_ov25_n4ycol))

cz_vars %>% 
  ungroup() %>% 
  select(!contains(c("rural","urban"))) %>% # rural/urban status not available after 2010
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(year > 1970,  czone %notin% cz_boundary_change) %>% # some lab. market vars not available in 1970
  filter(numna > 0)

cz_vars %>% 
  filter(year!= 1970) %>% 
  select(!contains(c('rural','urban'))) %>% 
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna > 0)

cz_vars %>% 
  filter(year == 1970) %>% 
  select(!c(medhp,medhhinc,pcinc)) %>% 
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna > 0)

cz_vars %>% 
  filter(year <= 2010) %>% 
  select(contains(c('rural','urban'))) %>% 
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna > 0)

write_dta(cz_vars, "Data/Clean/czone_characteristics_census19702000_ACS520102022.dta")

# cz2000_vars <- czone_characteristics %>% 
#   filter(czone==2000) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz2200_vars <- czone_characteristics %>% 
#   filter(czone==2200) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz2500_vars <- czone_characteristics %>% 
#   filter(czone==2500) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz7000_vars <- czone_characteristics %>% 
#   filter(czone==7000) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz17300_vars <- czone_characteristics %>% 
#   filter(czone==17300) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz22700_vars <- czone_characteristics %>%
#   filter(czone==22700) %>%
#   mutate(pop_rural = ifelse(fp == 55001 & year == 1970, pop_tot, pop_rural),
#          pop_urban = ifelse(fp == 55001 & year == 1970, 0, pop_urban)) %>% # assigned all pop as rural b/c very low pop density
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>%
#   select(-popshare_rural,-pop_density)
# 
# cz27603_vars <- czone_characteristics %>% 
#   filter(czone==27603) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz27704_vars <- czone_characteristics %>% 
#   filter(czone==27704) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz28900_vars <- czone_characteristics %>% 
#   filter(czone==28900) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz34402_vars <- czone_characteristics %>% 
#   filter(czone==34402) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz34901_vars <- czone_characteristics %>% 
#   filter(czone==34901) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)
# 
# cz38300_vars <- czone_characteristics %>% 
#   filter(czone==38300) %>% 
#   summarize(across(contains(c("pop","area","blwpov")), sum, na.rm = T)) %>% 
#   select(-popshare_rural,-pop_density)



