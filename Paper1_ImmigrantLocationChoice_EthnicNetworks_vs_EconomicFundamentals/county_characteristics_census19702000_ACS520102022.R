rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(tigris)
library(haven)

source("Code/functions.R")

## nhgis county-level characteristics

# total land + water area
us_counties <- counties(year = 2010)

us_counties_clean = tibble(
  statefp10 = us_counties$STATEFP10,
  countyfp10 = us_counties$COUNTYFP10,
  county = us_counties$NAMELSAD10,
  land_area = us_counties$ALAND10,
  water_area = us_counties$AWATER10) %>% 
  mutate(land_area = land_area / 1000000,
         water_area = water_area / 1000000) # convert to square km

# native and immigrant shares
nhgis_countypop_nat_imm <- read_csv("Data/Raw/nhgis/nhgis_countypop_nat_imm_1/nhgis0037_ts_nominal_county.csv")

# total pop + county area - for pop density
nhgis_countypop_tot_density <- read_csv("Data/Raw/nhgis/nhgis_countypop_tot_density_1/nhgis0038_ts_nominal_county.csv")

# rural-urban population
nhgis_countypop_urban_rural <- read_csv("Data/Raw/nhgis/nhgis_countypop_urban_rural_1/nhgis0020_ts_nominal_county.csv")

# urban-rural households
nhgis_countyhh_urban_rural <- read_csv("Data/Raw/nhgis/nhgis_county_hh_urban_rural_1/nhgis0019_ts_nominal_county.csv")

# labor force stats
nhgis_county_labforce_stats <- read_csv("Data/Raw/nhgis/nhgis_county_labforce_stats_1/nhgis0039_ts_nominal_county.csv")

# income stats - Hholds by income + median hhold income + per capita income + hholds below poverty level
# some but not all variables start from 1970
nhgis_county_income_stats <- read_csv("Data/Raw/nhgis/nhgis_county_income_stats_1/nhgis0040_ts_nominal_county.csv")

# education
nhgis_county_educ_stats <- read_csv("Data/Raw/nhgis/nhgis_county_educ_stats_1/nhgis0043_ts_nominal_county.csv")

years <- c("1970","1980","1990","2000","2010","2012","2015", "2022")
errmarg <- c("2010m","2012m","2015m","2022m") 

# pivoting the variables long

nhgis_countypop_nat_imm_long <- nhgis_countypop_nat_imm %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("105","2010", .)) %>% 
  rename_with(~ gsub("125","2012", .)) %>% 
  rename_with(~ gsub("155","2015", .)) %>%
  rename_with(~ gsub("225","2022", .)) %>% 
  select(gisjoin:countynh, contains(years)) %>% 
  select(!contains(errmarg)) %>% 
  pivot_longer(contains(years),
               names_to = c('.value','year'),
               names_pattern = "(.*)(\\d{4}+)")

nhgis_countypop_tot_density_long <- nhgis_countypop_tot_density %>% 
  rename_with(tolower) %>% 
  select(!contains("2010")) %>% 
  rename_with(~ gsub("105","2010", .)) %>% 
  rename_with(~ gsub("125","2012", .)) %>% 
  rename_with(~ gsub("155","2015", .)) %>% 
  rename_with(~ gsub("225","2022", .)) %>% 
  select(gisjoin:countynh, contains(years)) %>% 
  select(!contains(errmarg)) %>% 
  pivot_longer(contains(years),
               names_to = "year",
               names_pattern = ".*(\\d{4}+)",
               values_to = "pop_tot")

nhgis_countypop_urban_rural_long <- nhgis_countypop_urban_rural %>% 
  rename_with(tolower) %>% 
  pivot_longer(contains(years),
               names_to = c('.value','year'),
               names_pattern = "(.*)(\\d{4}+)")

nhgis_countyhh_urban_rural_long <- nhgis_countyhh_urban_rural %>% 
  rename_with(tolower) %>% 
  pivot_longer(contains(years),
               names_to = c('.value','year'),
               names_pattern = "(.*)(\\d{4}+)")
  
nhgis_county_labforce_stats_long <- nhgis_county_labforce_stats %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("105","2010", .)) %>% 
  rename_with(~ gsub("125","2012", .)) %>% 
  rename_with(~ gsub("155","2015", .)) %>% 
  rename_with(~ gsub("225","2022", .)) %>%
  select(gisjoin:countynh, contains(years)) %>% 
  select(!contains(errmarg)) %>% 
  pivot_longer(contains(years),
               names_to = c('.value','year'),
               names_pattern = "(.*)(\\d{4}+)")

nhgis_county_income_stats_long <- nhgis_county_income_stats %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("105","2010", .)) %>% 
  rename_with(~ gsub("125","2012", .)) %>% 
  rename_with(~ gsub("155","2015", .)) %>% 
  rename_with(~ gsub("225","2022", .)) %>%
  select(gisjoin:countynh, contains(years)) %>% 
  select(!contains(errmarg)) %>% 
  pivot_longer(contains(years),
               names_to = c('.value','year'),
               names_pattern = "(.*)(\\d{4}+)")

nhgis_county_educ_stats_long <- nhgis_county_educ_stats %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("105","2010", .)) %>% 
  rename_with(~ gsub("125","2012", .)) %>% 
  rename_with(~ gsub("155","2015", .)) %>% 
  rename_with(~ gsub("225","2022", .)) %>%
  select(gisjoin:countynh, contains(years)) %>% 
  select(!contains(errmarg)) %>% 
  pivot_longer(contains(years),
               names_to = c('.value','year'),
               names_pattern = "(.*)(\\d{4}+)")

## selecting vars of interest

# keeping all vars + creating foreign born share from pop_nat and pop_fb
nhgis_countypop_nat_imm_long_clean <- nhgis_countypop_nat_imm_long %>% 
  rename(pop_nat = at5aa, pop_fb = at5ab) %>% 
  mutate(share_fb = pop_fb / (pop_nat + pop_fb))

# creating population density by dividing tot pop by land area
nhgis_countypop_tot_density_long_clean <- nhgis_countypop_tot_density_long %>% 
  left_join(us_counties_clean %>% select(-county),
            join_by(statefp == statefp10, countyfp == countyfp10)) %>% 
  mutate(pop_density = pop_tot / land_area)

# dropping urban - inside vs outside urbanized areas
# keeping only urban vs rural
nhgis_countypop_urban_rural_long_clean <- nhgis_countypop_urban_rural_long %>% 
  select(-a57ab, -a57ac) %>% 
  rename(pop_urban = a57aa, pop_rural = a57ad) %>% 
  mutate(popshare_rural = pop_rural / (pop_rural + pop_urban))

# does not have urban - inside vs outside urbanized areas; not dropping any vars
nhgis_countyhh_urban_rural_long_clean <- nhgis_countyhh_urban_rural_long %>% 
  rename(hh_urban = a62aa, hh_rural = a62ab) %>% 
  mutate(hhshare_rural = hh_rural / (hh_rural + hh_urban))

# just keep employed, unemployed, and out of labforce for 16 and older, civilian
nhgis_county_labforce_stats_long_clean <- nhgis_county_labforce_stats_long %>% 
  select(gisjoin:year, b84ad, b84ae, b84af) %>% 
  rename(pop_emp = b84ad, pop_unemp = b84ae, pop_outlabforce = b84af) %>% 
  mutate(share_unemp = pop_unemp / (pop_emp + pop_unemp))
  
# keep per capita income, median hh income, and persons below poverty line
nhgis_county_income_stats_long_clean <- nhgis_county_income_stats_long %>% 
  select(gisjoin:year, b79aa, bd5aa, cl6aa) %>% 
  rename(pcinc = b79aa, medhhinc = bd5aa, blwpov = cl6aa)
  
# create pop over 25 with than 4 year college, 
# 4 year college and more, 
# and 4 year college share of pop over 25 
nhgis_county_educ_stats_long_clean <- nhgis_county_educ_stats_long %>% 
  mutate(pop_ov25_n4ycol = b69aa + b69ab) %>% 
  mutate(pop_ov25_4ycol = b69ac) %>% 
  select(-b69aa,-b69ab,-b69ac) %>% 
  mutate(share_ov25_4ycol = pop_ov25_4ycol / (pop_ov25_4ycol + pop_ov25_n4ycol))

county_characteristics <- nhgis_countypop_nat_imm_long_clean %>% 
  left_join(nhgis_countypop_tot_density_long_clean) %>% 
  left_join(nhgis_countypop_urban_rural_long_clean) %>% 
  left_join(nhgis_countyhh_urban_rural_long_clean) %>% 
  left_join(nhgis_county_labforce_stats_long_clean) %>% 
  left_join(nhgis_county_income_stats_long_clean) %>% 
  left_join(nhgis_county_educ_stats_long_clean)
  
# write_dta(county_characteristics, "Data/Clean/county_characteristics_census19702000_ACS520102022.dta")

#### Median County House Prices
nhgis_county_medhp_1980 <- read_csv("Data/Raw/nhgis/nhgis_county_medhp/nhgis0023_ds104_1980_county.csv")
nhgis_county_medhp_1990 <- read_csv("Data/Raw/nhgis/nhgis_county_medhp/nhgis0023_ds120_1990_county.csv")
nhgis_county_medhp_2000 <- read_csv("Data/Raw/nhgis/nhgis_county_medhp/nhgis0023_ds151_2000_county.csv")
nhgis_county_medhp_2012 <- read_csv("Data/Raw/nhgis/nhgis_county_medhp/nhgis0024_ds191_20125_county.csv")
nhgis_county_medhp_2010 <- read_csv("Data/Raw/nhgis/nhgis_county_medhp/nhgis0042_ds176_20105_county.csv")
nhgis_county_medhp_2015 <- read_csv("Data/Raw/nhgis/nhgis_county_medhp/nhgis0042_ds215_20155_county.csv")
nhgis_county_medhp_2022 <- read_csv("Data/Raw/nhgis/nhgis_county_medhp/nhgis0044_ds262_20225_county.csv")


nhgis_county_medhp_1980_c <- nhgis_county_medhp_1980 %>%
  rename_with(tolower) %>%
  select(state,statea,county,countya,c8o001) %>%
  rename(medhp = c8o001) %>%
  mutate(year=1980)

nhgis_county_medhp_1990_c <- nhgis_county_medhp_1990 %>%
  rename_with(tolower) %>%
  select(state,statea,county,countya,es6001) %>%
  rename(medhp = es6001) %>%
  mutate(year=1990)

nhgis_county_medhp_2000_c <- nhgis_county_medhp_2000 %>%
  rename_with(tolower) %>%
  select(state,statea,county,countya,gbg001) %>%
  rename(medhp = gbg001) %>%
  mutate(year=2000)

nhgis_county_medhp_2010_c <- nhgis_county_medhp_2010 %>%
  rename_with(tolower) %>%
  select(state,statea,county,countya,jsze001) %>%
  rename(medhp = jsze001) %>%
  mutate(medhp = as.numeric(medhp)) %>%
  mutate(year=2010)

nhgis_county_medhp_2012_c <- nhgis_county_medhp_2012 %>%
  rename_with(tolower) %>%
  select(state,statea,county,countya,qzne001) %>%
  rename(medhp = qzne001) %>%
  mutate(medhp = as.numeric(medhp)) %>%
  mutate(year=2012)

nhgis_county_medhp_2015_c <- nhgis_county_medhp_2015 %>%
  rename_with(tolower) %>%
  select(state,statea,county,countya,adree001) %>%
  rename(medhp = adree001) %>%
  mutate(year=2015)

nhgis_county_medhp_2022_c <- nhgis_county_medhp_2022 %>%
  rename_with(tolower) %>%
  select(state,statea,county,countya,aqume001) %>%
  rename(medhp = aqume001) %>%
  mutate(year=2022)

nhgis_county_medhp <- nhgis_county_medhp_1980_c %>%
  bind_rows(nhgis_county_medhp_1990_c) %>%
  bind_rows(nhgis_county_medhp_2000_c) %>%
  bind_rows(nhgis_county_medhp_2010_c) %>%
  bind_rows(nhgis_county_medhp_2012_c) %>%
  bind_rows(nhgis_county_medhp_2015_c) %>%
  bind_rows(nhgis_county_medhp_2022_c) %>%
  relocate(year, .before = state) %>%
  mutate(year = as.character(year))

county_characteristics <- county_characteristics %>%
  left_join(nhgis_county_medhp %>% select(!c(state,county)),
            join_by(year, statefp == statea, countyfp == countya)) %>%
  relocate(medhp, .after = medhhinc)

write_dta(county_characteristics, "Data/Clean/county_characteristics_census19702000_ACS520102022.dta")
