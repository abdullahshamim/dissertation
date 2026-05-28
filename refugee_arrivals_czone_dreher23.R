rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(tigris)
library(haven)
library(fixest)
library(rlang)
library(halfmoon)
library(stargazer)

source("Code/functions.R")

#### older code ####

# states <- tigris::states(year = 2010) %>% 
#   rename_with(tolower) %>% 
#   select(name10, statefp10) %>% 
#   rename(state10name = name10) %>% 
#   as_tibble()
# 
# # 16 countries that represent over 95% of arrivals before 2009
# refcountries16 <- c(
#   'afghanistan', 'burma', 'cambodia', 'cuba', 'ethiopia', 'iran', 'iraq', 'laos', 'liberia', 
#   'poland', 'romania', 'somalia', 'sudan', 'ussr', 'vietnam', 'yugoslavia'
# )
# 
# # state fips codes: 02 = Alaska, 08 = Colorado, 12 = Florida, 15 = Hawaii, 72 = Puerto Rico
# 
# # Load Dreher et al (2023) data
# refarrivals_raw <- read_dta("Data/Raw/RefArrivals_dreher-langlotz-matzat-parsons/orr_prm_1975_2018_v1.dta")
# 
# # Load State-BEA Economic Region crosswalk
# state_region_cross <- read_csv("Data/Raw/Geography_Crosswalks/state_fips_bea_regions.csv") %>% 
#   rename(region = bea_region)
# 
# # Load David Dorn's 1990 county to 1990 commuting zone crosswalk 
# county_cz_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_cty_czone/cw_cty_czone.dta")
# 
# # filter year, origin, state
# refarrivals_raw <- refarrivals_raw %>% 
#   filter(citizenship_stable %in% refcountries16,
#          year <= 2008,
#          statefp10 %notin% c("02","15","72")
#          )
# 
# # check that fips identify unique county names
# refarrivals_raw %>% 
#   expand(nesting(statefp10,countyfp10,county10name)) %>% 
#   count(statefp10,countyfp10) %>% filter(n!=1) # fips codes identify unique county names
# 
# # save state and county names for fips codes; for joining with cleaned data
# state_county_names <- refarrivals_raw %>% 
#   expand(nesting(statefp10,countyfp10,county10name)) %>% 
#   left_join(states) %>% 
#   select(state10name,statefp10,county10name,countyfp10)
# 
# # select relevant vars
# refarrivals_concise <- refarrivals_raw %>%
#   left_join(states) %>% 
#   select(year,citizenship_stable,refugees,countyfp10, statefp10)
# 
# # Find first year each origin appears
# first_year_by_origin <- refarrivals_concise %>%
#   group_by(citizenship_stable, statefp10, countyfp10) %>%
#   summarize(min_year = min(year), .groups = "drop")
# 
# # Then join it before expanding and filter
# refarrivals_county <- refarrivals_concise %>%
#   expand(nesting(citizenship_stable, statefp10, countyfp10), year) %>%
#   left_join(refarrivals_concise, by = c("citizenship_stable", "countyfp10", "statefp10", "year")) %>%
#   left_join(first_year_by_origin) %>%
#   filter(year >= min_year) %>%
#   mutate(refugees = replace_na(refugees, 0)) %>%
#   select(-min_year)
# 
# # aggregate county-level arrivals
# refarrivals_county <- refarrivals_county %>%
#   group_by(year,statefp10, countyfp10, citizenship_stable) %>%
#   summarize(refugees = sum(refugees)) %>% 
#   left_join(state_county_names)
# 
# # save
# write_dta(refarrivals_county, "Data/Clean/refugee_arrivals_1975_2018_county10.dta")

#### end older code ####

# Load Dreher et al (2023) data
refarrivals_county <- read_dta("Data/Clean/refugee_arrivals_county_dreher23.dta")

# Load State-BEA Economic Region crosswalk
state_region_cross <- read_csv("Data/Raw/Geography_Crosswalks/state_fips_bea_regions.csv") %>%
  rename(region = bea_region)

# Load David Dorn's 1990 county to 1990 commuting zone crosswalk
county_cz_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_cty_czone/cw_cty_czone.dta")


# which counties do not match with czones?
refarrivals_county <- refarrivals_county %>% 
  mutate(fp10 = str_c(statefp10,countyfp10))

fp10 <- tibble(fp10 = unique(refarrivals_county$fp10))

fp10_cz <- fp10 %>% 
  mutate(fp10=as.numeric(fp10)) %>% 
  left_join(county_cz_cross, join_by(fp10 == cty_fips))

fp10_cz %>% filter(is.na(czone))


# join commuting zone geography
refarrivals_county_czone <- refarrivals_county %>% 
  mutate(fp10=as.numeric(fp10)) %>% 
  left_join(county_cz_cross, join_by(fp10 == cty_fips))

# Manual Corrections:
refarrivals_county_czone$czone[refarrivals_county_czone$fp10==8014] <- 28900 # Broomfield County, Colorado
refarrivals_county_czone$czone[refarrivals_county_czone$fp10==12086] <- 7000 # Miami-Dade County, Florida

refarrivals_county_czone %>% filter(is.na(czone)) # all counties matched to 1990 commuting zone

refarrivals_county_czone_region <- refarrivals_county_czone %>% 
  left_join(state_region_cross, join_by(statefp10==statefp))

# 41 czones matched to multiple states
refarrivals_county_czone_region %>% ungroup() %>% expand(nesting(region,czone)) %>% count(czone) %>% filter(n!=1)

# 114 czones matched to multiple states
refarrivals_county_czone_region %>% ungroup() %>% expand(nesting(statefp10,czone)) %>% count(czone) %>% filter(n!=1)


# assign region rank in case of ties
czone_mutiple_regions <- refarrivals_county_czone_region %>% ungroup() %>% 
  expand(nesting(region,czone)) %>%
  add_count(czone, name = "n_regions") %>%
  filter(n_regions > 1) %>% 
  mutate(region_rank = case_match(region,
    "Great Lakes" ~ 8,
    "Far West" ~ 7,
    "Southwest" ~ 6,
    "Rocky Mountain" ~ 1,
    "Plains" ~ 2,
    "Southeast" ~ 3,
    "Mideast" ~ 5,
    "New England" ~ 4
  ))

# assign region for czones in spanning multiple regions
czone_mutiple_regions <- czone_mutiple_regions %>% group_by(czone) %>% 
  mutate(assigned_region_rank = max(region_rank)) %>% 
  select(-region, -region_rank, -n_regions) %>% 
  distinct() %>% 
  left_join(czone_mutiple_regions %>% select(region, region_rank) %>% distinct(),
            join_by(assigned_region_rank == region_rank)) %>% 
  rename(assigned_region = region) %>% 
  select(czone, assigned_region)

# # Tentative Rule
# Tie b/w Great Lakes, Mideast, Southeast -> Great Lakes
# Tie b/w Plains, Southeast, Southwest -> Southwest
# Tie b/w New England, Mideast -> Mideast
# Tie b/w Plains, Southeast -> Southeast
# Tie b/w Plains, Rocky Mountains -> Plains
# Tie b/w Rocky Mountain, Southwest -> Southwest
# Tie b/w Rocky Mountain, Far West -> Far West
# Tie b/w Southwest, Far West -> Far West

refarrivals_county_czone_region <- refarrivals_county_czone_region %>% 
  left_join(czone_mutiple_regions, join_by(czone)) %>% 
  mutate(assigned_region = ifelse(is.na(assigned_region), region, assigned_region))

refarrivals_county_czone_region %>% 
  ungroup() %>% 
  expand(nesting(assigned_region,czone)) %>% 
  count(czone) %>% filter(n!=1) # all czones assigned to exactly 1 region

refarrivals_czone_region <- refarrivals_county_czone_region %>% 
  group_by(year,citizenship_stable,czone,assigned_region) %>%
  summarize(refugees = sum(refugees)) %>% 
  rename(region=assigned_region)

refarrivals_czone_state_region <- refarrivals_county_czone_region %>% 
  group_by(year,citizenship_stable,statefp10,state10name,czone,assigned_region) %>%
  summarize(refugees = sum(refugees)) %>% 
  rename(region=assigned_region)

refarrivals_czone_region %>% ungroup %>% mutate(numna = rowSums(is.na(.))) %>% filter(numna>0)
refarrivals_czone_state_region %>% ungroup %>% mutate(numna = rowSums(is.na(.))) %>% filter(numna>0)

write_dta(refarrivals_czone_region, "Data/Clean/refugee_arrivals_czone_dreher23.dta")
# write_dta(refarrivals_czone_state_region, "Data/Clean/refugee_arrivals_czone_state_dreher23.dta")

