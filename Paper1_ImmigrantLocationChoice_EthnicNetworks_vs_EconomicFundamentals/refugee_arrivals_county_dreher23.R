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

states <- states <- readRDS("Data/Clean/state_sf.RDS") %>% select(state10name,statefp10)

# # 16 countries that represent over 95% of arrivals before 2009
# refcountries16 <- c(
#   'afghanistan', 'burma', 'cambodia', 'cuba', 'ethiopia', 'iran', 'iraq', 'laos', 'liberia',
#   'poland', 'romania', 'somalia', 'sudan', 'ussr', 'vietnam', 'yugoslavia'
# )

# state fips codes: 02 = Alaska, 08 = Colorado, 12 = Florida, 15 = Hawaii, 72 = Puerto Rico

# Load Dreher et al (2023) data
refarrivals_raw <- read_dta("Data/Raw/RefArrivals_dreher-langlotz-matzat-parsons/orr_prm_1975_2018_v1.dta")

# arrange refugee arrival countries by number of arrivals
topref_countries <- refarrivals_raw %>% 
  filter(year <= 2008) %>% 
  group_by(citizenship_stable) %>% 
  summarize(refugees = sum(refugees)) %>% 
  arrange(desc(refugees))

# top 16 arrival countries
refcountries16 <- topref_countries$citizenship_stable[1:16] # identical to above

# top 16 countries represent over 96 percent of all arrivals b/w 1975-2008
sum(topref_countries$refugees[1:16])/sum(topref_countries$refugees)

# filter year, origin, state
refarrivals_raw <- refarrivals_raw %>% 
  filter(citizenship_stable %in% refcountries16,
         year <= 2008,
         statefp10 %notin% c("02","15","72")
  )

# check that fips identify unique county names
refarrivals_raw %>% 
  expand(nesting(statefp10,countyfp10,county10name)) %>% 
  count(statefp10,countyfp10) %>% filter(n!=1) # fips codes identify unique county names

# save state and county names for fips codes; for joining with cleaned data
state_county_names <- refarrivals_raw %>% 
  expand(nesting(statefp10,countyfp10,county10name)) %>% 
  left_join(states) %>% 
  select(state10name,statefp10,county10name,countyfp10)

# select relevant vars
refarrivals_concise <- refarrivals_raw %>%
  left_join(states) %>% 
  select(year,citizenship_stable,refugees,countyfp10, statefp10)

# Find first year each origin appears
first_year_by_origin <- refarrivals_concise %>%
  group_by(citizenship_stable, statefp10, countyfp10) %>%
  summarize(min_year = min(year), .groups = "drop")

# Expand year but filter out years before first arrival
refarrivals_county <- refarrivals_concise %>%
  expand(nesting(citizenship_stable, statefp10, countyfp10), year) %>%
  left_join(refarrivals_concise, by = c("citizenship_stable", "countyfp10", "statefp10", "year")) %>%
  left_join(first_year_by_origin) %>%
  filter(year >= min_year) %>%
  mutate(refugees = replace_na(refugees, 0)) %>%
  select(-min_year)

# aggregate county-level arrivals
refarrivals_county <- refarrivals_county %>%
  group_by(year,statefp10, countyfp10, citizenship_stable) %>%
  summarize(refugees = sum(refugees)) %>% 
  left_join(state_county_names)

refarrivals_county <- refarrivals_county %>% relocate(c(state10name, county10name), .after = year)

# save
write_dta(refarrivals_county, "Data/Clean/refugee_arrivals_county_dreher23.dta")
