rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# read data
climate_country_czone <- read_dta("Data/Clean/climate_distance_country_czone.dta")
ussr_comp <- read_dta("Data/Clean/refs_ussr_composition.dta")

# state-level climate; time-invariant

# 1. need to convert country names to lowercase
# 2. create overall ussr + sudan category by aggregating constituent states
# 3. change bosnia and herzegovina to yugoslavia, myanmar (burma) to burma

# rename vars for consistency with other data

climate_country_czone <- climate_country_czone %>% 
  mutate(country_name = tolower(country_name)) %>% 
  mutate(country_name = ifelse(country_name == 'bosnia & herzegovina', 'yugoslavia', country_name)) %>% 
  mutate(iso3 = ifelse(iso3 == 'BIH', 'YUG', iso3)) %>% 
  mutate(country_name = ifelse(country_name == 'myanmar (burma)', 'burma', country_name)) %>% 
  rename(origin = country_name) %>% 
  rename_with( ~gsub('country', 'origin', .))

ussr_comp <- ussr_comp %>% rename(origin=citizenship)

# climate of ussr constructed by weighted sum of composition states
climate_ussr_czone <- climate_country_czone %>% 
  filter(origin %in% ussr_comp$origin) %>% # matches all countries
  left_join(ussr_comp) %>% # matches all countries %>% 
  group_by(czone) %>% 
  summarize(across(temp_czone:precip_euclid_dist, ~ stats::weighted.mean(.x, w=freq))) %>% 
  mutate(iso3 = "SUN", origin = "ussr", .before = czone)

# climate of sudan constructed by weighted sum of sudan and south sudan
climate_sudan_czone <- climate_country_czone %>% 
  filter(origin %in% c('sudan','south sudan')) %>% 
  mutate(wt = ifelse(origin == 'sudan', 48, 11)) %>% # population (in millions) in 2023
  group_by(czone) %>% 
  summarize(across(temp_czone:precip_euclid_dist, ~ stats::weighted.mean(.x, w=wt))) %>% 
  mutate(iso3 = "SDN", origin = "sudan", .before = czone)

climate_country_czone <- climate_country_czone %>% 
  filter(origin %notin% c('sudan','south sudan')) %>% 
  bind_rows(climate_ussr_czone) %>% 
  bind_rows(climate_sudan_czone) %>% select(-iso3)

# climate of czechoslovakia constructed by weighted sum of czech republic and slovakia
climate_czechoslovakia_czone <- climate_country_czone %>% 
  filter(origin %in% c('czechia', 'slovakia')) %>% 
  mutate(wt = ifelse(origin == 'czechia', 2, 1)) %>% # population (in millions) in 2023
  group_by(czone) %>% 
  summarize(across(temp_czone:precip_euclid_dist, ~ stats::weighted.mean(.x, w=wt))) %>% 
  mutate(iso3 = "CSK", origin = "czechoslovakia", .before = czone)

climate_country_czone <- climate_country_czone %>% 
  filter(origin %notin% c('sudan','south sudan')) %>% 
  bind_rows(climate_ussr_czone) %>% 
  bind_rows(climate_sudan_czone) %>% 
  bind_rows(climate_czechoslovakia_czone) %>% select(-iso3)

write_dta(climate_country_czone, 'Data/Clean/data_main_origin_czone_climate_euclid_dist.dta')
