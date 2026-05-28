rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(haven)
library(tidyverse)

source("Code/functions.R")

climate_country <- read_dta('Data/Clean/climate_country.dta')
climate_czone <- read_dta('Data/Clean/climate_czone.dta')

climate_country_simple <- climate_country %>%
  group_by(iso3, country_name) %>%
  summarize(across(c(temp, temp_range, precip), mean)) %>%
  rename(precip_country = precip, temp_range_country = temp_range, temp_country = temp) %>%
  mutate(key = 1)

climate_czone_simple <- climate_czone %>%
  group_by(czone) %>%
  summarize(across(c(temp, temp_range, precip), mean)) %>%
  rename(precip_czone = precip, temp_range_czone = temp_range, temp_czone = temp) %>%
  mutate(key = 1)

# # standardize climate variables
# 
# climate_country_scaled <- climate_country_simple %>%
#   ungroup() %>%
#   mutate(across(c(precip_country,temp_range_country,temp_country), scale))
# 
# climate_czone_scaled <- climate_czone_simple %>%
#   ungroup() %>%
#   mutate(across(c(precip_czone,humidity_czone,temp_czone), scale))

# cartesian product of czones and countries
climate_country_czone <- climate_czone_simple %>%
  inner_join(climate_country_simple, join_by(key))

# names(climate_country_czone) <- gsub("\\[,1\\]", "", names(climate_country_czone))

# czone-country climate similarity measured by czone-county climate variables' euclidean distance
climate_country_czone <- climate_country_czone %>%
  mutate(temp_euclid_dist = sqrt((temp_country - temp_czone)^2)) %>%
  mutate(temp_range_euclid_dist = sqrt((temp_range_country - temp_range_czone)^2)) %>%
  mutate(precip_euclid_dist = sqrt((precip_country - precip_czone)^2))

# clean
climate_country_czone <- climate_country_czone %>%
  select(czone,iso3,country_name,
         temp_czone, temp_country, temp_euclid_dist,
         temp_range_czone, temp_range_country, temp_range_euclid_dist,
         precip_czone, precip_country, precip_euclid_dist)

write_dta(climate_country_czone, 'Data/Clean/climate_distance_country_czone.dta')
