rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(haven)
library(tidyverse)

source("Code/functions.R")

climate_country <- read_dta('Data/Clean/climate_country.dta')
climate_state <- read_dta('Data/Clean/climate_state.dta')

#### cross-check to see if new code gives identical values for climate similarity - confirmed that it does ####

# # simplify climate data by averaging across years
# 
# climate_country_simple <- climate_country %>% 
#   group_by(iso3, country_name) %>% 
#   summarize(across(c(precip,humidity,temp), mean)) %>% 
#   rename(precip_country = precip, humidity_country = humidity, temp_country = temp) %>% 
#   mutate(key = 1)
# 
# climate_state_simple <- climate_state %>% 
#   group_by(statefp,state_name) %>% 
#   summarize(across(c(precip,humidity,temp), mean)) %>% 
#   rename(precip_state = precip, humidity_state = humidity, temp_state = temp) %>% 
#   mutate(key = 1)
# 
# # standardize climate variables
# 
# climate_country_scaled <- climate_country_simple %>%  
#   ungroup() %>% 
#   mutate(across(c(precip_country,humidity_country,temp_country), scale))
# 
# climate_state_scaled <- climate_state_simple %>%
#   ungroup() %>% 
#   mutate(across(c(precip_state,humidity_state,temp_state), scale))
# 
# # cartesian product of states and countries
# climate_country_state <- climate_state_scaled %>% 
#   inner_join(climate_country_scaled, join_by(key))
# 
# names(climate_country_state) <- gsub("\\[,1\\]", "", names(climate_country_state))
# 
# # state-country climate similarity measured by state-county climate variables' euclidean distance
# climate_country_state <- climate_country_state %>% 
#   mutate(euclid_dist_precip = sqrt((precip_country - precip_state)^2)) %>% 
#   mutate(euclid_dist_temp = sqrt((temp_country - temp_state)^2)) %>% 
#   mutate(euclid_dist_humidity = sqrt((humidity_country - humidity_state)^2))
# 
# # clean
# climate_country_state <- climate_country_state %>% 
#   select(statefp,state_name,iso3,country_name,
#          precip_state, precip_country, precip_euclid_dist = euclid_dist_precip, 
#          temp_state, temp_country, temp_euclid_dist = euclid_dist_temp,
#          humidity_state, humidity_country, humidity_euclid_dist = euclid_dist_humidity) 

#### end values from old code and values from new code comparison ####

climate_country_simple <- climate_country %>%
  group_by(iso3, country_name) %>%
  summarize(across(c(temp, temp_range, precip), mean)) %>%
  rename(precip_country = precip, temp_range_country = temp_range, temp_country = temp) %>%
  mutate(key = 1)

climate_state_simple <- climate_state %>%
  group_by(statefp, state_name) %>%
  summarize(across(c(temp, temp_range, precip), mean)) %>%
  rename(precip_state = precip, temp_range_state = temp_range, temp_state = temp) %>%
  mutate(key = 1)

# # standardize climate variables
# 
# climate_country_scaled <- climate_country_simple %>%
#   ungroup() %>%
#   mutate(across(c(precip_country,temp_range_country,temp_country), scale))
# 
# climate_state_scaled <- climate_state_simple %>%
#   ungroup() %>%
#   mutate(across(c(precip_state,humidity_state,temp_state), scale))

# cartesian product of states and countries
climate_country_state <- climate_state_simple %>%
  inner_join(climate_country_simple, join_by(key))

# names(climate_country_state) <- gsub("\\[,1\\]", "", names(climate_country_state))

# state-country climate similarity measured by state-county climate variables' euclidean distance
climate_country_state <- climate_country_state %>%
  mutate(temp_euclid_dist = sqrt((temp_country - temp_state)^2)) %>%
  mutate(temp_range_euclid_dist = sqrt((temp_range_country - temp_range_state)^2)) %>%
  mutate(precip_euclid_dist = sqrt((precip_country - precip_state)^2))

# clean
climate_country_state <- climate_country_state %>%
  select(statefp,state_name,iso3,country_name,
         temp_state, temp_country, temp_euclid_dist,
         temp_range_state, temp_range_country, temp_range_euclid_dist,
         precip_state, precip_country, precip_euclid_dist)

write_dta(climate_country_state, 'Data/Clean/climate_distance_country_state.dta')