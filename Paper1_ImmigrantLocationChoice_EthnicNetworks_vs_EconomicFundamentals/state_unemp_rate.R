rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(tigris)

source("Code/functions.R")

# get state fips codes and state names 
states <- as_tibble(states()) %>% 
  rename_with(tolower) %>% 
  select(statefp, name) %>% 
  rename(state_name=name)

# read data
state_unemp_rate <- read_csv("Data/Raw/LAUS/state_unemp_rate.txt") %>% 
  select(`Series ID`, contains("Annual")) %>% 
  filter(!str_detect(`Series ID`, "LASST"))

# clean var names
state_unemp_rate <- state_unemp_rate %>% 
  rename_with(tolower) %>% 
  rename_with( ~ str_replace_all(.x, " ", "_")) %>% 
  rename_with( ~ str_replace_all(.x, "annual", "year"))

# extract fips code from series id and filter out hawaii and alaska
state_unemp_rate <- state_unemp_rate %>% 
  mutate(statefp = str_sub(series_id, 6, 7), .after = series_id) %>% 
  filter(statefp %notin% c('02','15'))

# additional cleaning
state_unemp_rate <- state_unemp_rate %>% 
  left_join(states) %>% 
  relocate(state_name, .after = statefp) %>% 
  select(-series_id)

# pivot longer and final cleaning
state_unemp_rate <- state_unemp_rate %>% 
  pivot_longer(starts_with('year'), 
               names_to = 'year', 
               names_prefix = 'year_', 
               values_to = 'unemp_rate') %>% 
  mutate(unemp_rate = str_replace_all(unemp_rate, "\\(R\\)", "")) %>%
  mutate(across(c(year,unemp_rate), as.numeric))

# save
write_dta(state_unemp_rate, "Data/Clean/state_unemp_rate_clean.dta")


