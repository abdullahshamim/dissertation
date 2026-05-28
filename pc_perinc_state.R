rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(tigris)
library(scales)

source("Code/functions.R")

pcpi_state <- read_csv("Data/Raw/per_capita_personal_income_by_state/annual.csv")
states <- as_tibble(readRDS("Data/Clean/state_sf.RDS")) %>% select(-geometry)

pcpi_state <- pcpi_state %>% 
  mutate(year = lubridate::year(observation_date), .after = observation_date) %>% 
  select(-observation_date)

pcpi_state_long <- pcpi_state %>% 
  pivot_longer(ALPCPI:WYPCPI, names_to = "state", values_to = "pcpi") %>% 
  mutate(state = substr(state, 1, 2))

pcpi_state_long <- pcpi_state_long %>% 
  left_join(states, join_by(state==state10abb)) %>% 
  rename(state10abb = state) %>% 
  relocate(pcpi, .after = everything())

write_dta(pcpi_state_long, "Data/Clean/pc_perinc_state.dta")
