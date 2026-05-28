rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(tigris)
library(sf)

source("Code/functions.R")

state_sf <- tigris::states(year = 2010) %>% 
  rename_with(tolower) %>% 
  filter(statefp10 %notin% c('02','15',72)) %>% 
  select(name10, stusps10, statefp10,  geometry) %>% 
  rename(state10name = name10, state10abb = stusps10) %>% 
  arrange(state10name)

us_sf <- state_sf %>% summarize(geometry = sf::st_union(geometry))

saveRDS(state_sf, "Data/Clean/state_sf.RDS")
saveRDS(us_sf, "Data/Clean/us_sf.RDS")
