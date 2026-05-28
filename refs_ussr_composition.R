rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(fixest)
library(stargazer)
library(RcppRoll)
library(readxl)

source("Code/functions.R")

refarrivals_dreher23_orr_pt1 <- read_dta("Data/Raw/RefArrivals_dreher-langlotz-matzat-parsons/pt1dr/ORR_1975_2008_Indv_v2_pt_I_dr.dta")
refarrivals_dreher23_orr_pt2 <- read_dta("Data/Raw/RefArrivals_dreher-langlotz-matzat-parsons/pt2dr/ORR_1975_2008_Indv_v2_pt_II_dr.dta")

refs_ussr_composition1 <- refarrivals_dreher23_orr_pt1 %>% 
  filter(citizenship_stable == 'ussr') %>% 
  group_by(citizenship_country_name) %>% 
  summarize(refugees = n()) %>% 
  arrange(desc(refugees)) %>% 
  mutate(freq = refugees / sum(refugees)) %>% 
  mutate(cum_freq = cumsum(freq))

refs_ussr_composition2 <- refarrivals_dreher23_orr_pt2 %>% 
  filter(citizenship_stable == 'ussr') %>% 
  group_by(citizenship_country_name) %>% 
  summarize(refugees = n()) %>% 
  arrange(desc(refugees)) %>% 
  mutate(freq = refugees / sum(refugees)) %>% 
  mutate(cum_freq = cumsum(freq))

refs_ussr_composition1 <- refs_ussr_composition1 %>% 
  rename(citizenship = citizenship_country_name, persons = refugees)
refs_ussr_composition2 <- refs_ussr_composition2 %>% 
  rename(citizenship = citizenship_country_name, persons = refugees)

write_dta(refs_ussr_composition2, "Data/Clean/refs_ussr_composition.dta")
