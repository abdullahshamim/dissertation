rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(fixest)
library(stargazer)
library(RcppRoll)
library(readxl)

source("Code/functions.R")

# load data 
data_main <- read_dta("Data/Clean/data_main_cohort_lvl_laggedpopcontrols.dta")

# filter out refs with ties (imm cohorts present in U.S. prior to ref cohorts)
data_main_woties <- data_main %>% filter(origin %notin% c('cuba','iran','poland','ussr'))

# share of people moving

tot_arrivals <- data_main_woties %>% 
  filter(in_sample == 1) %>%
  group_by(origin,cohort) %>% # can comment either or both groups
  # group_by(origin) %>% 
  summarise(arrivals_tot = sum(arrivals_dreher)) 

tot_inflows <- data_main_woties %>% 
  filter(in_sample == 1, net_inflow > 0) %>%
  group_by(origin,cohort) %>% # can comment either or both groups
  # group_by(origin) %>% 
  summarise(net_inflows_tot = sum(net_inflow))

share_movers <- tot_arrivals %>% 
  left_join(tot_inflows) %>% 
  mutate(share_movers = net_inflows_tot / arrivals_tot) %>% 
  ungroup() %>% 
  mutate(share_movers_overall = stats::weighted.mean(share_movers, w = arrivals_tot)) 

# display table
share_movers_table <- share_movers %>% 
  mutate(share_movers = round(share_movers, 2)) %>% 
  mutate(share_movers_overall = round(share_movers_overall, 2)) %>% 
  mutate(net_inflows_tot = round(net_inflows_tot, 0)) %>% 
  mutate(origin = str_to_title(origin)) %>% 
  mutate(cohort = case_match(cohort, 
                             '7580' ~ '1975-80',
                             '8090' ~ '1980-90',
                             '9199' ~ '1991-99',
                             '0004' ~ '2000-04')) %>% 
  rename('Origin Country' = origin, Cohort = cohort) %>% 
  rename('Refugees Resettled' = arrivals_tot) %>% 
  rename('Sum of CZ Net Inflows' = net_inflows_tot) %>% 
  rename('Net Inflow Share' = share_movers) %>% 
  rename('Overall Net Inflow Share' = share_movers_overall)

knitr::kable(share_movers_table %>% select(-ncol(.)),
             format = 'latex',
             format.args = list(big.mark = ','),
             booktabs = T)

# 16.3 percent move when calculating with net movers
# Mossaad et al. (2020; Science Advances) report 17 % interstate movements
# Also report refugees from Sudan having the highest relocation rates; this is mirrorred in my data
