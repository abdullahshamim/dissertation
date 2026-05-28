rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(ipumsr)
library(fixest)

# read raw census/acs data

ddi <- read_ipums_ddi("Data/Raw/CensusWage/usa_00059.xml")
data <- read_ipums_micro(ddi) %>% rename_with(tolower)

# Collect puma equivalent geographic variables
data <- data %>% 
  mutate(
    cntygp98 = str_pad(cntygp98, width = 3, pad = "0"),
    puma = ifelse(year %in% c(1990,2000,2007), str_pad(puma, width = 4, pad = "0"), puma),
    puma = ifelse(year >= 2012, str_pad(puma, width = 5, pad = "0"), puma)
  ) %>%
  mutate(cntygp98_full = ifelse(year == 1980, str_c(statefip, cntygp98), cntygp98),
         puma_full = ifelse(year > 1980, str_c(statefip, puma), puma)) %>%
  mutate(cntygp98_full = as.numeric(cntygp98_full), puma_full = as.numeric(puma_full)) %>%
  mutate(puma_equiv = rowSums(across(c(cntygp97, cntygp98_full, puma_full)), na.rm = TRUE))

puma_pop <- data %>% 
  select(year,puma_equiv,perwt) %>% 
  mutate(perwt = ifelse(year==1970, perwt / 2, perwt)) %>% 
  group_by(year,puma_equiv) %>% 
  summarize(pop = sum(perwt))

puma_hh <- data %>% 
  select(year,puma_equiv,serial,hhwt) %>% 
  distinct() %>% 
  mutate(hhwt = ifelse(year==1970, hhwt / 2, hhwt)) %>% 
  group_by(year,puma_equiv) %>% 
  summarize(num_hhs = sum(hhwt))

puma_pop_numhhs <- puma_pop %>% left_join(puma_hh)

write_dta(puma_pop_numhhs, "Data/Clean/puma_equiv_pop_numhhs.dta")
