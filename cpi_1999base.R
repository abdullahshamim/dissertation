rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(tigris)
library(scales)

source("Code/functions.R")

cpi <- read_csv("Data/Raw/cpi_1913_2025.csv")

cpi_1999 <- filter(cpi, year == '1999') %>% pull(cpi)

cpi_1999base <- cpi %>% 
  mutate(cpi_1999 = cpi_1999) %>% 
  mutate(cpi_1999base = cpi / cpi_1999) %>% 
  mutate(year = as.numeric(year))

cpi_1999base <- cpi_1999base %>% select(year,cpi_1999base)

write_dta(cpi_1999base, "Data/Clean/cpi_1999base.dta")
