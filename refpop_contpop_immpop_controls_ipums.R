rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# data_refugee <- read_dta("Data/Clean/data_ipums_refugee.dta")
data_immigrant <- read_dta("Data/Clean/data_ipums_immigrant.dta")

# read David Dorn's ipums-commuting zone correspondence
puma_equiv_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta")

katrina_affected_puma <- tibble(year = 2007, puma_equiv = 2277777, czone = 3300, afactor = 1) # pumas recoded due to population loss in the aftermath of Hurricane Katrina

puma_equiv_czone_cross <- puma_equiv_czone_cross %>% 
  bind_rows(katrina_affected_puma) %>% 
  arrange(year, puma_equiv)

# duplicate 2007 crosswalk for 2015; this is to join multyear 2012 in ACS 2015 5 percent
puma_equiv_czone_cross <- puma_equiv_czone_cross %>% 
  filter(year == 2007) %>% 
  mutate(year = 2015) %>% 
  bind_rows(puma_equiv_czone_cross)

# # refpop aggregated across cohorts
# 
# # get refugee pop by year,origin,puma_equiv
# refpop_puma_ipums <- data_refugee %>%
#   group_by(year,cntryname,puma_equiv) %>%
#   summarize(refpop_ipums = sum(perwt))
# 
# # merge czone with puma
# refpop_czone_puma_ipums <- refpop_puma_ipums %>%
#   left_join(puma_equiv_czone_cross, join_by(year, puma_equiv))
# 
# # aggregate to czone level
# refpop_czone_ipums <- refpop_czone_puma_ipums %>%
#   group_by(year,cntryname,czone) %>%
#   summarize(refpop_ipums = weighted.sum(refpop_ipums, w = afactor))


# get continent pop by year,origin,puma_equiv
contpop_puma_ipums <- data_immigrant %>%
  group_by(year,bpl_continent,puma_equiv) %>%
  summarize(contpop_ipums = sum(perwt))

# merge czone with puma
contpop_czone_puma_ipums <- contpop_puma_ipums %>%
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv))

# aggregate to czone level
contpop_czone_ipums <- contpop_czone_puma_ipums %>%
  group_by(year,bpl_continent,czone) %>%
  summarize(contpop_ipums = weighted.sum(contpop_ipums, w = afactor))

# get all immigrant pop by year,origin,puma_equiv
fbpop_puma_ipums <- data_immigrant %>%
  group_by(year,puma_equiv) %>%
  summarize(fbpop_ipums = sum(perwt))

# merge czone with puma
fbpop_czone_puma_ipums <- fbpop_puma_ipums %>%
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv))

# aggregate to czone level
fbpop_czone_ipums <- fbpop_czone_puma_ipums %>%
  group_by(year,czone) %>%
  summarize(fbpop_ipums = weighted.sum(fbpop_ipums, w = afactor))


# matching quality
contpop_czone_puma_ipums %>% filter(is.na(czone)) # all matched
fbpop_czone_puma_ipums %>% filter(is.na(czone)) # all matched

contpop_puma_ipums %>%
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna>0)

contpop_czone_ipums %>%
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna>0)

fbpop_puma_ipums %>%
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna>0)

fbpop_czone_ipums %>%
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna>0)

# save data
write_dta(contpop_czone_ipums, "Data/Clean/continentpop_czone_controls_ipums.dta")
write_dta(fbpop_czone_ipums, "Data/Clean/aggimmpop_czone_controls_ipums.dta")
