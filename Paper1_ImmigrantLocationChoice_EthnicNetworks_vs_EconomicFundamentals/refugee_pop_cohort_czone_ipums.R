rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(ipumsr)
library(fixest)
library(labelled)
library(countrycode)
library(scales)

source("Code/functions.R")

# weighted.sum <- function(x, w, na.rm = FALSE) { 
#   sum(x * w, na.rm = na.rm) 
# }

# read raw census/acs data
ddi <- read_ipums_ddi("Data/Raw/CensusWage/usa_00059.xml")
data <- read_ipums_micro(ddi) %>% 
  rename_with(tolower) %>% 
  select(year,multyear,sample,serial,pernum,perwt,statefip,cntygp97,cntygp98,puma,citizen,bpl,bpld,yrimmig)

data <- data %>% mutate(perwt = ifelse(year == 1970, perwt / 2, perwt)) # divide 1970 perwt by 2

# define immigrant, filter out non-contiguous states/territories
data <- data %>% 
  mutate(immigrant = ifelse(citizen %in% 0:1, 0, 1)) %>% 
  filter(statefip %notin% c(2, 15, 72))

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


# filter data if immigrant
data_immigrant <- data %>% 
  filter(immigrant == 1) %>%
  filter(bpld >= 15000) %>% # additional filtering needed only for 1970 form 2 
  mutate(bpld_lab=to_character(bpld)) %>% 
  mutate(iso3c=countrycode(bpld_lab,"country.name","iso3c"))

# create puma equivalent geography
data_immigrant <- data_immigrant %>% 
  mutate(
    cntygp98 = str_pad(cntygp98, width = 3, pad = "0"),
    puma = ifelse(year %in% c(1990,2000,2007), str_pad(puma, width = 4, pad = "0"), puma),
    puma = ifelse(year == 2015 & multyear < 2012, str_pad(puma, width = 4, pad = "0"), puma),
    puma = ifelse(year == 2015 & multyear >= 2012, str_pad(puma, width = 5, pad = "0"), puma)
  ) %>%
  mutate(cntygp98_full = ifelse(year == 1980, str_c(statefip, cntygp98), cntygp98),
         puma_full = ifelse(year > 1980, str_c(statefip, puma), puma)) %>%
  mutate(cntygp98_full = as.numeric(cntygp98_full), puma_full = as.numeric(puma_full)) %>%
  mutate(puma_equiv = rowSums(across(c(cntygp97, cntygp98_full, puma_full)), na.rm = TRUE))

# # bpld codes continents
# 
# n amrc = 15000:19900
# sc amrc = 20000:30091
# w eurp = 40000:44000 except 43000 = Albania
# e eurp = 45000:46590 except 45300:45362 = Germany
# n.s. eurp = 49900
# asia = 50000:59900
# africa = 60000:60099
# oceania = 70000:71090

data_immigrant <- data_immigrant %>% 
  mutate(bpl_continent = case_match(bpld,
                                    15000:19900 ~ "n_americ",
                                    20000:30091 ~ "s_americ",
                                    40000:44000 ~ "w_europ",
                                    45000:46590 ~ "e_europ",
                                    50000:59900 ~ "asia",
                                    60000:60099 ~ "afric",
                                    70000:71090 ~ "oceania",
                                    .default = "other")) %>% 
  mutate(bpl_continent = ifelse(bpld == 43000, 'e_europ', bpl_continent)) %>% # albania
  mutate(bpl_continent = ifelse(bpld %in% 45300:45362, 'w_europ', bpl_continent)) # germany


refugee_countries <- c('afghanistan','burma','cambodia','cuba','ethiopia','iran','iraq','laos',
                       'liberia','poland','romania','somalia','sudan','ussr','vietnam') %>% # leaves out yugoslavia
  str_to_title()

# filter only refugee countries
data_refugee <- data_immigrant %>% 
  filter(bpld_lab %in% refugee_countries) %>% 
  mutate(cntryname = bpld_lab)

# how many countries matched?
data_refugee$bpld_lab %>% unique()

# burma,ussr,cambodia not captured; yugoslavia needs to be corrected
# note: ethiopia includes eritrea in 1990 but not later years 

data_burma <- data_immigrant %>% 
  filter(iso3c == "MMR") %>% 
  mutate(cntryname = "Burma")

data_cambodia <- data_immigrant %>% 
  filter(iso3c == "KHM") %>% 
  mutate(cntryname = "Cambodia")

# Yugoslavia, Croatia, Serbia, Bosnia, Kosovo
data_yugoslavia <- data_immigrant %>%
  # filter(bpld_lab != "Yugoslavia") %>% # filter out people already matched
  filter(bpld %in% c(45700,45710,45730,45740,45790)) %>% # capture people not matched
  mutate(cntryname = "Yugoslavia")

# Former USSR countries:
data_ussr <- data_immigrant %>% 
  filter(bpld %in% 46500:46590) %>% 
  mutate(cntryname = "Ussr")

# refugees_yugoslavia <- refarrivals_county %>%
#   filter(citizenship_stable == 'yugoslavia') %>%
#   group_by(citizenship_stable, year) %>%
#   summarize(refugees = sum(refugees))
# 
# ggplot(data_yugoslavia %>% filter(year == 2000), aes(x=yrimmig)) +
#   geom_freqpoly(aes(weight=perwt)) +
#   facet_wrap(~bpld_lab)

# refugees_ussr <- refarrivals_county %>% 
#   filter(citizenship_stable == 'ussr') %>% 
#   group_by(citizenship_stable, year) %>% 
#   summarize(refugees = sum(refugees)
#   
# ggplot(data_ussr %>% filter(year == 2000), aes(x=yrimmig)) + 
#   geom_freqpoly(aes(weight=perwt)) + 
#   facet_wrap(~bpld_lab)

# merge all refugee pop data
data_refugee <- data_refugee %>% 
  bind_rows(data_burma) %>% 
  bind_rows(data_cambodia) %>% 
  bind_rows(data_yugoslavia) %>% 
  bind_rows(data_ussr)


# simple cohort definitions
data_refugee <- data_refugee %>% 
  mutate(cohort = case_match(yrimmig,
                                     1910:1974 ~ "pre1975",
                                     1975:1979 ~ "7580",
                                     1980:1990 ~ "8090",
                                     1991:1999 ~ "9199",
                                     2000:2004 ~ "0004",
                                     2005:2008 ~ "0508",
                                     2008:2025 ~ "post2008",
                                     .default = "other")) %>% 
  mutate(cohort = ifelse(year==1970, "pre1975", cohort))


# get refugee pop by year,origin,cohort,puma_equiv
refpop_cohort_puma_ipums <- data_refugee %>% 
  group_by(year,cntryname,cohort,puma_equiv) %>% 
  summarize(refpop_ipums = sum(perwt))

# make sure years are matched properly
refpop_cohort_puma_ipums$year %>% unique()
puma_equiv_czone_cross$year %>% unique()

# merge czone
refpop_cohort_czone_puma_ipums <- refpop_cohort_puma_ipums %>% 
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv)) 
  
refpop_cohort_czone_puma_ipums %>% filter(is.na(czone)) # all matched
refpop_cohort_czone_puma_ipums %>% filter(is.na(afactor)) # all matched

# aggregate to czone level
refpop_cohort_czone_ipums <- refpop_cohort_czone_puma_ipums %>% 
  group_by(year,cntryname,cohort,czone) %>% 
  summarize(refpop_ipums = weighted.sum(refpop_ipums, w = afactor))

refpop_cohort_puma_ipums %>% 
  ungroup() %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna>0)

refpop_cohort_czone_ipums %>% 
  ungroup() %>% 
  mutate(numna = rowSums(across(everything(), is.na))) %>% 
  filter(numna>0)

# refpop aggregated across cohorts

# get refugee pop by year,origin,puma_equiv
refpop_puma_ipums <- data_refugee %>%
  group_by(year,cntryname,puma_equiv) %>%
  summarize(refpop_ipums = sum(perwt))

# merge czone with puma
refpop_czone_puma_ipums <- refpop_puma_ipums %>%
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv))

refpop_czone_puma_ipums %>% filter(is.na(czone)) # all matched
refpop_czone_puma_ipums %>% filter(is.na(afactor)) # all matched

# aggregate to czone level
refpop_czone_ipums <- refpop_czone_puma_ipums %>%
  group_by(year,cntryname,czone) %>%
  summarize(refpop_ipums = weighted.sum(refpop_ipums, w = afactor))

# 6/19/25: updated the following for full origin-year-puma coverage after first year

# Find first year each origin appears
first_year_by_cntry <- refpop_czone_ipums %>%
  group_by(cntryname, czone) %>%
  summarize(min_year = min(year), .groups = "drop")

# Expand year but filter out years before first arrival
refpop_czone_complete_ipums <- refpop_czone_ipums %>% ungroup() %>% 
  expand(nesting(cntryname,czone), year) %>% 
  left_join(refpop_czone_ipums) %>% 
  left_join(first_year_by_cntry) %>% 
  filter(year >= min_year) %>% 
  mutate(refpop_ipums = replace_na(refpop_ipums, 0)) %>% 
  select(-min_year)

# check

# missing one year
refpop_czone_ipums %>% 
  filter(cntryname=='Cuba', czone == 601)

# complete coverage
refpop_czone_complete_ipums %>% 
  filter(cntryname=='Cuba', czone == 601)

# end new update 6/19/25

refpop_puma_ipums %>%
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna>0)

refpop_czone_ipums %>%
  ungroup() %>%
  mutate(numna = rowSums(across(everything(), is.na))) %>%
  filter(numna>0)


# save
write_dta(data_refugee, "Data/Clean/data_ipums_refugee.dta")
write_dta(data_immigrant, "Data/Clean/data_ipums_immigrant.dta")
write_dta(refpop_cohort_czone_ipums, "Data/Clean/refugee_pop_cohort_czone_ipums.dta")
write_dta(refpop_czone_complete_ipums, "Data/Clean/refugee_pop_czone_ipums.dta") # 6/19/25 update

