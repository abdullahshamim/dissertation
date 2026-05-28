rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(fixest)
library(labelled)

source("Code/functions.R")

# need to get originpop and originpop_cohort
# for as many nonrefugee immigrant origin countries as possible

data_immigrant <- read_dta("Data/Clean/data_ipums_immigrant.dta") %>% filter(year != 2015)
data_refugee <- read_dta("Data/Clean/data_ipums_refugee.dta") %>% filter(year != 2015)

# read David Dorn's ipums-commuting zone correspondence
puma_equiv_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta")

katrina_affected_puma <- tibble(year = 2007, puma_equiv = 2277777, czone = 3300, afactor = 1) # pumas recoded due to population loss in the aftermath of Hurricane Katrina

puma_equiv_czone_cross <- puma_equiv_czone_cross %>% 
  bind_rows(katrina_affected_puma) %>% 
  arrange(year, puma_equiv)

# # duplicate 2007 crosswalk for 2015; this is to join multyear 2012 in ACS 2015 5 percent
# puma_equiv_czone_cross <- puma_equiv_czone_cross %>% 
#   filter(year == 2007) %>% 
#   mutate(year = 2015) %>% 
#   bind_rows(puma_equiv_czone_cross)

# filter out refugee countries

## constructing a dataset identifying origin-country
## filtering out refugee origin countries and countries with incomplete coverage

data_nonref_immigrant <- data_immigrant %>% 
  filter(bpld %notin% unique(data_refugee$bpld))

# get harmonized origin countries

bpl_names_codes <- data_nonref_immigrant %>% 
  select(bpl, bpld, bpld_lab) %>% distinct() %>% 
  mutate(bpl_lab = to_character(bpl), .before = bpld_lab) %>% 
  mutate(bpl_iso3c = countrycode::countrycode(bpl_lab, 'country.name', 'iso3c'),
         bpld_iso3c = countrycode::countrycode(bpld_lab, 'country.name', 'iso3c'))

bpl_bpld_matched <- bpl_names_codes %>% 
  filter(bpl_lab == bpld_lab) %>% 
  mutate(origin_iso3c = bpl_iso3c)

bpl_bpld_matched <- bpl_bpld_matched %>% 
  mutate(origin_iso3c = case_match(
    bpld_lab, 
    c('England', "Wales", "Scotland", "United Kingdom, ns") ~ "GBR",
    "Czechoslovakia" ~ "CSK",
    "Iraq/Saudi Arabia" ~ "SAU",
    "Israel/Palestine" ~ "ISR",
    c("Yemen Arab Republic (North)", "Yemen, PDR (South)") ~ "YEM",
    .default = origin_iso3c))

bpl_bpld_unmatched <- bpl_names_codes %>% 
  filter(bpl_lab != bpld_lab) %>% 
  mutate(origin_iso3c = bpld_iso3c)

bpl_bpld_unmatched <- bpl_bpld_unmatched %>% 
  mutate(origin_iso3c = case_match(
    bpld_lab, 
    c('Taiwan', 'Hong Kong', 'Macau') ~ 'CHN',
    c('Czech Republic', 'Slovakia') ~ 'CSK',
    'Indochina, ns' ~ NA,
    'Macedonia' ~ 'GRC',
    c('Channel Islands', 'Guernsey', 'Isle of Man', 'Jersey', 'Northern Ireland') ~ 'GBR',
    c('Azores','Madeira Islands') ~ 'PRT',
    c('West Bank', 'Gaza Strip', 'Palestine') ~ 'ISR',
    c('East Germany', 'West Berlin', 'East Berlin') ~ 'DEU',
    .default = origin_iso3c))

bpl_bpld_harmonized <- bpl_bpld_matched %>% bind_rows(bpl_bpld_unmatched) %>% 
  mutate(origin_lab = countrycode::countrycode(origin_iso3c, 'iso3c', 'country.name')) %>% 
  mutate(origin_lab = case_match(origin_iso3c, 'CSK' ~ 'Czechoslovakia', .default = origin_lab))

# checks

data_nonref_immigrant %>% 
  left_join(bpl_bpld_harmonized) %>% 
  filter(is.na(origin_lab)) %>% 
  count(bpl_lab) %>% print(n=Inf)

# checks: most unmatched comprised of 4 categories
# abroad, ns, africa, west indies + caribbean, ns

data_nonref_immigrant %>% 
  left_join(bpl_bpld_harmonized) %>% 
  filter(is.na(origin_lab)) %>% 
  count(bpld_lab) %>% print(n=Inf)

# immigrant data with harmonized origins

data_nonref_immigrant_harmonized <- data_nonref_immigrant %>% 
  left_join(bpl_bpld_harmonized) %>% 
  filter(! is.na(origin_lab))

# what share of obs kept (over 97 percent)
nrow(data_nonref_immigrant_harmonized) / nrow(data_nonref_immigrant)

# which origins have complete coverage after first appearing in data?

full_panel_years <- data_nonref_immigrant_harmonized %>% 
  group_by(origin_lab) %>% 
  summarize(first_year = min(year)) %>% 
  mutate(full_panel_years = case_match(first_year, 1970 ~ 5, 1980 ~ 4, 1990 ~ 3, 2000 ~ 2, 2007 ~ 1))

full_coverage_countries <- data_nonref_immigrant_harmonized %>% 
  # filter(year != 2015) %>% 
  count(origin_lab, year) %>% 
  count(origin_lab) %>% 
  left_join(full_panel_years) %>% 
  mutate(full_coverage = ifelse(n==full_panel_years, 1, 0))

# leaving out countries without full coverage retains over 99.6 percent of obs

data_nonref_immigrant_harmonized <- data_nonref_immigrant_harmonized %>% 
  left_join(full_coverage_countries %>% select(origin_lab, full_coverage))

nrow(data_nonref_immigrant_harmonized %>% filter(full_coverage == 1)) / nrow(data_nonref_immigrant_harmonized)

data_nonref_immigrant_harmonized %>% 
  filter(full_coverage == 1) %>% 
  pull(origin_lab) %>% unique() %>% 
  length() # leaves 95 nonrefugee countries



## constructing pop stocks by origin and origin-cohort 
## across year-czone pairs

yrimmig_1980 <- data_nonref_immigrant_harmonized %>% filter(yrimmig == 1980)
yrimmig_1980 <- yrimmig_1980 %>% 
  mutate(cohort = '7580') %>% 
  bind_rows(yrimmig_1980 %>% mutate(cohort = '8090'))

data_nonref_immigrant_harmonized <- data_nonref_immigrant_harmonized %>% 
  mutate(cohort = case_match(yrimmig, 
                             1910:1974 ~ "pre1975",
                             1975:1979 ~ "7580",
                             1980:1990 ~ '8090',
                             1991:1999 ~ '9199',
                             2000:2004 ~ '0004',
                             2000:2015 ~ 'post2004',
                             .default = 'other')) %>% 
  bind_rows(yrimmig_1980)

# immobs_origin_2000 <- data_immigrant %>% filter(year==2000) %>% count(bpld_lab) %>% arrange(desc(n))

immpop_cohort_origin_puma <- data_nonref_immigrant_harmonized %>% 
  mutate(origin_lab = tolower(origin_lab)) %>% 
  rename(origin = origin_lab, continent = bpl_continent) %>% 
  group_by(year, cohort, origin, continent, puma_equiv) %>% 
  summarize(pop = sum(perwt))

immpop_cohort_origin_czone <- immpop_cohort_origin_puma %>% 
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv)) %>% 
  group_by(year, cohort, origin, continent, czone) %>% 
  summarize(pop = sum(pop))

immpop_cohort_origin_czone <- immpop_cohort_origin_czone %>% 
  filter(year <= 2007) %>% 
  filter( (year == 1970 & cohort == 'pre1975') |
            (year == 1980 & cohort == '7580') | 
            (year == 1990 & cohort == '8090') |
            (year == 2000 & cohort == '9199') |
            (year == 2007 & cohort == '0004')) %>% 
  ungroup() %>% arrange(origin,cohort) # %>% select(-year)

# origin-level pop in czone

immpop_origin_puma <- data_nonref_immigrant_harmonized %>% 
  mutate(origin_lab = tolower(origin_lab)) %>% 
  rename(origin = origin_lab, continent = bpl_continent) %>% 
  group_by(year, origin, continent, puma_equiv) %>% 
  summarize(originpop = sum(perwt))

immpop_origin_czone <- immpop_origin_puma %>% 
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv)) %>% 
  group_by(year, origin, continent, czone) %>% 
  summarize(originpop = sum(originpop))

# Find first year each origin appears
first_year_by_cntry <- immpop_origin_czone %>%
  group_by(origin, czone) %>%
  summarize(min_year = min(year), .groups = "drop")

# Expand year but filter out years before first arrival

immpop_origin_czone_complete <- immpop_origin_czone %>% ungroup() %>%
  expand(nesting(origin,continent,czone), year) %>% # expanding year
  left_join(immpop_origin_czone) %>% # joining pop
  left_join(first_year_by_cntry) %>% filter(year >= min_year) %>% # filtering out year cells before first origin-pop record
  mutate(originpop = replace_na(originpop, 0)) %>% select(-min_year)

# save nonrefugee immigrant micodata with identifiable countries satisfying complete coverage
write_dta(data_nonref_immigrant_harmonized, "Data/Clean/data_ipums_nonref_immigrant_harmonized.dta")

# imm pop stocks
write_dta(immpop_origin_czone_complete, "Data/Clean/nonrefugee_immigrant_pop_origin_czone_ipums.dta")
write_dta(immpop_cohort_origin_czone, "Data/Clean/nonrefugee_immigrant_pop_cohort_origin_czone_ipums.dta")

