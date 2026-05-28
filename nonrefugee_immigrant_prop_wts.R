rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(ipumsr)
library(fixest)

source("Code/functions.R")

ddi <- read_ipums_ddi("Data/Raw/CensusWage/usa_00062.xml")
data <- read_ipums_micro(ddi) %>% rename_with(tolower)

data <- data %>% select(
  year, multyear, sample, serial, pernum, famsize, nchild, sex, age, marst, 
  yrimmig, speakeng, educ, educd, empstat, empstatd, labforce, classwkr, classwkrd, 
  wkswork2, hrswork2, uhrswork, incwage
)

data_refugee <- read_dta("Data/Clean/data_ipums_refugee.dta")
data_immigrant <- read_dta("Data/Clean/data_ipums_nonref_immigrant_harmonized.dta")

# read David Dorn's ipums-commuting zone correspondence
puma_equiv_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta")

katrina_affected_puma <- tibble(year = 2007, puma_equiv = 2277777, czone = 3300, afactor = 1) # pumas recoded due to population loss in the aftermath of Hurricane Katrina

puma_equiv_czone_cross <- puma_equiv_czone_cross %>% 
  bind_rows(katrina_affected_puma) %>% 
  arrange(year, puma_equiv)

## propensity weights

data_refugee <- data_refugee %>% filter(year != 2015) %>% mutate(refugee = 1)
data_immigrant <- data_immigrant %>% mutate(refugee = 0)

data_merged <- data_refugee %>% 
  bind_rows(data_immigrant) %>% 
  left_join(data) %>% 
  filter(year != 1970)

data_merged %>% count(famsize)
data_merged %>% count(nchild)
data_merged %>% count(age)
data_merged %>% count(marst)
data_merged %>% count(speakeng)
data_merged %>% count(educ)
data_merged %>% count(empstat)
data_merged %>% count(labforce)
data_merged %>% count(classwkr)
data_merged %>% count(wkswork2)
data_merged %>% count(hrswork2)
data_merged %>% count(uhrswork)
data_merged %>% count(incwage)

mycut <- function(x, breaks, ...) {
  as.factor(cut(x, breaks = breaks, right = FALSE, ...)) # closed brackets on right endpoint
}

# Recode and cut variables
data_merged <- data_merged %>% 
  mutate(
    dfamsize = mycut(famsize, c(0,1,2,6,10,Inf)),
    dnchild = cut(nchild, c(-Inf,0,1,3,5,Inf)),
    dage = cut(age, c(-Inf,0,10,20,30,40,50,60,70,Inf)),
    deduc = cut(educ, c(-Inf,0,6,Inf)),
    dempstat = ifelse(empstat == 1, 1, 0),
    dlabforce = ifelse(labforce == 2, 1, 0),
    selfemployed = ifelse(classwkr == 1, 1, 0),
    duhrswork = cut(uhrswork, c(-Inf,0,10,20,30,40,50,Inf)),
    incwage_clean = ifelse(incwage >= 999998, 0, incwage),
    yrsusa = year - yrimmig,
    employed_wagewrkr = ifelse(dempstat == 1 & selfemployed == 0, 1, 0)
  )

# Restrict to specific cohorts-years
data_merged <- data_merged %>% 
  filter((year == 1980 & cohort == '7580') | 
         (year == 1990 & cohort == '8090') |
         (year == 2000 & cohort == '9199') |
         (year == 2007 & cohort == '0004'))

# Estimate propensity score model
model <- glm(
  refugee ~ 0 + as.factor(cohort) * (dfamsize + dage + deduc + as.factor(marst) + as.factor(speakeng) + yrsusa +
                                       dlabforce + selfemployed + duhrswork + dempstat + dempstat:duhrswork + 
                                       employed_wagewrkr:incwage_clean),
  data = data_merged, family = binomial(link = "probit")
  )

# Add propensity scores
data_merged <- data_merged %>% 
  mutate(prop_score = predict(model, type = "response"))

# Get refugee share by cohort
refshare_cohort <- data_merged %>% 
  group_by(cohort) %>% 
  summarize(share_refugee = stats::weighted.mean(refugee, w = perwt))

# Compute weights
data_merged <- data_merged %>% 
  left_join(refshare_cohort, by = "cohort") %>% 
  mutate(prop_wts = perwt * (prop_score / (1 - prop_score)) / 
                     (share_refugee / (1 - share_refugee))) %>% 
  mutate(prop_wts = ifelse(refugee == 1, perwt, prop_wts))

data_merged %>% group_by(refugee) %>% 
  summarize(across(c(famsize,age,educ,marst,yrsusa,speakeng,
                     selfemployed,dlabforce,dempstat,uhrswork,incwage_clean), 
                   ~ stats::weighted.mean(.x, w = prop_wts)))

## prop wgtd pop stocks

data_immigrant_propwgtd <- data_merged %>% filter(refugee == 0)

immpop_cohort_origin_puma <- data_immigrant_propwgtd %>% 
  mutate(origin_lab = tolower(origin_lab)) %>% 
  rename(origin = origin_lab, continent = bpl_continent) %>% 
  group_by(year, cohort, origin, continent, puma_equiv) %>% 
  summarize(pop = sum(prop_wts))

immpop_cohort_origin_czone <- immpop_cohort_origin_puma %>% 
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv)) %>% 
  group_by(year, cohort, origin, continent, czone) %>% 
  summarize(pop = sum(pop))

# origin-level pop in czone

immpop_origin_puma <- data_immigrant_propwgtd %>% 
  mutate(origin_lab = tolower(origin_lab)) %>% 
  rename(origin = origin_lab, continent = bpl_continent) %>% 
  group_by(year, origin, continent, puma_equiv) %>% 
  summarize(originpop = sum(prop_wts))

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

# imm pop stocks
write_dta(immpop_origin_czone_complete, "Data/Clean/nonrefugee_immigrant_propwgtdpop_origin_czone_ipums.dta")
write_dta(immpop_cohort_origin_czone, "Data/Clean/nonrefugee_immigrant_propwgtdpop_cohort_origin_czone_ipums.dta")


