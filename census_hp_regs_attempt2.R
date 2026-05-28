rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(ipumsr)
library(fixest)

# read raw data

ddi <- read_ipums_ddi("Data/Raw/CensusHP/usa_00060.xml")
data <- read_ipums_micro(ddi)
data <- data %>% rename_with(tolower) %>% distinct() # same hh has multiple entries

# read puma_equiv (countygp97,countygp98,puma) and Dorn 1990 commuting zone crosswalk
puma_equiv_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta")

# run regression: gross rents on a year * czone fixed effect controlling for
# rooms, bedrooms, builtyear, unitsstr - likely controlling for commuse, ownership,
# plumbing, and kitchen

# # vars
# ownershp: 0 = N/A, 1 = being bought or owned, 2 = rented
# commuse: 0 = N/A, 1 = No commercial use, 2 = Yes, used commercially, 3 = Unknown, unit on 10+ acres, 4 = Unknown, unit on 3+ cuerdas
# rentgrs: rent + utilities; continuous; top codes: 1970 and 1980 = 999, 1990 = 1500, 2000 = 9999, ACS = 99.5 percentile in State; values above top code coded as state median (mean) of values above top code in 1990 (ACS) 
# valueh: top coded; 1970 = 50K, 1980 = 200K, 1990 = 400K, 2000 and ACS2000-2007 = 1000K, 2008-onwards top codes more variable - available at ipums website
# vacancy: 0 = N/A, 1 = for rent, 2 = for sale only, 3 = rented/sold but not yet occupied, 4-9 = other vacant (seasonal, recreational, etc.)
# kitchen: 0 = N/A 1 = No, 2 = No, or shared use, 3 = Yes, shared use,	4 = Yes (shared or exclusive use), 5 = Yes, exclusive use
# rooms: 0-9 for most samples; 0 = N/A, 9 = 9+, data on higher than 9 rooms only available after 2007 ACS
# plumbing: 0 = N/A, 10-14 = without complete plumbing, 20-22 = with complete plumbing
# builtyr: 0 = N/A, 1 = 0-1 year old,	2 = 2-5 years, 3 = 6-10 years, 4 = 11-20 years, 5 = 21-30 years, 6 = 31-40 years (31+ in 1960 and 1970)
# builtyr2: 00 = N/A, 01 = 1939 or earlier, 02 = 1940-1949, 03 = 1950-1959, 04 = 1960-1969, 05 = 1970-1979, 06 = 1980-1989, 07 = 1990-1994 (1990-1999 in the 2005-onward ACS and the PRCS), 08 = 1995-1999 (1995-1998 in the 2000-2002 ACS), 09 = 2000-2004 (1999-2002 in the 2000-2002 ACS, 2000-2009 in the 2021-onward ACS and PRCS), 10 = 2005 (2005 or later in datasets containing 2005, 2006, or 2007 ACS/PRCS data), 11 = 2006, 12 = 2007, 13 = 2008, 14 = 2009, 15 = 2010, 16 = 2011, 17 = 2012, 18 = 2013, 19 = 2014, 20 = 2015
# unitsstr: 00 = N/A, 01 = Mobile home or trailer, 02 = Boat, tent, van, other, 03 = 1-family house, detached, 04 = 1-family house, attached, 05 = 2-family building, 06 = 3-4 family building, 07 = 5-9 family building, 08 = 10-19 family building, 09 = 20-49 family building, 10 = 50+ family building
# bedrooms: 00	N/A, 01 = No bedrooms, 02 = 1, 03 = 2, 04 = 3, 05 = 4, 06 = 5+ (1970-2000, 2000-2007 ACS/PRCS), data on higher than 6 bedrooms available only after 2007 ACS

# construct harmonized rooms excluding bedrooms and yrbuilt vars

# harmonized rooms excluding bedrooms
data <- data %>% 
  mutate(rooms_harmon = ifelse(rooms > 9, 9, rooms)) %>% 
  mutate(bedrooms_harmon = ifelse(bedrooms > 6, 6, bedrooms)) %>% 
  mutate(rooms_excluding_bedrooms_harmon = rooms_harmon - bedrooms_harmon + 1) %>% 
  mutate(rooms_excluding_bedrooms = rooms - bedrooms + 1)

# harmonized building age
# if counting from the last year in the survey
# 2005: 51-60 = 1945-1954, 41-50 = 1955-1964, 31-40 = 1965-1974, 21-30: 1975-1984, 11-20: 1985-1994, 6-10: 1995-1999, 2-5: 2000-2003, 0-1 = 2004-2005
# 2011: 30+ = 1980-, 21-30: 1981-1990, 11-20: 1991-2000, 6-10: 2001-2005, 2-5: 2006-2009, 0-1 = 2010-2011

data <- data %>% 
  mutate(building_age = ifelse(year == 2007, case_match(builtyr2, 1 ~ 9, 2 ~ 8, 3 ~ 7, 4 ~ 6, 5 ~ 5, 6 ~ 4, 7 ~ 3, 9 ~ 2, 10 ~ 1, .default = 0), 0)) %>% 
  mutate(building_age = ifelse(year == 2015, case_match(builtyr2, 1 ~ 10, 2 ~ 9, 3 ~ 8, 4 ~ 7, 5 ~ 6, 6 ~ 5, 7 ~ 4, 9:10 ~ 3, 11:14 ~ 2, 15:20 ~ 1, .default = 0), building_age)) %>% 
  mutate(building_age = ifelse(year <= 2000, builtyr, building_age))

# data %>% count(year,builtyr,builtyr2,building_age) %>% print(n=1000)

# harmonized pumas across census years
data <- data %>% 
  mutate(
    cntygp98 = str_pad(cntygp98, width = 3, pad = "0"),
    puma = ifelse(year %in% c(1990,2000,2007), str_pad(puma, width = 4, pad = "0"), puma),
    puma = ifelse(year > 2012, str_pad(puma, width = 5, pad = "0"), puma)
  ) %>%
  mutate(cntygp98_full = ifelse(year == 1980, str_c(statefip, cntygp98), cntygp98),
         puma_full = ifelse(year > 1980, str_c(statefip, puma), puma)) %>%
  mutate(cntygp98_full = as.numeric(cntygp98_full), puma_full = as.numeric(puma_full)) %>%
  mutate(puma_equiv = rowSums(across(c(cntygp97, cntygp98_full, puma_full)), na.rm = TRUE))

pumas_df <- data %>% select(year,puma_equiv) %>% distinct()

# linear specification with (year by puma - specific) slopes (adj R-squared 0.65)
linear_hp_reg <- feols(rentgrs ~ 0 | year ^ puma_equiv [bedrooms, rooms_excluding_bedrooms, building_age, unitsstr],
                       combine.quick = FALSE, weights = ~hhwt,
                       data %>% filter(bedrooms != 0, ownershp == 2, commuse < 2, unitsstr > 2, building_age != 0, kitchen > 3, plumbing %in% 20:22))

# linear-log specification with (year by puma - specific) slopes (adj R-squared 0.65)
log_hp_reg <- feols(rentgrs ~ 0 | year ^ puma_equiv [log(bedrooms), log(rooms_excluding_bedrooms), building_age, unitsstr],
                       combine.quick = FALSE, weights = ~hhwt,
                       data %>% filter(bedrooms != 0, ownershp == 2, commuse < 2, unitsstr > 2, building_age != 0, kitchen > 3, plumbing %in% 20:22))

# linear-log specification with uniform slopes across pumas and years (adj R-squared = 0.61)
log_hp_reg_sep <- feols(rentgrs ~ log(bedrooms) + log(rooms_excluding_bedrooms) + building_age + unitsstr | year ^ puma_equiv,
                    combine.quick = FALSE, weights = ~hhwt,
                    data %>% filter(bedrooms != 0, ownershp == 2, commuse < 2, unitsstr > 2, building_age != 0, kitchen > 3, plumbing %in% 20:22))


# recover fixed effect
summary(linear_hp_reg)
fe <- fixef(linear_hp_reg)
summary(fe)

summary(log_hp_reg)
fe_log <- fixef(log_hp_reg)
summary(fe_log)

summary(log_hp_reg_sep)
fe_log_sep <- fixef(log_hp_reg_sep)
summary(fe_log_sep)

# choose preferred specification and save fixed effects into data frame
hp_year_puma <- enframe(fe$`year^puma_equiv`, name = "year_eqvpuma", value = "hp") %>% 
  separate(year_eqvpuma, into = c("year", "puma_equiv"), sep = "_") 

# merge czone based on puma
hp_year_czone_puma <- hp_year_puma %>% 
  mutate(puma_equiv = as.integer(puma_equiv), year = as.integer(year)) %>% 
  arrange(year, puma_equiv) %>% 
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv))

hp_year_czone_puma[is.na(hp_year_czone_puma$czone), ] # one observation in 2007 unmatched corresponding to pumas in Louisiana with large pop outflows in the aftermath of Hurrican Katrina; corresponding pumas recoded to 2277777

# correct for Katrina affected pumas with puma code changes in 2006 and 2007 vs 2005
hp_year_czone_puma$hp[hp_year_czone_puma$year == 2007 & hp_year_czone_puma$puma_equiv %in% c("221801","221802","221905")] <- hp_year_czone_puma$hp[hp_year_czone_puma$year == 2007 & hp_year_czone_puma$puma_equiv == 2277777]

# puma codes were changed starting 2012, so people interviewed in 2011 had the old codes and people interviewed afterward had the new ones
hp_year_czone_puma[is.na(hp_year_czone_puma$czone), ] # one observation in 2007 unmatched corresponding to pumas in Louisiana with large pop outflows in the aftermath of Hurrican Katrina; corresponding pumas recoded to 2277777

# solution: drop old codes
hp_year_czone_puma_clean <- hp_year_czone_puma %>% filter(!is.na(czone))

hp_year_czone <- hp_year_czone_puma_clean %>% 
  group_by(year, czone) %>% 
  summarize(hp = stats::weighted.mean(hp, w = afactor))

hp_year_czone[is.na(hp_year_czone$hp), ] # all matched

p <- ggplot(hp_year_czone, aes(x=hp)) + 
  theme_bw()+
  geom_histogram()+
  facet_wrap(~ year)+
  labs(x = "Yearly Rent Fixed Effect", y = "Number of Commuting Zones")

ggsave("Output/Figures/hp_fixed_effects.png",
       height = 4, width = 7, units = 'in', p)

write_dta(hp_year_czone, "Data/Clean/hp_fixed_effects_czone_year_attempt2.dta")
write_dta(hp_year_puma, "Data/Clean/hp_fixed_effects_puma_equiv_year_attempt2.dta")
