rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(ipumsr)
library(fixest)

# read raw census/acs data

ddi <- read_ipums_ddi("Data/Raw/CensusWage/usa_00059.xml")
data <- read_ipums_micro(ddi) %>% rename_with(tolower)

# read David Dorn's ipums-commuting zone correspondence
puma_equiv_czone_cross <- read_dta("Data/Clean/puma_equiv_czone_crosswalk.dta")

# individual‐level controls includes education, experience, the interaction of education and experience, marital status, race and ethnicity,
# veteran status, industry, occupation, and ability to speak English, all interacted with the female indicator. As is standard, the potential experience is
# defined as age minus years of schooling minus 5. Then, we control for this potential experience to the powers of 1–4, interacted with years of schooling.
# We also control for the number of years since arrival to the United States for immigrants

# 1st step: figure out structure of each var
# sex: 1 = male, 2 = female
# age: continuous
# marst: factor var : 1 = "Married, spouse present", 2 = "Married, spouse absent", 3 = "Separated", 4 = "Divorced", 5 = "Widowed", 6 = "Never Married/Single", 9 = NA (value 9 never represented in data)
# race: 1 = "White" 2 = "Black/African American" 3 = "American Indian or Alaska Native" 4 = "Chinese" 5 = Japanese, 6 = "Other Asian or Pacific Islander" 7 = "Other race, nec
# hispan: 0 =	Not Hispanic 1 = Mexican, 2 = Puerto Rican, 3 = Cuban 4 = Other
# bpl
# citizen: 0 = N/A 1 = Born abroad of American parents 2 = Naturalized citizen 3 = Not a citizen
# yrimmig: 0 = "born in US"; 1970, 1980, 1990 samples report a range of yrimmig, 2000 and later report exact years
# speakeng: 0 = NA, 1 = Does not speak English, 3 = Yes, speaks only English, 4 =	Yes, speaks very well, 5 = Yes, speaks well, 6 = Yes, but not well
# educ: more or less continuous - 00 = N/A or no schooling, 01=Nursery school to grade 4, 02=Grade 5, 6, 7, or 8, 03=Grade 9, 04=Grade 10, 05=Grade 11, 06=Grade 12, 07=1 year of college, 08=2 years of college, 09=3 years of college, 10=4 years of college, 11=5+ years of college
# empstat: 0 = N/A, 1 =	Employed. 2 = Unemployed, 3 = Not in labor force
# labforce: 0 = N/A, 1 = No, not in the labor force, 2 = Yes, in the labor force 
# classwkr: 0 = N/A, 1 = Self-Employed, 2 = Works for wages
# occ: complex category with many options
# wkswork2 (Weeks worked last year, intervalled): 0 = N/A (or Missing), 1 = 1-13 weeks, 2 = 14-26 weeks, 3 = 27-39 weeks, 4 = 40-47 weeks, 5 = 48-49 weeks, 6 = 50-52 weeks
# hrswork2 (hours worked last week, intervalled): 0 = N/A, 1 = 1-14 hours, 2 = 15-29 hours, 3 = 30-34 hours, 4 = 35-39 hours, 5 = 40 hours, 6 = 41-48 hours, 7 = 49-59 hours, 8 = 60+ hours
# uhrswork (Usual hours worked per week): continuous
# incwage: 999999 = N/A, 999998 = Missing, continuous but top coded, top codes: 1970 = $50,000, 1980 = $75,000, 1990 = $140,000*, 2000 = $175,000**, ACS (2000-2002) = $200,000**, ACS (2003-onward) = 99.5th Percentile in State**, PRCS (2005-onward) = 99.5th Percentile in State**


# hourly wage calculation

# hours worked per week
# filter by empstat == 1 (or 1 and 2)
# hours worked per week = uhrswork
# filtering out uhrswork == 0 if necessary

# weeks worked last year:
# recode using midpoint of range
# filter out wkswork2 == 0 if necessary

# total wage income
# top codes: 1970 = $50,000, 1980 = $75,000, 1990 = $140,000*, 2000 = $175,000**, ACS (2000-2002) = $200,000**, ACS (2003-onward) = 99.5th Percentile in State**, PRCS (2005-onward) =99.5th Percentile in State**
# filter out missing and N/A values

# calculating hourly wage
data_clean <- data %>% 
  filter(empstat == 1 | empstat == 2) %>% 
  filter(classwkr == 2, age %in% 25:59) %>% 
  filter(uhrswork > 29 | year == 1970, hrswork2 > 2 | year > 1970) %>% 
  filter(wkswork2 > 2, incwage < 999998) %>% 
  mutate(wkswork_imp = case_match(wkswork2, 1 ~ 7, 2 ~ 20, 3 ~ 33, 4 ~ 43, 5 ~ 48.5, 6 ~ 51, .default = wkswork2)) %>% 
  mutate(hrswork_imp = case_match(hrswork2, 1 ~ 7, 2 ~ 22, 3 ~ 33, 4 ~ 37, 5 ~ 40, 6 ~ 44.5, 7 ~ 54, 8 ~ 70, .default = hrswork2)) %>% 
  mutate(hourly_wage = incwage / (uhrswork * wkswork_imp)) %>% 
  mutate(hourly_wage = ifelse(year == 1970, incwage / (hrswork_imp * wkswork_imp), hourly_wage))

# ensure reasonable hourly_wage calculations
ggplot(data_clean, aes(x=hourly_wage)) + 
  geom_histogram() +
  xlim(0,100)+
  facet_wrap(~year)
  
# experience
data_clean <- data_clean %>% 
  mutate(yrsinschool = case_match(educ, 0:1 ~ 3, 2 ~ 7, 11 ~ 19, .default = educ)) %>% 
  mutate(yrsinschool = ifelse(educ %in% 3:10, educ + 7, yrsinschool)) %>% 
  mutate(exper = age - yrsinschool - 5)

# simple education categories
data_clean <- data_clean %>% 
  mutate(educ_simple = case_match(educ, 0:6 ~ 1, 7:9 ~ 2, 10:11 ~ 3, .default = educ))

# immigrant-specific
data_clean <- data_clean %>% 
  mutate(immigrant = ifelse(citizen > 1, 1, 0)) %>% # immigrant indicator
  mutate(yrsusa = ifelse(year <=2000, year - yrimmig, multyear - yrimmig)) %>% 
  mutate(yrsusa = ifelse(immigrant == 0, 0, yrsusa)) %>% 
  mutate(hispan_ind = ifelse(hispan > 0, 1, 0))  # hispanic indicator

# # aggregation of finer occupation codes
#
# Managerial and Professional (000-200);
# Technical, Sales, and Administrative (201-400);
# Service (401-470);
# Farming, Forestry, and Fishing (471-500);
# Precision Production, Craft, and Repairers (501-700);
# Operatives and Laborers (701-900);
# Non-occupational responses (900-999)

data_clean <- data_clean %>% 
  filter(occ1990 <= 900) %>% 
  mutate(occ1990_agg = case_match(occ1990, 
                                  0:200 ~ "Managerial and Professional",
                                  201:400 ~ "Technical, Sales, and Administrative", 
                                  401:470 ~ "Service",
                                  471:500 ~ "Farming, Forestry, and Fishing",
                                  501:700 ~ "Precision Production, Craft, and Repairers",
                                  701:900 ~ "Operatives and Laborers"))

# # aggregation of industry codes
# 
# AGRICULTURE, FORESTRY, AND FISHERIES (10-32)
# MINING (40-50)
# CONSTRUCTION (60)
# MANUFACTURING (100-392)
# TRANSPORTATION, COMMUNICATIONS, AND OTHER PUBLIC UTILITIES (400-472)
# WHOLESALE TRADE (500-571)
# RETAIL TRADE (580-691)
# FINANCE, INSURANCE, AND REAL ESTATE (700-712)
# BUSINESS AND REPAIR SERVICES (721-760)
# PERSONAL SERVICES (761-791)
# ENTERTAINMENT AND RECREATION SERVICES (800-810)
# PROFESSIONAL AND RELATED SERVICES (812-893)
# PUBLIC ADMINISTRATION (900-932)
# MILITARY, DID NOT RESPOND, AND UNEMPLOYED (940-999)

data_clean <- data_clean %>% 
  filter(ind1990 < 940) %>% 
  mutate(ind1990_agg = case_match(ind1990, 
                                  10:32 ~ "Agriculture, Forestry, And Fisheries",
                                  40:50 ~ "Mining", 
                                  60 ~ "Construction",
                                  100:392 ~ "Manufacturing",
                                  400:472 ~ "Transportation, Communications, And Other Public Utilities",
                                  500:571 ~ "Wholesale Trade",
                                  580:691 ~ "Retail Trade",
                                  700:712 ~ "Finance, Insurance, And Real Estate",
                                  721:760 ~ "Business And Repair Services",
                                  761:791 ~ "Personal Services",
                                  800:810 ~ "Entertainment And Recreation Services",
                                  812:893 ~ "Professional And Related Services",
                                  900:932 ~ "Public Administration"))


# Collect puma equivalent geographic variables
data_clean <- data_clean %>% 
  mutate(
    cntygp98 = str_pad(cntygp98, width = 3, pad = "0"),
    puma = ifelse(year %in% c(1990,2000,2007), str_pad(puma, width = 4, pad = "0"), puma),
    puma = ifelse(year > 2012, str_pad(puma, width = 5, pad = "0"), puma)
  ) %>%
  mutate(cntygp98_full = ifelse(year == 1980, str_c(statefip, cntygp98), cntygp98),
         puma_full = ifelse(year > 1980, str_c(statefip, puma), puma)) %>%
  mutate(cntygp98_full = as.numeric(cntygp98_full), puma_full = as.numeric(puma_full)) %>%
  mutate(puma_equiv = rowSums(across(c(cntygp97, cntygp98_full, puma_full)), na.rm = TRUE))


# run a regression (Adj. R2: around 0.40 for all)
wagereg_1 <- feols(hourly_wage ~ 0 | year^puma_equiv + year^sex^marst^educ_simple [exper, exper^2] +
                   year^race + year^hispan_ind + year^immigrant^educ_simple [yrsusa, yrsusa^2] 
                   + year^ind1990_agg + year^occ1990_agg,
                 weights = ~ perwt, combine.quick = F, data_clean %>% filter(!is.na(yrimmig)))

wagereg_2 <- feols(hourly_wage ~ 0 | year^puma_equiv + year^sex^marst^educ_simple [exper, exper^2] + year^sex^marst +
                     year^race + year^hispan_ind + year^ind1990_agg + year^occ1990_agg,
                   weights = ~ perwt, combine.quick = F, data_clean %>% filter(immigrant == 0))

wagereg_3 <- feols(hourly_wage ~ 0 | year^puma_equiv + year^educ_simple [exper, exper^2] +
                     year^race + year^hispan_ind + year^ind1990_agg + year^occ1990_agg,
                   weights = ~ perwt, combine.quick = F, data_clean %>% filter(immigrant == 0, sex == 1))

wagereg_4 <- feols(hourly_wage ~ 0 | year^puma_equiv + year^educ_simple [log(exper)] +
                     year^race + year^hispan_ind + year^ind1990_agg + year^occ1990_agg,
                   weights = ~ perwt, combine.quick = F, data_clean %>% filter(immigrant == 0, sex == 1))

wagereg_5 <- feols(hourly_wage ~ 0 | year^puma_equiv^educ_simple [exper, exper^2] +
                     year^race + year^hispan_ind + year^ind1990_agg + year^occ1990_agg,
                   weights = ~ perwt, combine.quick = F, data_clean %>% filter(immigrant == 0, sex == 1))

wagereg_6 <- feols(hourly_wage ~ 0 | year^puma_equiv^educ_simple [exper, exper^2],
                   weights = ~ perwt, combine.quick = F, 
                   data_clean %>% filter(immigrant == 0, sex == 1))


summary(wagereg_1) # adj R2 = 0.42
summary(wagereg_2) # adj R2 = 0.43
summary(wagereg_3) # adj R2 = 0.41
summary(wagereg_4) # adj R2 = 0.41
summary(wagereg_5) # adj R2 = 0.43
summary(wagereg_6) # adj R2 = 0.41

# save fixed effects 
fe_wage <- fixef(wagereg_1)
fe_wage_nat <- fixef(wagereg_2)
fe_wage_nat_male <- fixef(wagereg_3)
fe_wage_nat_male_log_spec <- fixef(wagereg_4)
fe_wage_educ_interacted <- fixef(wagereg_5)
fe_wage_educ_interacted_less_controls <- fixef(wagereg_6)

# choose preferred specification and isolate year X puma fixed effects

# wage_year_puma <- enframe(fe_wage_nat_male$`year^puma_equiv`, name = "year_eqvpuma", value = "wage")
# wage_year_puma <- wage_year_puma %>%
#   separate(year_eqvpuma, into = c("year", "puma_equiv", "educ"), sep = "_")

wage_year_puma <- enframe(fe_wage_educ_interacted_less_controls$`year^puma_equiv^educ_simple`, name = "year_eqvpuma_educ", value = "wage")
wage_year_puma <- wage_year_puma %>%
  separate(year_eqvpuma_educ, into = c("year", "puma_equiv", "educ"), sep = "_")

# merge pumas to czones
wage_year_puma_czone <- wage_year_puma %>% 
  mutate(puma_equiv = as.integer(puma_equiv), year = as.integer(year)) %>% 
  arrange(year, puma_equiv) %>% 
  left_join(puma_equiv_czone_cross, join_by(year, puma_equiv))

wage_year_puma_czone[is.na(wage_year_puma_czone$czone), ] # one observation in 2007 unmatched corresponding to pumas in Louisiana with large pop outflows in the aftermath of Hurrican Katrina; corresponding pumas recoded to 2277777

# correct for Katrina affected pumas with puma code changes in 2006 and 2007 vs 2005
wage_year_puma_czone <- wage_year_puma_czone %>% 
  mutate(wage = ifelse(puma_equiv %in% c(221801, 221802, 221905) & year == 2007,
                       wage_year_puma_czone$wage[wage_year_puma_czone$puma_equiv==2277777 & wage_year_puma_czone$year == 2007],
                       wage))

# puma codes were changed starting 2012, so people interviewed in 2011 had the old codes and people interviewed afterward had the new ones
wage_year_puma_czone[is.na(wage_year_puma_czone$czone), ] # one observation in 2007 unmatched corresponding to pumas in Louisiana with large pop outflows in the aftermath of Hurrican Katrina; corresponding pumas recoded to 2277777

# solution: drop old codes
wage_year_puma_czone_clean <- wage_year_puma_czone %>% filter(!is.na(czone))

# aggregate to czone level

wage_year_czone <- wage_year_puma_czone_clean %>%
  # group_by(year, czone) %>%
  group_by(year, educ, czone) %>%
  summarize(wage = stats::weighted.mean(wage, w = afactor))

wage_year_czone[is.na(wage_year_czone$wage), ] # all matched

# visualize the wage fixed effects across czones by year
p <- ggplot(wage_year_czone, aes(x=wage)) + 
  # geom_histogram() +
  scale_fill_brewer(palette = "Set2") +
  geom_histogram(aes(color = educ)) +
  theme_minimal()+
  facet_wrap(~ year)

p2 <- ggplot(wage_year_czone %>% filter(educ==1), aes(x=wage)) + 
  geom_histogram() +
  theme_bw()+
  facet_wrap(~ year) +
  labs(x = "Hourly Wage Fixed Effect", y = "Number of Commuting Zones")

ggsave("Output/Figures/wage_fixed_effects_educ1.png",
       height = 4, width = 7, units = 'in', p2)

# # chatgpt suggestion
# okabe_ito <- c(
#   "1" = "#E69F00",  # orange
#   "2" = "#56B4E9",  # blue
#   "3" = "#009E73"  # green
# )
# 
# ggplot(wage_year_czone, aes(x = wage)) +
#   geom_histogram(aes(fill = educ), position = "identity", alpha = 0.6, bins = 30) +
#   facet_wrap(~ year, scales = "free_y") +
#   scale_fill_manual(values = okabe_ito) +
#   theme_bw() +
#   labs(
#     title = "Distribution of Wage Fixed Effects by Education and Year",
#     x = "Wage Fixed Effect",
#     y = "Number of CZones",
#     fill = "Education Level"
#   )

write_dta(wage_year_czone, "Data/Clean/wage_fixed_effects_czone_year.dta")
write_dta(wage_year_puma, "Data/Clean/wage_fixed_effects_puma_equiv_year.dta")


