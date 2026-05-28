rm(list = ls())

library(tidyverse)
library(haven)

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

county_cz_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_cty_czone/cw_cty_czone.dta")

county_cz_cross_1 <- county_cz_cross %>% 
  rename(fp = cty_fips) %>% 
  mutate(fp = str_pad(fp, width=5, pad="0")) %>% 
  mutate(statefp=substr(fp, 1, 2), countyfp=substr(fp, 3, 5))

# Define fixed-width column positions
fwf_positions <- fwf_positions(
  start = c(1, 5, 7, 9, 14, 15, 16, 17, 19),
  end   = c(4, 6, 8, 11, 14, 15, 16, 18, 26),
  col_names = c("year", "state", "statefp", "countyfp",
                "race", "hispanic", "sex", "age", "population")
)

# Read in the file
seer_data <- read_fwf("Data/Raw/SEER/us.1969_2023.20ages.adjusted.txt", 
                      col_positions = fwf_positions,
                      col_types = cols(
                        year = col_integer(),
                        state = col_character(),
                        statefp = col_character(),
                        countyfp = col_character(),
                        race = col_character(),
                        hispanic = col_character(),
                        sex = col_character(),
                        age = col_integer(),
                        population = col_integer()
                      ))

# Filter for ages 20 to 69
seer_working_age_pop <- seer_data %>%
  filter(age %in% 5:13) %>% 
  group_by(year,state,statefp,countyfp) %>% 
  summarise(population = sum(population)) %>% 
  ungroup() %>% 
  filter(statefp != '02', statefp != '15', statefp != '72')

seer_pop <- seer_data %>%
  group_by(year,state,statefp,countyfp) %>% 
  summarise(population = sum(population)) %>% 
  ungroup() %>% 
  filter(statefp != '02', statefp != '15', statefp != '72')

seer_working_age_pop <- seer_working_age_pop %>% 
  mutate(fp = paste0(statefp,countyfp), .after = countyfp)

# incomplete coverage
countycoverage <- seer_working_age_pop %>% count(state,fp) %>% filter(n!=55)

# counties not merged to czone
seer_working_age_pop %>% 
  left_join(county_cz_cross_1) %>% 
  filter(is.na(czone)) %>% 
  count(state,fp)

# 1 AZ    04910    25
# 2 CO    08014    22
# 3 CO    08911    33
# 4 CO    08912    33
# 5 CO    08913    33
# 6 CO    08914    33
# 7 FL    12086    55
# 8 KR    99999     1
# 9 NM    35910    13
# 10 NY    36910    11
# 11 VA    51910    13
# 12 VA    51911    13
# 13 VA    51913    11
# 14 VA    51914    11
# 15 VA    51915    11
# 16 VA    51916    11
# 17 VA    51917    55
# 18 VA    51918    11

## corrections from seer website: https://seer.cancer.gov/seerstat/variables/countyattribs/ruralurban.html?utm_source=chatgpt.com
## 1: from seer website - county-groupings to county
## 2: from David Dorn's county-czone crosswalk, assign czones

# 04910: split into Yuma 106,895 vs. La Paz 13,844 
# 08014: assign czone 28900
# 08911 - 28900
# 08912 - 28900
# 08913 - 28900
# 08914 - 28800
# 12086 - 12025 # Miami-Dade old county code in place of new county code
# 35910 - 34901 # NY City aggregation
# 36910 - 19400
# 51910 - 11304
# 51911 - 2500
# 51913 - 2300
# 51914 - 17200
# 51915 - 17300
# 51916 - 402
# 51917 - 11304
# 51918 - 17300    

# corrections 

# yuma county breakup

yuma_breakup <- seer_working_age_pop %>% filter(fp=="04910")

yuma_breakup <- yuma_breakup %>% 
  mutate(population_04027 = 106895 / (106895 + 13844) * population) %>% # from dorn county change document
  mutate(population_04012 = 13844 / (106895 + 13844) * population) %>% 
  select(-fp,-population) %>% 
  pivot_longer(c(population_04027,population_04012),
               names_to = 'fp', names_prefix = 'population_',
               values_to = 'population') %>% 
  mutate(countyfp = substr(fp, 3, 5), .after = statefp)

seer_working_age_pop <- seer_working_age_pop %>% 
  bind_rows(yuma_breakup) %>% 
  filter(fp!="04910") # removing old yuma code

# miami-dade
seer_working_age_pop$fp[seer_working_age_pop$fp==12086] <- 12025

seer_working_age_pop <- seer_working_age_pop %>% 
  mutate(countyfp = substr(fp, 3, 5), .after = statefp)

# joining czones
seer_working_age_pop_czone_lvl <- seer_working_age_pop %>% 
  left_join(county_cz_cross_1)

# corrections
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='08014'] <- 28900
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='08911'] <- 28900
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='08912'] <- 28900
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='08913'] <- 28900
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='08914'] <- 28800
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='35910'] <- 34901
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='36910'] <- 19400
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='51910'] <- 11304
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='51911'] <- 2500
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='51913'] <- 2300
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='51914'] <- 17200
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='51915'] <- 17300
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='51916'] <- 402
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='51917'] <- 11304
seer_working_age_pop_czone_lvl$czone[seer_working_age_pop_czone_lvl$fp=='51918'] <- 17300

seer_working_age_pop_czone_lvl %>% filter(is.na(czone)) # only missing czone for katrina affected counties (~ 60 counties across multiple states )

seer_working_age_pop_czone_lvl <- seer_working_age_pop_czone_lvl %>% 
  group_by(year,czone) %>% 
  summarize(population = sum(population))

seer_working_age_pop_czone_lvl %>% filter(is.na(czone)) # only missing czone for katrina observation; see above

write_dta(seer_working_age_pop_czone_lvl, "Data/Clean/seer_wrkagepop_czone.dta")
