rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(haven)
library(tidyverse)

# read bea emp data
bea_1969_2000 <- read_csv("Data/Raw/BEA_Emp/CAEMP25S__ALL_AREAS_1969_2000.csv")
bea_2001_2020 <- read_csv("Data/Raw/BEA_Emp/CAEMP25N__ALL_AREAS_2001_2020.csv")

# read and clean county czone crosswalk
county_cz_cross <- read_dta("Data/Raw/Geography_Crosswalks/Dorn_CommutingZones/cw_cty_czone/cw_cty_czone.dta")

county_cz_cross_1 <- county_cz_cross %>% 
  rename(fp = cty_fips) %>% 
  mutate(fp = str_pad(fp, width=5, pad="0")) %>% 
  mutate(statefp=substr(fp, 1, 2), countyfp=substr(fp, 3, 5))

# clean

# 1969-2000

bea_1969_2000 <- bea_1969_2000 %>% 
  rename(fp = GeoFIPS) %>% 
  mutate(statefp = substr(fp, 1, 2), countyfp = substr(fp, 3, 5))

bea_1969_2000 <- bea_1969_2000 %>% 
  filter(countyfp != '000') %>% 
  filter(LineCode == '10') %>% 
  filter(statefp != '02', statefp != '15', statefp != '72') %>% 
  select(fp,statefp,countyfp, GeoName, `1969`:`2000`)

# 2001-2020

bea_2001_2020 <- bea_2001_2020 %>% 
  rename(fp = GeoFIPS) %>% 
  mutate(statefp = substr(fp, 1, 2), countyfp = substr(fp, 3, 5))

bea_2001_2020 <- bea_2001_2020 %>% 
  filter(countyfp != '000') %>% 
  filter(LineCode == '10') %>% 
  filter(statefp != '02', statefp != '15', statefp != '72') %>% 
  select(fp,statefp,countyfp, GeoName, `2001`:`2020`)

# merge data both periods
bea_1969_2020 <- bea_1969_2000 %>% 
  left_join(bea_2001_2020, join_by(fp,statefp,countyfp))

# pivot longer
bea_1969_2020_long <- bea_1969_2020 %>% 
  pivot_longer(`1969`:`2020`, names_to = 'year', values_to = "emp_tot")

# merge czones
bea_1969_2020_long %>% 
  left_join(county_cz_cross_1) %>% 
  filter(is.na(czone)) %>% 
  count(fp,GeoName.x) %>% 
  print(n=30)

# corrections:

#   | SEER Code | Combined Counties and Cities              | Component FIPS Codes (State=51) |
#   | --------- | ----------------------------------------- | ------------------------------- |
#   | 51901     | Albemarle + Charlottesville               | 51003, 51540                    |
#   | 51903     | Alleghany + Covington                     | 51005, 51580                    |
#   | 51907     | Augusta + Staunton + Waynesboro           | 51015, 51790, 51820             |
#   | 51911     | Campbell + Lynchburg                      | 51031, 51680                    |
#   | 51913     | Carroll + Galax                           | 51035, 51640                    |
#   | 51918     | Dinwiddie + Colonial Heights + Petersburg | 51053, 51570, 51730             |
#   | 51919     | Fairfax + Fairfax City + Falls Church     | 51059, 51600, 51610             |
#   | 51921     | Frederick + Winchester                    | 51069, 51840                    |
#   | 51923     | Greensville + Emporia                     | 51081, 51595                    |
#   | 51929     | Henry + Martinsville                      | 51089, 51690                    |
#   | 51931     | James City + Williamsburg                 | 51095, 51830                    |
#   | 51933     | Montgomery + Radford                      | 51121, 51750                    |
#   | 51939     | Pittsylvania + Danville                   | 51143, 51590                    |
#   | 51941     | Prince George + Hopewell                  | 51149, 51670                    |
#   | 51942     | Prince William + Manassas + Manassas Park | 51153, 51683, 51685             |
#   | 51944     | Roanoke + Salem                           | 51161, 51775                    |
#   | 51945     | Rockbridge + Buena Vista + Lexington      | 51163, 51530, 51678             |
#   | 51947     | Rockingham + Harrisonburg                 | 51165, 51660                    |
#   | 51949     | Southampton + Franklin                    | 51175, 51620                    |
#   | 51951     | Spotsylvania + Fredericksburg             | 51177, 51630                    |
#   | 51953     | Washington + Bristol                      | 51191, 51520                    |
#   | 51955     | Wise + Norton                             | 51195, 51720                    |
#   | 51958     | York + Poquoson                           | 51199, 51735                    |
#   

## corrections from seer website: https://seer.cancer.gov/seerstat/variables/countyattribs/ruralurban.html?utm_source=chatgpt.com
## 1: from seer website - county-groupings to county
## 2: from David Dorn's county-czone crosswalk, assign czones

# 51901 - 17600
# 51903 - 17300
# 51907 - 17300
# 51911 - 2300
# 51913 - 602
# 51918 - 2400
# 51919 - 11304
# 51921 - 17502
# 51923 - 2600
# 51929 - 402
# 51931 - 2500
# 51933 - 16600
# 51939 - 500
# 51941 - 2400
# 51942 - 11304
# 51944 - 16600
# 51945 - 17300
# 51947 - 17200
# 51949 - 2500
# 51951 - 11301
# 51953 - 100
# 51955 - 11203
# 51958 - 2500

# Wisconsin
# 55901 - 22602 # 55901 contains Shawano County (FIPS 55115) and Menominee County (FIPS 55078)

# Miami-Dade county old code: 12025
# Oglala-Lakota county old code: 46113

bea_1969_2020_long$fp[bea_1969_2020_long$fp==12086] <- 12025 # Miami-Dade assigned old code
bea_1969_2020_long$fp[bea_1969_2020_long$fp==46102] <- 46113 # Oglala-Lakota assigned old code

bea_1969_2020_long <- bea_1969_2020_long %>% 
  mutate(statefp = substr(fp, 1, 2), countyfp = substr(fp, 3, 5))

# merge czones
bea_1969_2020_long_czone_lvl <- bea_1969_2020_long %>% 
  left_join(county_cz_cross_1)

# assign correct czone for unmatched counties
bea_1969_2020_long_czone_lvl <- bea_1969_2020_long_czone_lvl %>%
  mutate(czone = case_match(fp,
                            "51901" ~ 17600,
                            "51903" ~ 17300,
                            "51907" ~ 17300,
                            "51911" ~ 2300,
                            "51913" ~ 602,
                            "51918" ~ 2400,
                            "51919" ~ 11304,
                            "51921" ~ 17502,
                            "51923" ~ 2600,
                            "51929" ~ 402,
                            "51931" ~ 2500,
                            "51933" ~ 16600,
                            "51939" ~ 500,
                            "51941" ~ 2400,
                            "51942" ~ 11304,
                            "51944" ~ 16600,
                            "51945" ~ 17300,
                            "51947" ~ 17200,
                            "51949" ~ 2500,
                            "51951" ~ 11301,
                            "51953" ~ 100,
                            "51955" ~ 11203,
                            "51958" ~ 2500, # all above for Virginia
                            "55901" ~ 22602, # Wisconsin
                            .default = czone
  ))


bea_1969_2020_long_czone_lvl %>% 
  filter(is.na(czone))

bea_1969_2020_long_czone_lvl <- bea_1969_2020_long_czone_lvl %>% 
  mutate(emp_tot = as.numeric(emp_tot)) %>% 
  group_by(year,czone) %>% 
  summarise(emp_tot = sum(emp_tot, na.rm = T)) # ignores missing values due to county code changes 

bea_1969_2020_long_czone_lvl %>% filter(is.na(czone)) # all matched

write_dta(bea_1969_2020_long_czone_lvl, "Data/Clean/bea_emp_czone.dta")
