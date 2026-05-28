rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# state fips codes
states <- as_tibble(states()) %>% 
  rename_with(tolower) %>% 
  select(statefp, name) %>% 
  rename(state_name = name) %>% 
  arrange(statefp)

# state codes from noaa
states_noaa <- states %>% 
  filter(statefp %notin% c('02', '11', '15'), statefp <= '56') %>% 
  mutate(statecode_noaa = str_pad(row_number(statefp), width = 2, pad = '0')) %>% 
  arrange(statefp)

# read raw county climate data

county_temp <- read_table("Data/Raw/county_climate/climdiv-tmpccy-v1.0.0-20250605", col_names = FALSE)
county_precip <- read_table("Data/Raw/county_climate/climdiv-pcpncy-v1.0.0-20250605", col_names = FALSE)
county_tmax <- read_table("Data/Raw/county_climate/climdiv-tmaxcy-v1.0.0-20250605", col_names = FALSE)
county_tmin <- read_table("Data/Raw/county_climate/climdiv-tmincy-v1.0.0-20250605", col_names = FALSE)
county_hddays <- read_table("Data/Raw/county_climate/climdiv-hddccy-v1.0.0-20250605", col_names = FALSE)
county_cddays <- read_table("Data/Raw/county_climate/climdiv-cddccy-v1.0.0-20250605", col_names = FALSE)

# merge and clean

data_counties <- county_temp %>% 
  bind_rows(county_precip) %>% 
  bind_rows(county_tmax) %>% 
  bind_rows(county_tmin) %>% 
  bind_rows(county_hddays) %>% 
  bind_rows(county_cddays) %>% 
  select(-X14)

identifiers <- tibble(
  X1 = data_counties$X1,
  statecode_noaa = str_sub(data_counties$X1, 1, 2),
  countyfp = str_sub(data_counties$X1, 3, 5),
  series = str_sub(data_counties$X1, 6, 7),
  year = str_sub(data_counties$X1, 8, 11)
)

data_counties <- data_counties %>% 
  left_join(identifiers) %>% select(-X1) %>% 
  relocate(c(statecode_noaa,countyfp,series,year), .before = everything()) %>% 
  rename_with( ~ paste('month', 1:12, sep = "_"), .cols = X2:X13) %>% 
  mutate(series = case_match(series,
                             '01' ~ 'precip', '02' ~ 'temp',
                             '27' ~ 'temp_max', '28' ~ 'temp_min',
                             '25' ~ 'hddays', '26' ~ 'cddays')) %>% 
  filter(statecode_noaa <= 48) # filtering out hawaii and alaska


# rearrange county climate data
data_counties <- data_counties %>% 
  pivot_longer(starts_with("month"), names_to = 'month', names_prefix = 'month_', values_to = 'values') %>% 
  pivot_wider(names_from = series, values_from = values)

# attach state fips codes
data_counties <- data_counties %>% 
  left_join(states_noaa) %>% 
  relocate(c(state_name,statefp), .before = everything()) %>% 
  mutate(fp = paste0(statefp,countyfp), .after = countyfp) %>% 
  select(-statecode_noaa)

# correct washington dc fips code; coded in as a county of maryland in noaa data
data_counties <- data_counties %>% 
  mutate(fp = ifelse(fp=='24511', '11001', fp)) %>% 
  mutate(statefp = substr(fp, 1, 2), countyfp = substr(fp, 3, 5)) %>% 
  mutate(state_name = ifelse(statefp=='11', 'District of Columbia', state_name))

# convert measures to metric units
data_counties <- data_counties %>% 
  mutate(across(contains('temp'), ~ (.x - 32) * 5/9 )) %>% # converted to celsius
  mutate(precip = precip * 25.4) # converted from in to mm

# create measure of temperature max-min range
data_counties <- data_counties %>% 
  mutate(temp_range = temp_max - temp_min, .after = temp_min)

write_dta(data_counties, 'Data/Clean/climate_county.dta')
