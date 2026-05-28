rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(jsonlite)
library(httr)
library(haven)
library(tigris)
library(countrycode)

source("Code/functions.R")

# world bank cckp state codes
wb_climate_statecodes <- read_csv("Data/Raw/Dictionaries/wb_climate_statecodes.txt", 
                                  col_names = c('state_id', 'state_name')) %>% 
  mutate(state_id = str_sub(state_id, 5, nchar(state_id)))

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

# US states
res_precip_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_pr_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_humidity_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_hurs_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_pop_tot_states <- GET("https://cckpapi.worldbank.org/cckp/v1/pop-x1_timeseries_popcount_timeseries_annual_2000-2000_mean_historical_gpw-v4_rev11_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_pop_density_states <- GET("https://cckpapi.worldbank.org/cckp/v1/pop-x1_timeseries_popdensity_timeseries_annual_2000-2000_mean_historical_gpw-v4_rev11_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_temp_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tas_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_temp_max_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tasmax_timeseries_monthly_1950-2023_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_temp_min_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tasmin_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")

# parsed data

parsed_precip_states <- fromJSON(content(res_precip_states, "text"))
parsed_humidity_states <- fromJSON(content(res_humidity_states, "text"))
parsed_pop_tot_states <- fromJSON(content(res_pop_tot_states, "text"))
parsed_pop_density_states <- fromJSON(content(res_pop_density_states, "text"))
parsed_temp_states <- fromJSON(content(res_temp_states, "text"))
parsed_temp_max_states <- fromJSON(content(res_temp_max_states, "text"))
parsed_temp_min_states <- fromJSON(content(res_temp_min_states, "text"))

# extract data

data_precip_states <- enframe(rapply(parsed_precip_states$data, unlist), value = 'precip') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_humidity_states <- enframe(rapply(parsed_humidity_states$data, unlist), value = 'humidity') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_pop_tot_states <- enframe(rapply(parsed_pop_tot_states$data, unlist), value = 'pop_tot') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_pop_density_states <- enframe(rapply(parsed_pop_density_states$data, unlist), value = 'pop_density') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_temp_states <- enframe(rapply(parsed_temp_states$data, unlist), value = 'temp') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_temp_max_states <- enframe(rapply(parsed_temp_max_states$data, unlist), value = 'temp_max') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") %>% 
  filter(year != 2023) # api for 1950-2023 used since 1950-2022 version broken for this series only

data_temp_min_states <- enframe(rapply(parsed_temp_min_states$data, unlist), value = 'temp_min') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

# merge
data_states <- data_precip_states %>% 
  left_join(data_humidity_states) %>% 
  left_join(data_temp_states) %>% 
  left_join(data_temp_max_states) %>% 
  left_join(data_temp_min_states)

data_pop_states <- data_pop_tot_states %>% 
  left_join(data_pop_density_states)

# attach state names
data_states <- data_states %>% 
  left_join(wb_climate_statecodes) %>% 
  left_join(states) %>% 
  filter(statefp %notin% c('02','15'))

data_pop_states <- data_pop_states %>% 
  left_join(wb_climate_statecodes) %>% 
  left_join(states) %>% 
  filter(statefp %notin% c('02','15'))

data_states <- data_states %>% 
  select(state_name, statefp, year, month, temp, temp_max, temp_min, precip, humidity)

data_pop_states <- data_pop_states %>% 
  select(state_name, statefp, year, month, pop_tot, pop_density)

# create measure of temperature max-min range
data_states <- data_states %>% 
  mutate(temp_range = temp_max - temp_min, .after = temp_min)

write_dta(data_states, 'Data/Clean/climate_state.dta')
write_dta(data_pop_states, 'Data/Clean/pop_2000_state.dta')
