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
  rename(state_name = name)

# world
res_precip <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_pr_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")
res_humidity <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_hurs_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")
res_pop_tot <- GET("https://cckpapi.worldbank.org/cckp/v1/pop-x1_timeseries_popcount_timeseries_annual_2000-2000_mean_historical_gpw-v4_rev11_mean/all_countries?_format=json")
res_pop_density <- GET("https://cckpapi.worldbank.org/cckp/v1/pop-x1_timeseries_popdensity_timeseries_annual_2000-2000_mean_historical_gpw-v4_rev11_mean/all_countries?_format=json")
res_temp <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tas_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")
res_temp_max <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tasmax_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")
res_temp_min <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tasmin_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")

# US states
res_precip_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_pr_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_humidity_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_hurs_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_pop_tot_states <- GET("https://cckpapi.worldbank.org/cckp/v1/pop-x1_timeseries_popcount_timeseries_annual_2000-2000_mean_historical_gpw-v4_rev11_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_pop_density_states <- GET("https://cckpapi.worldbank.org/cckp/v1/pop-x1_timeseries_popdensity_timeseries_annual_2000-2000_mean_historical_gpw-v4_rev11_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_temp_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tas_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_temp_max_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tasmax_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")
res_temp_min_states <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tasmin_timeseries_seasonal_1950-2022_mean_historical_era5_x0.25_mean/USA,USA.2593214,USA.2593215,USA.2593216,USA.2593217,USA.2593218,USA.2593219,USA.2593220,USA.2593221,USA.2593222,USA.2593223,USA.2593224,USA.2593225,USA.2593226,USA.2593227,USA.2593228,USA.2593229,USA.2593230,USA.2593231,USA.2593232,USA.2593233,USA.2593234,USA.2593235,USA.2593236,USA.2593237,USA.2593238,USA.2593239,USA.2593240,USA.2593241,USA.2593242,USA.2593243,USA.2593244,USA.2593245,USA.2593246,USA.2593247,USA.2593248,USA.2593249,USA.2593250,USA.2593251,USA.2593252,USA.2593253,USA.2593254,USA.2593255,USA.2593256,USA.2593257,USA.2593258,USA.2593259,USA.2593260,USA.2593261,USA.2593262,USA.2593263,USA.2593264?_format=json")


# parsed data
parsed_precip <- fromJSON(content(res_precip, "text"))
parsed_humidity <- fromJSON(content(res_humidity, "text"))
parsed_pop_tot <- fromJSON(content(res_pop_tot, "text"))
parsed_pop_density <- fromJSON(content(res_pop_density, "text"))
parsed_temp <- fromJSON(content(res_temp, "text"))
parsed_temp_max <- fromJSON(content(res_temp_max, "text"))
parsed_temp_min <- fromJSON(content(res_temp_min, "text"))

parsed_precip_states <- fromJSON(content(res_precip_states, "text"))
parsed_humidity_states <- fromJSON(content(res_humidity_states, "text"))
parsed_pop_tot_states <- fromJSON(content(res_pop_tot_states, "text"))
parsed_pop_density_states <- fromJSON(content(res_pop_density_states, "text"))
parsed_temp_states <- fromJSON(content(res_temp_states, "text"))
parsed_temp_max_states <- fromJSON(content(res_temp_max_states, "text"))
parsed_temp_min_states <- fromJSON(content(res_temp_min_states, "text"))


data_precip <- enframe(rapply(parsed_precip$data, unlist), value = 'precip') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3)

data_humidity <- enframe(rapply(parsed_humidity$data, unlist), value = 'humidity') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3)

data_pop_tot <- enframe(rapply(parsed_pop_tot$data, unlist), value = 'pop_tot') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3)

data_pop_density <- enframe(rapply(parsed_pop_density$data, unlist), value = 'pop_density') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3)

data_temp <- enframe(rapply(parsed_temp$data, unlist), value = 'temp') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3)

data_temp_max <- enframe(rapply(parsed_temp_max$data, unlist), value = 'temp_max') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3)

data_temp_min <- enframe(rapply(parsed_temp_min$data, unlist), value = 'temp_min') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3)

# states

data_precip_states <- enframe(rapply(parsed_precip_states$data, unlist), value = 'precip') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3) %>% 
  mutate(quarter = round(quarter))

data_humidity_states <- enframe(rapply(parsed_humidity_states$data, unlist), value = 'humidity') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3) %>% 
  mutate(quarter = round(quarter))

data_pop_tot_states <- enframe(rapply(parsed_pop_tot_states$data, unlist), value = 'pop_tot') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3) %>% 
  mutate(quarter = round(quarter))

data_pop_density_states <- enframe(rapply(parsed_pop_density_states$data, unlist), value = 'pop_density') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3) %>% 
  mutate(quarter = round(quarter))

data_temp_states <- enframe(rapply(parsed_temp_states$data, unlist), value = 'temp') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3) %>% 
  mutate(quarter = round(quarter))

data_temp_max_states <- enframe(rapply(parsed_temp_max_states$data, unlist), value = 'temp_max') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3) %>% 
  mutate(quarter = round(quarter))

data_temp_min_states <- enframe(rapply(parsed_temp_min_states$data, unlist), value = 'temp_min') %>% 
  filter(nchar(name) > 11) %>% 
  separate(name, into = c('iso3','state_id','period'), sep = '\\.') %>% 
  separate(period, into = c('year','quarter'), sep = "-") %>% 
  mutate(quarter = (as.numeric(quarter) + 2) / 3) %>% 
  mutate(quarter = round(quarter))

# merge
data <- data_precip %>% 
  left_join(data_humidity) %>% 
  left_join(data_temp) %>% 
  left_join(data_temp_max) %>% 
  left_join(data_temp_min)

data_pop <- data_pop_tot %>% 
  left_join(data_pop_density)

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

# simplify climate data by averaging across years

data_country_simple <- data %>% 
  group_by(iso3) %>% 
  summarize(across(c(precip,humidity,temp), mean)) %>% 
  rename(precip_cntry = precip, humidity_cntry = humidity, temp_cntry = temp) %>% 
  mutate(key = 1)

data_states_simple <- data_states %>% 
  group_by(statefp,state_name) %>% 
  summarize(across(c(precip,humidity,temp), mean)) %>% 
  rename(precip_state = precip, humidity_state = humidity, temp_state = temp) %>% 
  mutate(key = 1)

data_country_scaled <- data_country_simple %>% 
  mutate(across(c(precip_cntry,humidity_cntry,temp_cntry), scale))

data_states_scaled <- data_states_simple %>%
  ungroup() %>% 
  mutate(across(c(precip_state,humidity_state,temp_state), scale))

# cartesian product of states and countries

data_country_states <- data_states_scaled %>% 
  inner_join(data_country_scaled, join_by(key))

names(data_country_states) <- gsub("\\[,1\\]", "", names(data_country_states))

data_country_states <- data_country_states %>% 
  mutate(euclid_dist_precip = sqrt((precip_cntry - precip_state)^2)) %>% 
  mutate(euclid_dist_temp = sqrt((temp_cntry - temp_state)^2)) %>% 
  mutate(euclid_dist_humidity = sqrt((humidity_cntry - humidity_state)^2))

data_country_states <- data_country_states %>% 
  mutate(cntry_name = countrycode(data_country_states$iso3, 'iso3c', 'country.name'))

data_country_states <- data_country_states %>% 
  select(statefp,state_name,iso3,cntry_name,
         precip_state,precip_cntry,precip_euclid_dist = euclid_dist_precip, 
         temp_state, temp_cntry,temp_euclid_dist = euclid_dist_temp,
         humidity_state,humidity_cntry,humidity_euclid_dist =euclid_dist_humidity) %>% 
  mutate(cntry_name = ifelse(iso3 == 'KSV', 'Kosovo', cntry_name))

write_dta(data_country_states, "Data/Clean/state_country_climate_similarity.dta")

write_dta(data, "Data/Clean/country_climate.dta")
write_dta(data_pop, "Data/Clean/country_pop.dta")

write_dta(data_states, "Data/Clean/USA_state_climate.dta")
write_dta(data_pop_states, "Data/Clean/USA_state_pop.dta")
