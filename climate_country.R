rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(jsonlite)
library(httr)
library(haven)
library(tigris)
library(countrycode)

source("Code/functions.R")

# world climate data
res_precip <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_pr_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")
res_humidity <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_hurs_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")
res_pop_tot <- GET("https://cckpapi.worldbank.org/cckp/v1/pop-x1_timeseries_popcount_timeseries_annual_2000-2000_mean_historical_gpw-v4_rev11_mean/all_countries?_format=json")
res_pop_density <- GET("https://cckpapi.worldbank.org/cckp/v1/pop-x1_timeseries_popdensity_timeseries_annual_2000-2000_mean_historical_gpw-v4_rev11_mean/all_countries?_format=json")
res_temp <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tas_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")
res_temp_max <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tasmax_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")
res_temp_min <- GET("https://cckpapi.worldbank.org/cckp/v1/era5-x0.25_timeseries_tasmin_timeseries_monthly_1950-2022_mean_historical_era5_x0.25_mean/all_countries?_format=json")

# parsed data
parsed_precip <- fromJSON(content(res_precip, "text"))
parsed_humidity <- fromJSON(content(res_humidity, "text"))
parsed_pop_tot <- fromJSON(content(res_pop_tot, "text"))
parsed_pop_density <- fromJSON(content(res_pop_density, "text"))
parsed_temp <- fromJSON(content(res_temp, "text"))
parsed_temp_max <- fromJSON(content(res_temp_max, "text"))
parsed_temp_min <- fromJSON(content(res_temp_min, "text"))

# extract vars
data_precip <- enframe(rapply(parsed_precip$data, unlist), value = 'precip') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_humidity <- enframe(rapply(parsed_humidity$data, unlist), value = 'humidity') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_pop_tot <- enframe(rapply(parsed_pop_tot$data, unlist), value = 'pop_tot') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_pop_density <- enframe(rapply(parsed_pop_density$data, unlist), value = 'pop_density') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_temp <- enframe(rapply(parsed_temp$data, unlist), value = 'temp') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

data_temp_max <- enframe(rapply(parsed_temp_max$data, unlist), value = 'temp_max') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-")

data_temp_min <- enframe(rapply(parsed_temp_min$data, unlist), value = 'temp_min') %>% 
  separate(name, into = c('iso3','period'), sep = '\\.') %>% 
  separate(period, into = c('year','month'), sep = "-") 

# merge
data <- data_precip %>% 
  left_join(data_humidity) %>% 
  left_join(data_temp) %>% 
  left_join(data_temp_max) %>% 
  left_join(data_temp_min)

data <- data %>%
  mutate(country_name = countrycode::countrycode(iso3, "iso3c", "country.name")) %>% 
  mutate(country_name = ifelse(iso3 == 'KSV', "Kosovo", country_name)) %>% 
  relocate(country_name, .before = iso3) %>% 
  relocate(c(precip,humidity), .after = temp_min)

data_pop_2000 <- data_pop_tot %>% 
  left_join(data_pop_density) %>% 
  mutate(country_name = countrycode::countrycode(iso3, "iso3c", "country.name")) %>% 
  mutate(country_name = ifelse(iso3 == 'KSV', "Kosovo", country_name)) %>% 
  relocate(country_name, .before = iso3)

# create measure of temperature max-min range
data <- data %>% 
  mutate(temp_range = temp_max - temp_min, .after = temp_min)

write_dta(data, 'Data/Clean/climate_country.dta')
write_dta(data_pop_2000, 'Data/Clean/pop_2000_country.dta')
