rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(tigris)
library(sf)

source("Code/functions.R")

options(tigris_use_cache = TRUE)

# read affiliate address data
affiliate_directory <- read_csv("InstitutionalDetails/WRAPS_archive/local_affiliate_directory.csv")

resettlement_agencies <- tibble(
  name = c(
    "Church World Service (CWS)",
    "Lutheran Immigration & Refugee Service (LIRS)",
    "Domestic & Foreign Missionary Society (DFMS)",
    "U. S. Committee for Refugees and Immigrants (USCRI)",
    "Ethiopian Community Development Council (ECDC)",
    "United States Conference of Catholic Bishops (USCCB)",
    "Hebrew Immigrant Aid Society (HIAS)",
    "World Relief (WR)",
    "International Rescue Committee (IRC)"
  )
) %>%
  mutate(
    abbreviation = str_extract(name, "\\(([^)]+)\\)") |> str_remove_all("[()]"),
    name = str_remove(name, "\\s*\\([^)]*\\)")
  )

affiliate_directory <- affiliate_directory %>% 
  left_join(resettlement_agencies, join_by(parent_organization == abbreviation)) %>% 
  relocate(name, .after = parent_organization) %>% 
  rename(parent_organization_name = name) %>%
  arrange(parent_organization)

# some zip codes do not have tabulation areas (maybe applicable to single commercial address)
# assign nearby zip codes with valid tabulation areas
affiliate_directory$zip_code[affiliate_directory$zip_code == 84110] <- 84101
affiliate_directory$zip_code[affiliate_directory$zip_code == 56302] <- 56304

# remove non-contiguous U.S.
affiliate_directory <- affiliate_directory %>% filter(state %notin% c("HI", "AK", "PR")) # removes one resettlement office in each state/territory

# geographies

affiliate_zipcodes <- tigris::zctas(starts_with = str_sub(affiliate_directory$zip_code, 1, 5), year = 2016) %>% 
  rename_with(tolower) %>% 
  select(zcta5ce10, geometry)

affiliate_jur_boundaries <- st_buffer(affiliate_zipcodes, dist = 160934)

states <- states(year = 2016) %>% 
  rename_with(tolower) %>% 
  select(statefp, stusps, name, geometry) %>% 
  rename(state_name = name) %>% 
  filter(statefp %notin% c('02','15','60','66','69','72','78'))

# attach geographies to affiliate directory

affiliate_zipcode_sf <- affiliate_directory %>% 
  left_join(affiliate_zipcodes, join_by(zip_code == zcta5ce10))

affiliate_jur_boundary_sf <- affiliate_directory %>% 
  left_join(affiliate_jur_boundaries, join_by(zip_code == zcta5ce10))

affiliate_state_sf <-  affiliate_directory %>% 
  left_join(states %>% select(stusps, geometry), join_by(state == stusps))

# get affiliate jurisprudence boundary within state

# jur_boundary_geom <- affiliate_jur_boundary_sf %>% select(geometry) %>% st_as_sf()
# state_geom <- affiliate_state_sf %>% select(geometry) %>% st_as_sf
# 
# affiliate_jur_boundary_state <- st_intersection(jur_boundary_geom, state_geom)

affiliate_jur_boundary_state_sf <- affiliate_jur_boundary_sf %>% 
  left_join(states %>% select(stusps, geometry), join_by(state == stusps)) %>% 
  mutate(id = row_number(), .before = parent_organization)

affiliate_jur_boundary_state_sf <- affiliate_jur_boundary_state_sf%>% 
  group_by(id) %>% 
  mutate(geometry = st_intersection(geometry.x, geometry.y))

# maps
resettlement_network_maps <- ggplot() + 
  geom_sf(data = states, fill = NA) +
  geom_sf(data = affiliate_jur_boundary_state_sf, aes(geometry = geometry), fill = "red", alpha = 0.2) +
  geom_sf(data = affiliate_zipcode_sf, aes(geometry =geometry), color = "blue", size = 1.5) +  # central dots
  facet_wrap(~parent_organization_name)+
  theme_bw()+
  theme(
    strip.text = element_text(size = 5),  # adjust this number to your desired text size
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 8)
  )+
  theme(strip.background = element_rect(linewidth = 0.3))

# display maps
resettlement_network_maps

ggsave(
  filename = "Output/Figures/resettlement_agency_network_maps.png",
  plot = resettlement_network_maps,
  width = 7, height = 4, units = "in"
)
