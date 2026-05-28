rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(sf)

source("Code/functions.R")

# load shapefiles
state_sf <- readRDS("Data/Clean/state_sf.RDS")
us_sf <- readRDS("Data/Clean/us_sf.RDS")
czone_sf <- readRDS("Data/Clean/czone_sf.RDS")

# centroids, distance

czone_centroids <- st_centroid(czone_sf)
dist_matrix <- st_distance(czone_centroids)
dist_matrix <- units::drop_units(dist_matrix)

dist_df <- czone_centroids$czone %>% bind_cols(as_tibble(dist_matrix))
names(dist_df) <- c("czone", paste0("dist_cz", czone_centroids$czone))

# pivot longer + convert distance units to km
dist_df <- dist_df %>% 
  rename(refer_cz = czone) %>% 
  pivot_longer(starts_with("dist"),names_to = "dest_cz", names_prefix = "dist_cz", values_to = "dist") %>% 
  mutate(dist = dist / 1000)
  

# spatial linkage parameters
decay_par <- 1
threshold <- 500

# function that computes inverse distance within threshold
# + normalized version by sum of inverse distance within threshold

invdist_function <- function(dist_df, threshold, decay_par) {
  
  dist_df <- dist_df %>% 
    mutate(within_threshold = ifelse(dist > 0 & dist < threshold, 1, 0)) %>% 
    mutate(threshold_idw = ifelse(refer_cz != dest_cz, (within_threshold / dist)^decay_par, 0)) %>% 
    group_by(refer_cz) %>% 
    mutate(threshold_idw_norm = threshold_idw / sum(threshold_idw)) %>% 
    ungroup()
  
  return(dist_df)
  
}

dist_df <- invdist_function(dist_df, threshold, decay_par)

# Plot

selected_cz <- czone_centroids[500, ]
selected_buffer <- st_buffer(selected_cz, dist = threshold * 1000) 

p <- ggplot() +
  geom_sf(data = czone_sf, fill = "NA",
          color = "grey40", linewidth = 0.5, linetype = 'solid')+
  geom_sf(data = selected_cz, color = "blue", size = 1.2) +
  geom_sf(data = selected_buffer, fill = "red", alpha = 0.2)+
  theme_bw()

p

ggsave("Output/Figures/spatial_linkage_threshold.png",
       height = 4, width = 7, units = 'in', p)

write_dta(dist_df, "Data/Clean/czone_spatial_linkage_measures.dta")
