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

# centroids, distance, inverse distance
czone_centroids <- st_centroid(czone_sf)
dist_matrix <- st_distance(czone_centroids)
dist_matrix <- units::drop_units(dist_matrix)

# more sensitive spatial interdependencies with distance
decay_par <- 1

# inverse distance with decay parameter
invdist_matrix <- ifelse(dist_matrix > 0, (1/dist_matrix)^decay_par, 0)
invdist_matrix <- invdist_matrix / rowSums(invdist_matrix) # row-standardized

invdist_df <- czone_centroids$czone %>% bind_cols(as_tibble(invdist_matrix))
names(invdist_df) <- c("czone", paste0("invdist_cz", czone_centroids$czone))

# threshold for spatial spillover cutoffs
threshold <- 500 * 1000 # 500 km converted to meters

# matrix coding czones within threshold as ones and calculating inverse distance for the same
threshold_matrix <- ifelse(dist_matrix <= threshold & dist_matrix > 0, 1, 0)
threshold_invdist_matrix <- ifelse(dist_matrix <= threshold & dist_matrix > 0, (1/dist_matrix)^decay_par, 0)
threshold_invdist_matrix <- threshold_invdist_matrix / rowSums(threshold_invdist_matrix) # row-standardized

threshold_invdist_df <- czone_centroids$czone %>% bind_cols(as_tibble(threshold_invdist_matrix))
names(threshold_invdist_df) <- c("czone", paste0("invdist_cz", czone_centroids$czone))

threshold_df <- czone_centroids$czone %>% bind_cols(as_tibble(threshold_matrix))
names(threshold_df) <- c("czone", paste0("in_threshold_cz", czone_centroids$czone))

# pivot longer

invdist_df_long <- invdist_df %>% 
  rename(refer_cz = czone) %>% 
  pivot_longer(starts_with("invdist"),
               names_to = "dest_cz", 
               names_prefix = "invdist_cz", 
               values_to = "invdist")

threshold_invdist_df_long <- threshold_invdist_df %>% 
  rename(refer_cz = czone) %>% 
  pivot_longer(starts_with("invdist"),
               names_to = "dest_cz", 
               names_prefix = "invdist_cz", 
               values_to = "invdist")

threshold_df_long <- threshold_df %>% 
  rename(refer_cz = czone) %>% 
  pivot_longer(starts_with("in_threshold"),
               names_to = "dest_cz", 
               names_prefix = "in_threshold_cz", 
               values_to = "within_threshold")

#### Plot ####

selected_cz <- czone_centroids[500, ]
selected_buffer <- st_buffer(selected_cz, dist = threshold)

# # czone boundaries
# background <- ggplot() +
#   geom_sf(data = czone_sf, fill = NA, color = "grey40", linewidth = 0.2)
# 
# # state boundaries
# state_boundaries <- geom_sf(data = state_sf, fill = "NA",
#                             color = "grey40", linewidth = 0.3, linetype = 'dashed')
# 
# us_boundary <- geom_sf(data = us_sf, fill = "NA",
#                        color = "grey40", linewidth = 0.5, linetype = 'solid')
# 
# czone_boundaries <- geom_sf(data = czone_sf, fill = "NA",
#                             color = "grey40", linewidth = 0.5, linetype = 'solid')

p <- ggplot() +
  geom_sf(data = czone_sf, fill = "NA",
          color = "grey40", linewidth = 0.5, linetype = 'solid')+
  geom_sf(data = selected_cz, color = "blue", size = 1.2) +
  geom_sf(data = selected_buffer, fill = "red", alpha = 0.2)+
  theme_bw()

ggsave("Output/Figures/spatial_linkage_threshold.png",
       height = 4, width = 7, units = 'in', p)

write_dta(invdist_df_long, "Data/Clean/invdist_df_czone.dta")
write_dta(threshold_df_long, "Data/Clean/czone_threshold_matrix.dta")
write_dta(threshold_invdist_df_long, "Data/Clean/invdist_df_czone_within_threshold.dta")
