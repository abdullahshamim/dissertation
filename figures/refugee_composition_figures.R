rm(list = ls())

# Set working directory
setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(haven)
library(tidyverse)
library(scales)
library(wesanderson)

source("Code/functions.R")
options(scipen = 500000)

refarrivals_raw <- read_dta("Data/Raw/RefArrivals_dreher-langlotz-matzat-parsons/orr_prm_1975_2018_v1.dta")

refarrivals_country_year <- refarrivals_raw %>% 
  mutate(citizenship_stable = str_to_title(citizenship_stable)) %>% 
  mutate(citizenship_stable = ifelse(citizenship_stable=='Ussr', 'USSR', citizenship_stable)) %>% 
  filter(year %in% 1975:2008) %>% 
  filter(statefp10 %notin% c("02","15")) %>% 
  group_by(citizenship_stable, year) %>% 
  summarize(tot_refugees_country_year = sum(refugees))

refarrivals_country <- refarrivals_country_year %>% 
  group_by(citizenship_stable) %>% 
  summarize(tot_refugees_country = sum(tot_refugees_country_year)) %>% 
  arrange(desc(tot_refugees_country))

refarrivals_year <-  refarrivals_country_year %>% 
  group_by(year) %>% 
  summarize(tot_refugees_year = sum(tot_refugees_country_year))

# separate top 16 refugee sending countries from all others
refarrivals_country_trunc <- refarrivals_country %>%
  mutate(refugee_sender_rank = row_number()) %>%
  mutate(refugee_sender_rank = ifelse(refugee_sender_rank > 16, 17, refugee_sender_rank)) %>%
  mutate(citizenship_stable = ifelse(refugee_sender_rank == 17, "Other", citizenship_stable)) %>%
  group_by(citizenship_stable) %>%
  summarize(tot_refugees_country = sum(tot_refugees_country)) %>%
  arrange(desc(tot_refugees_country)) %>% print(n=17)

# other category captures less than 4 percent of tot refugee arrivals
refarrivals_country_trunc$tot_refugees_country[refarrivals_country_trunc$citizenship_stable == 'other'] /
  sum(refarrivals_country_trunc$tot_refugees_country)

# # separate top 11 refugee sending countries from all others
# refarrivals_country_trunc <- refarrivals_country %>% 
#   mutate(refugee_sender_rank = row_number()) %>% 
#   mutate(refugee_sender_rank = ifelse(refugee_sender_rank > 11, 12, refugee_sender_rank)) %>% 
#   mutate(citizenship_stable = ifelse(refugee_sender_rank == 12, "Other", citizenship_stable)) %>% 
#   group_by(citizenship_stable) %>% 
#   summarize(tot_refugees_country = sum(tot_refugees_country)) %>% 
#   arrange(desc(tot_refugees_country))
# 
# # other category captures less than 10 percent of tot refugee arrivals
# refarrivals_country_trunc$tot_refugees_country[refarrivals_country_trunc$citizenship_stable == 'other'] / 
#   sum(refarrivals_country_trunc$tot_refugees_country) 

refarrivals_country_year_trunc <- refarrivals_country_year %>% 
  mutate(citizenship_stable = 
           ifelse(
             citizenship_stable %in% refarrivals_country_trunc$citizenship_stable,
             citizenship_stable,
             "Other")) %>% 
  group_by(citizenship_stable,year) %>% 
  summarise(tot_refugees_country_year = sum(tot_refugees_country_year))

cols <- c("#caa331",
          "#6a70d7",
          "#91b23e",
          "#583788",
          "#82bc64",
          "#be72c9",
          "#51bb76",
          "#892863",
          "#43c8ac",
          "#c24c6e",
          "#4f7124",
          "#d56cad",
          "#b9a34c",
          "#618fd8",
          "#c57329",
          "#b8473e",
          "#b87944")

c17 <- rev(c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1"))

plot_refarrivals_comp_levels <- ggplot() +
  # Stacked area chart
  geom_area(data = refarrivals_country_year_trunc, 
            aes(x = year, y = tot_refugees_country_year, fill = citizenship_stable),
            alpha = 0.8) + 
  # Overlayed line for total arrivals
  geom_line(data = refarrivals_year, 
            aes(x = year, y = tot_refugees_year), 
            color = "black", size = 1) +
  scale_y_continuous(labels = label_number(suffix = "k", scale = 1e-3)) +
  scale_fill_manual(values = c17,
                    breaks = unique(refarrivals_country_year_trunc$citizenship_stable)) +  
  # Labels and theme
  labs(x = NULL,
       y = "Refugees Resettled (1k = 1,000)",
       fill = "") +
  theme_bw() + 
  theme(legend.position = 'top',
        legend.key.size = unit(0.3, "cm")) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))+
  guides(fill = guide_legend(nrow = 3))

ggsave("Output/Figures/refarrivals_composition_levels.png",
       height = 5, width = 7, units = "in", plot_refarrivals_comp_levels)

refarrivals_freq_country_year_trunc <- refarrivals_country_year_trunc %>% 
  left_join(refarrivals_year) %>% 
  mutate(tot_refugees_country_year = tot_refugees_country_year / tot_refugees_year)

plot_refarrivals_comp_share <- ggplot() +
  # Stacked area chart
  geom_area(data = refarrivals_freq_country_year_trunc, 
            aes(x = year, y = tot_refugees_country_year, fill = citizenship_stable, 
                color = citizenship_stable),  # add color outline
            alpha = 0.8, size = 0.2) + 
  scale_fill_manual(values = c17,
                    breaks = unique(refarrivals_country_year_trunc$citizenship_stable)) +
  # Labels and theme
  labs(x = NULL,
       y = "Refugees Resettlement Share",
       fill = "") +
  theme_bw() + 
  theme(legend.position = 'top',
        legend.key.size = unit(0.3, "cm")) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))+
  guides(fill = guide_legend(nrow = 3), color = 'none')

ggsave("Output/Figures/refarrivals_composition_share.png",
       width = 7, height = 4, units = "in", plot_refarrivals_comp_share)

refarrivals_composition_country_year_trunc <- refarrivals_country_year_trunc %>% 
  left_join(refarrivals_year) %>% 
  mutate(tot_refugees_country_year = tot_refugees_country_year / tot_refugees_year)

a <- refarrivals_country_year_trunc %>% 
  ungroup() %>% 
  complete(citizenship_stable,year)

aa <- a %>% 
  mutate(tot_refugees_country_year = ifelse(is.na(tot_refugees_country_year), 0, tot_refugees_country_year)) %>% 
  group_by(citizenship_stable) %>% 
  mutate(tot_cum_refugees_country_year = cumsum(tot_refugees_country_year))

aaa <- aa %>% left_join(refarrivals_year) %>% 
  group_by(citizenship_stable) %>% 
  mutate(cum_tot_refugees_year = cumsum(tot_refugees_year)) %>% 
  mutate(refugee_country_composition = tot_cum_refugees_country_year / cum_tot_refugees_year)

plot_refarrivals_comp_cumshare <- ggplot() +
  # Stacked area chart
  geom_area(data = aaa, 
            aes(x = year, y = refugee_country_composition,
                fill = citizenship_stable, color = citizenship_stable),
            alpha = 0.8, size = 0.2) + 
  # # Overlayed line for total arrivals
  # geom_line(data = refarrivals_year, 
  #           aes(x = year, y = tot_refugees_year), 
  #           color = "black", size = 1) +
  # scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
  scale_fill_manual(values = c17,
                    breaks = unique(aaa$citizenship_stable)) +  
  # Labels and theme
  labs(x = NULL,
       y = "Cumulative Refugees Resettlement Share",
       fill = "") +
  theme_bw() + 
  theme(legend.position = 'top',
        legend.key.size = unit(0.3, "cm")) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))+
  guides(fill = guide_legend(nrow = 3), color = 'none')

ggsave("Output/Figures/refarrivals_composition_cumshare.png",
       width = 7, height = 4, units = "in", plot_refarrivals_comp_cumshare)
