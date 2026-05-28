rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(fixest)
library(stargazer)
library(sf)

source("Code/functions.R")

state_sf <- readRDS("Data/Clean/state_sf.RDS")
us_sf <- readRDS("Data/Clean/us_sf.RDS")
czone_sf <- readRDS("Data/Clean/czone_sf.RDS")


# do i have to normalize by pop_nat to get quasi-exogenous allocations?

census_years <- c(1980,1990,2000,2013)
refarrival_years <- c(1980,1990,2000,2008)

refpop_czone <- read_dta('Data/Clean/refugee_pop_czone_ipums.dta')
refarrivals_czone <- read_dta('Data/Clean/refugee_arrivals_czone_dreher23.dta')
czone_characteristics <- read_dta('Data/Clean/czone_characteristics_census19702000_ACS520102022.dta')

cum_refarrivals_origin_czone <- refarrivals_czone %>% 
  group_by(citizenship_stable,czone) %>% 
  mutate(cum_refugees = cumsum(refugees)) %>% 
  filter(year %in% refarrival_years) %>% 
  select(-refugees) %>% 
  left_join(czone_sf, join_by(czone))

cum_refarrivals_czone <- cum_refarrivals_origin_czone %>% 
  group_by(year, czone) %>% 
  summarize(cum_refugees = sum(cum_refugees)) 

cum_refarrivals_czone <- cum_refarrivals_czone %>% 
  group_by(year) %>% 
  mutate(cum_refugees_quantile = qcut(cum_refugees, c(0.5,0.75,0.9,0.95,0.99), digits = 0)) %>% 
  left_join(czone_sf)

# countries with older cohorts present in U.S. prior to refugee resettlement
refs_with_ties <- c('cuba','iran','poland','romania','ussr')

cum_refarrivals_czone_woties <- cum_refarrivals_origin_czone %>% 
  filter(citizenship_stable %notin% refs_with_ties) %>% 
  group_by(year, czone) %>% 
  summarize(cum_refugees = sum(cum_refugees)) 

cum_refarrivals_czone_woties <- cum_refarrivals_czone_woties %>% 
  group_by(year) %>% 
  mutate(cum_refugees_quantile = qcut(cum_refugees, c(0.5,0.75,0.9,0.95,0.99), digits = 0)) %>% 
  left_join(czone_sf)

# attach native population and overall foreign born population

czone_pop <- czone_characteristics %>% 
  select(year, czone, pop_tot, pop_nat, pop_fb)

cum_refarrivals_czone <- cum_refarrivals_czone %>% 
  left_join(czone_pop %>% filter(year==2010) %>% select(-year)) %>% 
  rename(pop_tot_2010 = pop_tot, pop_nat_2010 = pop_nat, pop_fb_2010 = pop_fb)

cum_refarrivals_czone <- cum_refarrivals_czone %>% 
  mutate(cum_refugee_share_tot = qcut(cum_refugees / pop_tot_2010, c(0.5,0.75,0.9,0.95,0.99), as_percent = T)) %>% 
  mutate(cum_refugee_share_nat = qcut(cum_refugees / pop_nat_2010, c(0.5,0.75,0.9,0.95,0.99), as_percent = T)) %>% 
  mutate(cum_refugee_share_fb = qcut(cum_refugees / pop_fb_2010, c(0.5,0.75,0.9,0.95,0.99), as_percent = T))

cum_refarrivals_czone_woties <- cum_refarrivals_czone_woties %>% 
  left_join(czone_pop %>% filter(year==2010) %>% select(-year)) %>% 
  rename(pop_tot_2010 = pop_tot, pop_nat_2010 = pop_nat, pop_fb_2010 = pop_fb)

cum_refarrivals_czone_woties <- cum_refarrivals_czone_woties %>% 
  mutate(cum_refugee_share_tot = qcut(cum_refugees / pop_tot_2010, c(0.5,0.75,0.9,0.95,0.99), as_percent = T)) %>% 
  mutate(cum_refugee_share_nat = qcut(cum_refugees / pop_nat_2010, c(0.5,0.75,0.9,0.95,0.99), as_percent = T)) %>% 
  mutate(cum_refugee_share_fb = qcut(cum_refugees / pop_fb_2010, c(0.5,0.75,0.9,0.95,0.99), as_percent = T)) 

#### arrivals by decade ####

# decade_refarrivals_origin_czone <- refarrivals_czone %>% 
#   mutate(decade = case_match(year, 
#                              1975:1980 ~ 1975,
#                              1981:1990 ~ 1980,
#                              1991:2000 ~ 1990,
#                              2001:2008 ~ 2000)) %>% 
#   group_by(decade,citizenship_stable,czone) %>% 
#   mutate(refugees = cumsum(refugees)) %>% 
#   left_join(czone_sf)
# 
# decade_refarrivals_czone <- decade_refarrivals_origin_czone %>% 
#   group_by(decade,czone) %>% 
#   summarize(refugees = sum(refugees))
# 
# decade_refarrivals_czone <- decade_refarrivals_czone %>% 
#   group_by(decade) %>% 
#   mutate(refugees_quantile = cut(refugees, quantile(refugees, c(0.5,0.75,0.9,0.95,0.99)))) %>% 
#   left_join(czone_sf)

#### end arrivals by decade ####

#### graphs ####

# ggplot(cum_refarrivals_czone %>% filter(year == 2008), 
#        aes(x=log(cum_refugees))) + 
#   geom_histogram()

# czone boundaries
background <- ggplot() +
  geom_sf(data = czone_sf, fill = "grey80", color = "grey80", linewidth = 0.2)

# state boundaries
state_boundaries <- geom_sf(data = state_sf, fill = "NA",
                            color = "grey40", linewidth = 0.3, linetype = 'dashed')

us_boundary <- geom_sf(data = us_sf, fill = "NA",
                       color = "grey40", linewidth = 0.5, linetype = 'solid')

# Theme for Labels
label_theme <- theme(
  legend.position = c(0.88, 0.15),  # Inside the plot (x = 70%, y = 15%)
  legend.key.size = unit(0.3, "cm"),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 8),
  legend.background = element_rect(fill = "white",color = "grey50",linewidth = 0.3,linetype = "solid"),
  plot.margin = margin(t = 5, r = 5, b = 5, l = 5, unit = "mm")
)

# raw arrivals

# all refs
plot_allrefs_levels <- background +
  geom_sf(data = cum_refarrivals_czone %>% filter(year == 2008),
          aes(fill = cum_refugees_quantile, geometry = geometry),
          color = 'grey80') +
  state_boundaries + us_boundary +
  scale_fill_brewer(palette = 'Greens', name = NULL) +
  # labs(title = "Refugees Resettled in CZs Between 1975-2008") +
  theme_bw() +
  label_theme +
  guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))


# refs without ties
plot_refs_woties_levels <- background +
  geom_sf(data = cum_refarrivals_czone_woties %>% filter(year == 2008),
          aes(fill = cum_refugees_quantile, geometry = geometry),
          color = 'grey80') +
  state_boundaries + us_boundary +
  scale_fill_brewer(palette = 'Greens', name = NULL) +
  # labs(title = "Refugees (Without Ties) Resettled in CZs Between 1975-2008") +
  theme_bw() +
  label_theme +
  guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))

# as a share of total population

# all refs
plot_allrefs_shares <- background +
  geom_sf(data = cum_refarrivals_czone %>% filter(year == 2008),
          aes(fill = cum_refugee_share_tot, geometry = geometry),
          color = 'grey80') +
  state_boundaries + us_boundary +
  scale_fill_brewer(palette = 'Greens', name = NULL) +
  # labs(title = "Refugees Resettled 1975-2008 Relative to 2010 CZ Population") +
  theme_bw() +
  label_theme +
  guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))

plot_refs_woties_shares <- background +
  geom_sf(data = cum_refarrivals_czone_woties %>% filter(year == 2008),
          aes(fill = cum_refugee_share_tot, geometry = geometry),
          color = 'grey80') +
  state_boundaries + us_boundary +
  scale_fill_brewer(palette = 'Greens', name = NULL) +
  # labs(title = "Refugees (Without Ties) Resettled 1975-2008 Relative to 2010 CZ Population") +
  theme_bw() +
  label_theme +
  guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))

# plot_allrefs_levels
# plot_refs_woties_levels
# plot_allrefs_shares
# plot_refs_woties_shares

# immigrant numbers and shares

immpop <- czone_characteristics %>% 
  group_by(year) %>% 
  # mutate(fbpop_in_thousands = fbpop / 1000) %>% 
  mutate(pop_fb_range = qcut(pop_fb, c(0.5,0.75,0.9,0.95,0.99), digits = 0)) %>% 
  mutate(pop_fb_range_share = qcut(pop_fb / pop_tot, c(0.5,0.75,0.9,0.95,0.99), as_percent = T)) %>% 
  left_join(czone_sf)  

# FB population in levels
plot_immpop_levels <- background +
  geom_sf(data = immpop %>% filter(year == 2010),
          aes(fill = pop_fb_range, geometry = geometry),
          color = 'grey80') +
  state_boundaries + us_boundary +
  scale_fill_brewer(palette = 'Greens', name = NULL) +
  # labs(title = "Foreign-born Population of CZs in 2010") +
  theme_bw() +
  label_theme +
  guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))

# FB intensity
plot_immpop_shares <-background +
  geom_sf(data = immpop %>% filter(year == 2010),
          aes(fill = pop_fb_range_share, geometry = geometry),
          color = 'grey80') +
  state_boundaries + us_boundary +
  scale_fill_brewer(palette = 'Greens', name = NULL) +
  # labs(title = "Foreign-born Intensity of CZs in 2010") +
  theme_bw() +
  label_theme +
  guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))

# plot_immpop_levels
# plot_immpop_shares

# save graphs

# refugee resettlement

ggsave("Output/Figures/Maps/Czone_Level/refresettlement_allrefs_levels.png",
       width = 7, height = 4, units = "in",
       plot_allrefs_levels)

ggsave("Output/Figures/Maps/Czone_Level/resettlement_refs_woties_levels.png",
       width = 7, height = 4, units = "in",
       plot_refs_woties_levels)

ggsave("Output/Figures/Maps/Czone_Level/refresettlement_allrefs_shares.png",
       width = 7, height = 4, units = "in",
       plot_allrefs_shares)

ggsave("Output/Figures/Maps/Czone_Level/resettlement_refs_woties_shares.png",
       width = 7, height = 4, units = "in",
       plot_refs_woties_shares)
       
# immigrant stocks and shares

ggsave("Output/Figures/Maps/Czone_Level/immpop_levels.png",
       width = 7, height = 4, units = "in",
       plot_immpop_levels)

ggsave("Output/Figures/Maps/Czone_Level/immpop_shares.png",
       width = 7, height = 4, units = "in",
       plot_immpop_shares)
