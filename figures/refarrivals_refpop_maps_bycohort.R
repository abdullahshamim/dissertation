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
refarrival_years <- c(1980,1990,1999,2004)

refpop_cohort_czone_insample <- read_dta("Data/Clean/refugee_pop_cohort_czone_insample.dta")
refarrivals_cohort_czone_insample <- read_dta('Data/Clean/refugee_arrivals_cohort_czone_insample.dta')
# czone_characteristics <- read_dta('Data/Clean/czone_characteristics.dta')
# data_main <- read_dta("Data/Clean/data_main_cohort_lvl_laggedpopcontrols.dta")

# clean and filter refpop data
refpop_cohort_czone_insample <- refpop_cohort_czone_insample %>% 
  # mutate(cohort = factor(cohort, levels = c('pre1975', '7580', '8090', '9199', '0004', '0508', 'post2008'))) %>% 
  filter(year %in% 1970:2007) %>% 
  group_by(year,cntryname) %>% 
  filter(cohort == 'pre1975' & year == 1970 |
         cohort == '7580' & year == 1980 |
           cohort == '8090' & year == 1990 |
           cohort == '9199' & year == 2000 |
           cohort == '0004' & year == 2007 )
  
refpop_cohort_czone_insample %>% ungroup() %>% count(year,cohort)

refpop_cohort_czone_insample <- refpop_cohort_czone_insample %>% 
  rename(origin=cntryname) %>% 
  filter(in_sample == 1) %>% 
  left_join(czone_sf)

# 
# # aggregate refarrivals to czone data
# refarrivals_origin_cohort_czone_cohort1 <- refarrivals_czone %>% 
#   filter(year %in% 1975:1980) %>% 
#   mutate(cohort = '7580') %>% 
#   group_by(cohort,citizenship_stable,region,czone) %>% 
#   summarize(refugees = sum(refugees))
# 
# refarrivals_origin_cohort_czone_cohort2 <- refarrivals_czone %>% 
#   filter(year >= 1980) %>% 
#   mutate(cohort = case_match(year, 1980:1990 ~ '8090', 1991:1999 ~ '9199', 2000:2004 ~ '0004', 2005:2018 ~ 'after2004')) %>% 
#   group_by(cohort,citizenship_stable,region,czone) %>% 
#   summarize(refugees = sum(refugees))
# 
# refarrivals_origin_cohort_czone <- refarrivals_origin_cohort_czone_cohort1 %>% 
#   bind_rows(refarrivals_origin_cohort_czone_cohort2) %>% 
#   mutate(cohort = factor(cohort, levels = c('7580', '8090', '9199', '0004', 'after2004'))) %>% 
#   rename(origin = citizenship_stable) %>% 
#   arrange(cohort, origin)

refarrivals_cohort_czone_insample <- refarrivals_cohort_czone_insample %>% 
  rename(origin=citizenship_stable) %>% 
  filter(in_sample == 1) %>% 
  left_join(czone_sf)

net_inflows_cohort_czone_insample <- refarrivals_cohort_czone_insample %>%
  left_join(refpop_cohort_czone_insample) %>% 
  mutate(refpop_ipums = replace_na(refpop_ipums, 0)) %>% 
  select(-year) %>% relocate(refpop_ipums, .after = refarrivals_dreher) %>% 
  mutate(net_inflow = refpop_ipums - refarrivals_dreher, .after = refpop_ipums)
  

# # join net inflows to czone shapefile
# data_main <- data_main %>% 
#   mutate(cohort = factor(cohort, levels = c('7580', '8090', '9199', '0004', 'after2004'))) %>% 
#   left_join(czone_sf)

#### maps ####

plots <- list()
origins <- unique(refarrivals_cohort_czone_insample$origin)
origin_cohorts <- refarrivals_cohort_czone_insample %>% count(origin,cohort)

# czone boundaries
background <- ggplot() +
  geom_sf(data = czone_sf, fill = "grey80", color = "grey80", linewidth = 0.2)

# state boundaries
state_boundaries <- geom_sf(data = state_sf, fill = "NA",
                            color = "grey40", linewidth = 0.3, linetype = 'dashed')

us_boundary <- geom_sf(data = us_sf, fill = "NA",
                       color = "grey40", linewidth = 0.5, linetype = 'solid')

czone_boundaries <- geom_sf(data = czone_sf, fill = "NA",
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

# refugee placement maps by origin-cohort (from dreher)
for (o in origins) {
  
  cohorts <- filter(origin_cohorts, origin == o)$cohort
  
  for (c in cohorts) {
    
    p <- background +
      geom_sf(data = refarrivals_cohort_czone_insample %>% 
                filter(cohort == c, origin == o, refarrivals_dreher > 0),
              aes(fill = refarrivals_dreher, geometry = geometry),
              color = 'grey80') +
      scale_fill_gradient2(
        trans = "log10",
        name = NULL
      ) +
      guides(fill = guide_colourbar(theme = theme(
        legend.key.width  = unit(15, "lines"),
        legend.key.height = unit(0.5, "lines")
      ))) +
      theme_bw() + 
      theme(
        legend.position = "top",
        legend.margin = margin(t = -5, b = 0)
      ) +
      state_boundaries + us_boundary +
      labs(main = paste("Origin", str_to_title(o), "Cohort", c, "Placements", sep = " "))
    
    # print(p)
    
    ggsave(paste0("Output/Figures/Maps/RefPlacement/czone_cohort_lvl/",o,"_",c,".png"),
           width = 7, height = 4, units = "in", p)
    
  }
}

# refugee pop maps by origin-cohort (from ipums)
for (o in origins) {
  
  cohorts <- filter(origin_cohorts, origin == o)$cohort
  
  for (c in cohorts) {
    
    p <- background +
      geom_sf(data = refpop_cohort_czone_insample %>% 
                filter(cohort == c, origin == o, refpop_ipums > 0),
              aes(fill = refpop_ipums, geometry = geometry),
              color = 'grey80') +
      scale_fill_gradient2(
        trans = "log10",
        name = NULL,
        labels = scales::label_number(accuracy = 1)
      ) +
      guides(fill = guide_colourbar(theme = theme(
        legend.key.width  = unit(20, "lines"),
        legend.key.height = unit(0.5, "lines")
      ))) +
      theme_bw() + 
      theme(
        legend.position = "top",
        legend.margin = margin(t = -5, b = 0)
      ) +
      state_boundaries + us_boundary +
      labs(main = paste("Origin", str_to_title(o), "Cohort", c, "Population Size", sep = " "))
    
    # print(p)
    
    ggsave(paste0("Output/Figures/Maps/RefPop_ipums/czone_cohort_lvl/",o,"_",c,".png"),
           width = 7, height = 4, units = "in", p)
    
  }
}

# refugee net inflow maps by origin-cohort (difference of pop and placement)
for (o in origins) {
  
  cohorts <- filter(origin_cohorts, origin == o)$cohort
  
  for (c in cohorts) {
    
    p <- background +
      geom_sf(data = net_inflows_cohort_czone_insample %>% 
                filter(cohort == c, origin == o),
              aes(fill = net_inflow, geometry = geometry),
              color = 'grey80') +
      scale_fill_gradient2(
        # trans = "log10",
        name = NULL
      ) +
      guides(fill = guide_colourbar(theme = theme(
        legend.key.width  = unit(15, "lines"),
        legend.key.height = unit(0.5, "lines")
      ))) +
      theme_bw() + 
      theme(
        legend.position = "top",
        legend.margin = margin(t = -5, b = 0)
      ) +
      state_boundaries + us_boundary +
      labs(main = paste("Origin", str_to_title(o), "Cohort", c, "Net Inflows", sep = " "))
    
    # print(p)
    
    ggsave(paste0("Output/Figures/Maps/NetInflow/czone_cohort_lvl/",o,"_",c,".png"),
           width = 7, height = 4, units = "in", p)
    
  }
}


# # raw arrivals
# 
# # all refs
# plot_allrefs_levels <- background +
#   geom_sf(data = cum_refarrivals_czone %>% filter(cohort == c, origin == o),
#           aes(fill = refugees, geometry = geometry),
#           color = 'grey80') +
#   state_boundaries + us_boundary +
#   scale_fill_brewer(palette = 'Greens', name = NULL) +
#   # labs(title = "Refugees Resettled in CZs Between 1975-2008") +
#   theme_bw() +
#   label_theme +
#   guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))
# 
# 
# # refs without ties
# plot_refs_woties_levels <- background +
#   geom_sf(data = cum_refarrivals_czone_woties %>% filter(year == 2008),
#           aes(fill = cum_refugees_quantile, geometry = geometry),
#           color = 'grey80') +
#   state_boundaries + us_boundary +
#   scale_fill_brewer(palette = 'Greens', name = NULL) +
#   # labs(title = "Refugees (Without Ties) Resettled in CZs Between 1975-2008") +
#   theme_bw() +
#   label_theme +
#   guides(fill = guide_legend(title.position = "top",title.hjust = 0.5))