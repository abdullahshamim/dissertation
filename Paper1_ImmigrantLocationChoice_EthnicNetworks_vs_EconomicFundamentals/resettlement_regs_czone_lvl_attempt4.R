rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

#### load data + minor adjustments ####

data_main_outcome_ethnic_network <- read_dta("Data/Clean/data_main_outcome_ethnic_network_vars.dta")
data_main_spatial_lag_controls <- read_dta("Data/Clean/data_main_spatial_lag_controls.dta")
data_main_econ_fund <- read_dta("Data/Clean/data_main_econ_fundamentals.dta")
data_main_political_economy <- read_dta("Data/Clean/data_main_political_economy.dta")
data_main_origin_czone_climate_euclid_dist <- read_dta("Data/Clean/data_main_origin_czone_climate_euclid_dist.dta")

data_main <- data_main_outcome_ethnic_network %>% 
  left_join(data_main_spatial_lag_controls) %>% 
  left_join(data_main_econ_fund) %>% 
  left_join(data_main_political_economy) %>% 
  left_join(data_main_origin_czone_climate_euclid_dist) %>% 
  relocate(in_sample, .after = everything())

# tot cohort-origin level refugee arrival weights and refugee + nonrefugee arrival weights

refarrivals_cohort <- read_dta("Data/Clean/refugee_origin_cohorts_all_insample.dta")

refarrivals_cohort <- refarrivals_cohort %>% 
  rename(totarrivals_ipums = arrivals_ipums, 
         totarrivals_dreher = arrivals_dreher,
         origin = cntryname) %>% 
  select(-year,-in_sample) 

data_main <- data_main %>% 
  left_join(refarrivals_cohort) %>% 
  rename(wt_totarrivals_dreher = totarrivals_dreher,
         wt_totarrivals_ipums = totarrivals_ipums)

# ethnic network defined as lagged origin pop + refugee placement by origin
data_main <- data_main %>% mutate(harmon_network = placements + lagged_originpop)

data_main <- data_main %>% 
  mutate(cohort = factor(cohort, levels = c('7580','8090','9199','0004')))

#### end new data merge ####

dict = c(
  "arrivals_dreher" = "Number of Refugees Resettled in CZ",
  "lagged_originpop" = "Lagged Origin Population",
  "spatial_lag_arrivals" = "Own-Origin Resettlements in Nearby CZs",
  "wage" = "Lagged Wage (in 1999 \\$s)",
  "hp" = "Lagged House Prices (in 1999 \\$s)",
  "emp_pop_ratio" = "Avg. Employment-to-Population Ratio",
  "resid_lagged_contpop" = "Residual Lagged Continent Pop.",
  "resid_lagged_fbpop" = "Residual Lagged Foreign-Born Pop.",
  "resid_lagged_totpop" = "Residual Lagged Total Pop.",
  "lagged_popov25_4ycol" = "Lagged 4yr College Share",
  "voteshare_president_democrat" = "Avg. Dem. Presidential Vote Share",
  "expend_pcpi_norm" = "State Expenditure-Income Ratio (in 1999 \\$s)",
  "temp_euclid_dist" = "CZ-Origin Temperature Similarity",
  "temp_range_euclid_dist" = "CZ-Origin Temperature Range Similarity",
  "temp_euclid_precip" = "CZ-Origin Rainfall Similarity"
  )

# # without czone fe
# reg1 <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
#               + spatial_lag_arrivals
#               + wage + hp + emp_pop_ratio
#               + lagged_popov25_4ycol
#               + voteshare_president_democrat + expend_pcpi_norm
#               + temp_euclid_dist + humidity_euclid_dist
#               | cohort^origin,
#               cluster = ~ czone,
#               # weights = ~ arrivals_dreher,
#               data = data_main)
# 
# # with czone fe
# reg1_czone_fe <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
#                        + spatial_lag_arrivals
#                        + wage + hp + emp_pop_ratio
#                        + lagged_popov25_4ycol
#                        + voteshare_president_democrat + expend_pcpi_norm
#                        + temp_euclid_dist + humidity_euclid_dist
#                        | cohort^origin + czone,
#                        cluster = ~ czone,
#                        # weights = ~ arrivals_dreher,
#                        data = data_main)

# filter out refs with ties (imm cohorts present in U.S. prior to ref cohorts)
data_main_woties <- data_main %>% filter(origin %notin% c('cuba','iran','poland', 'romania','ussr'))

# # for regression versions baseline, a-e: 
# baseline) lagged_originpop + spatial_lag_arrivals
# a) spatial_lag_arrivals + EconFund (wage,hp,emppopratio)
# b) baseline + a
# c) b + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
# d) c + lagged_popov25_4ycol
# e) d + + voteshare_president_democrat + expend_pcpi_norm + temp_euclid_dist + humidity_euclid_dist


# filtering out refs with ties
reg2e <- feols(placements ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
               + nearby_arrivals_idw_norm
               + wage + hp + emp_pop_ratio
               + lagged_popov25_4ycol
               + voteshare_president_democrat + expend_pcpi_norm
               + temp_euclid_dist : temp_range_euclid_dist + precip_euclid_dist
               | cohort^origin,
               cluster = ~ czone^origin, 
               # weights = ~ arrivals_dreher, 
               data = data_main_woties %>% filter(in_sample == 1))

etable(reg2e, view = T)

etable(reg2, reg2a, reg2b, reg2c, reg2d, reg2e,
       dict = dict, view = T, tex = T)

# with czone fe and filtering out refs with ties
reg2e_czone_fe <- feols(placements ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                        + nearby_arrivals_idw_norm
                        + wage + hp + emp_pop_ratio
                        + lagged_popov25_4ycol
                        + voteshare_president_democrat + expend_pcpi_norm
                        + temp_euclid_dist : temp_range_euclid_dist + precip_euclid_dist
                        | cohort^origin + czone,
                        cluster = ~ czone^origin, 
                        # weights = ~ arrivals_dreher, 
                        data = data_main_woties %>% filter(in_sample == 1))

etable(reg2e_czone_fe, view = T)

etable(reg2_czone_fe, reg2a_czone_fe, reg2b_czone_fe, reg2c_czone_fe, reg2d_czone_fe, reg2e_czone_fe, 
       dict = dict, tex = T, view = T)

# country-by-country

origins_woties_insample <- data_main_woties %>% 
  filter(in_sample == 1) %>% select(origin) %>% unique()

regs_byorigin <- list()

# country-by-country
for (k in origins_woties_insample$origin) { 
  
  print(k)
  
  # filtering out refs with ties
  regs_byorigin[[paste(k)]] <- 
    feols(arrivals_dreher ~ lagged_originpop 
          + spatial_lag_arrivals
          + wage + hp + emp_pop_ratio  
          + lagged_popov25_4ycol
          + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
          # + voteshare_president_democrat + expend_pcpi_norm
          # + temp_euclid_dist + humidity_euclid_dist
          | cohort^origin,
          cluster = ~ czone^origin, 
          # weights = ~ arrivals_dreher, 
          data = data_main_woties %>% filter(origin == k, in_sample == 1))
  
}

etable(regs_byorigin, headers = str_to_title(names(regs_byorigin)), dict = dict, view = T)

# with czone fe

origins_woties_insample_multiple_cohorts <- data_main_woties %>% 
  # data_main %>%
  filter(in_sample==1) %>% 
  count(origin,cohort) %>% 
  count(origin) %>% 
  filter(n>1) %>% 
  select(origin)

regs_byorigin_czonefe <- list()

for (k in origins_woties_insample_multiple_cohorts$origin) { 
  
  print(k)
  
  # filtering out refs with ties
  regs_byorigin_czonefe[[paste(k)]] <- 
    feols(arrivals_dreher ~ lagged_originpop 
          + spatial_lag_arrivals
          + wage + hp + emp_pop_ratio  
          + lagged_popov25_4ycol
          + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
          # + voteshare_president_democrat + expend_pcpi_norm
          # + temp_euclid_dist + humidity_euclid_dist
          | cohort^origin + czone,
          cluster = ~ czone^origin, 
          # weights = ~ arrivals_dreher, 
          data = data_main_woties %>% filter(origin == k, in_sample == 1))
  
}

etable(regs_byorigin_czonefe,
       headers = str_to_title(names(regs_byorigin_czonefe)), 
       dict = dict, view = T)

# # how are initial cohorts (without U.S. ties) resettled?
# 
# first_cohort_ids <- data_main_woties %>% 
#   group_by(origin) %>% 
#   mutate(min_cohort = min(as.numeric(cohort))) %>% 
#   filter(as.numeric(cohort) == min_cohort) %>% 
#   ungroup() %>% 
#   select(origin, cohort) %>% 
#   mutate(first_cohort = 1) %>% 
#   distinct()
# 
# first_cohort_ids$cohort[first_cohort_ids$origin=='somalia'] <- 9199
# first_cohort_ids$cohort[first_cohort_ids$origin=='sudan'] <- 9199
# first_cohort_ids$cohort[first_cohort_ids$origin=='ussr'] <- 8090
# first_cohort_ids$cohort[first_cohort_ids$origin=='yugoslavia'] <- 9199
# 
# data_main_woties <- data_main_woties %>%
#   left_join(first_cohort_ids) %>% 
#   mutate(first_cohort = ifelse(is.na(first_cohort), 0, first_cohort))
# 
# # first cohorts
# reg3_firstcoh <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop
#               + resid_lagged_totpop
#               + spatial_lag_arrivals
#               + wage + hp + emp_pop_ratio 
#               # + voteshare_president_democrat + expend_pcpi_norm + lagged_popov25_4ycol
#               # + temp_euclid_dist + humidity_euclid_dist
#               | cohort^origin,
#               cluster = ~ czone, 
#               # weights = ~ arrivals_dreher, 
#               data = data_main_woties %>% filter(first_cohort == 1))
# 
# # subsequent cohorts
# reg3_latercoh <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop
#                        + resid_lagged_totpop
#                        + spatial_lag_arrivals
#                        + wage + hp + emp_pop_ratio
#               # + voteshare_president_democrat + expend_pcpi_norm + lagged_popov25_4ycol
#               # + temp_euclid_dist + humidity_euclid_dist
#               | cohort^origin,
#               cluster = ~ czone, 
#               # weights = ~ arrivals_dreher, 
#               data = data_main_woties %>% filter(first_cohort == 0))
# 
# # first cohorts
# reg3_czonefe_firstcoh <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop
#                        + resid_lagged_totpop
#                        + spatial_lag_arrivals
#                        + wage + hp + emp_pop_ratio 
#                        # + voteshare_president_democrat + expend_pcpi_norm + lagged_popov25_4ycol
#                        # + temp_euclid_dist + humidity_euclid_dist
#                        | cohort^origin + czone,
#                        cluster = ~ czone^origin, 
#                        # weights = ~ arrivals_dreher, 
#                        data = data_main_woties %>% filter(first_cohort == 1))
# 
# # subsequent cohorts
# reg3_czonefe_latercoh <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop
#                        + resid_lagged_totpop
#                        + spatial_lag_arrivals
#                        + wage + hp + emp_pop_ratio
#                        # + voteshare_president_democrat + expend_pcpi_norm + lagged_popov25_4ycol
#                        # + temp_euclid_dist + humidity_euclid_dist
#                        | cohort^origin + czone,
#                        cluster = ~ czone^origin, 
#                        # weights = ~ arrivals_dreher, 
#                        data = data_main_woties %>% filter(first_cohort == 0))
# 
# etable(reg3_firstcoh, reg3_czonefe_firstcoh, reg3_latercoh, reg3_czonefe_latercoh,
#        headers = rep(c('1st Cohort', 'Later Cohorts'), each = 2), view = T)
# 
# resettlementregs_firstcohort_byorigin <- list()
# resettlementregs_latercohort_byorigin <- list()
# 
# origins_woties <- unique(data_main_woties$origin)
# 
# # country-by-country first cohort
# for (i in origins_woties) { 
#   
#   print(i)
#   
#   # filtering out refs with ties
#   resettlementregs_firstcohort_byorigin[[paste(i)]] <- 
#     feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop
#           + resid_lagged_totpop
#           + spatial_lag_arrivals
#           + wage + hp + emp_pop_ratio 
#           # + voteshare_president_democrat + expend_pcpi_norm + lagged_popov25_4ycol
#           # + temp_euclid_dist + humidity_euclid_dist
#           | cohort^origin,
#           cluster = ~ czone, 
#           # weights = ~ arrivals_dreher, 
#           data = data_main_woties %>% filter(origin == i, first_cohort == 1))
#   
# }
# 
# # country-by-country later cohorts
# for (j in origins_woties) { 
#   
#   print(j)
#   
#   # filtering out refs with ties
#   resettlementregs_latercohort_byorigin[[paste(j)]] <- 
#     feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop
#           + resid_lagged_totpop
#           + spatial_lag_arrivals
#           + wage + hp + emp_pop_ratio 
#           # + voteshare_president_democrat + expend_pcpi_norm + lagged_popov25_4ycol
#           # + temp_euclid_dist + humidity_euclid_dist
#           | cohort^origin,
#           cluster = ~ czone, 
#           # weights = ~ arrivals_dreher, 
#           data = data_main_woties %>% filter(origin == j, first_cohort == 0))
#   
# }
# 
# etable(resettlementregs_firstcohort_byorigin, headers = names(resettlementregs_firstcohort_byorigin), view = TRUE)
# etable(resettlementregs_latercohort_byorigin, headers = names(resettlementregs_latercohort_byorigin), view = TRUE)