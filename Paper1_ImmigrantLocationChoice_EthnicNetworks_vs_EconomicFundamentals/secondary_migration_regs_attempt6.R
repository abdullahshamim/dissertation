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

#### end data loading + cleaning ####
  
#### regressions - pooling all countries ####

# log(net_inflow) ~ log(harmon_network) + log(resid_contpop) + log(resid_fbpop) + log(resid_totpop)
# net_inflow ~ harmon_network + resid_contpop + resid_fbpop + resid_totpop

# # without czone fe
# reg1mov <- feols(net_inflow ~ harmon_network # + resid_contpop + resid_fbpop
#                  + resid_totpop
#                  + spatial_lag_arrivals
#                  + wage
#                  + hp
#                  + emp_pop_ratio 
#                  + lagged_popov25_4ycol
#                  + voteshare_president_democrat + avg_expend_decade_inmillions
#                  + temp_euclid_dist + humidity_euclid_dist
#                  | cohort^origin,
#                  cluster = ~ czone, 
#                  # weights = ~ wt_agg_arrivals,
#                  data = data_main %>% filter(in_sample == 1))
# 
# # with czone fe
# reg1mov_czfe <- feols(net_inflow ~ harmon_network # + resid_contpop + resid_fbpop
#                       + resid_totpop
#                       + spatial_lag_arrivals
#                       + wage
#                       + hp
#                       + emp_pop_ratio 
#                       + lagged_popov25_4ycol
#                       + voteshare_president_democrat + avg_expend_decade_inmillions
#                       + temp_euclid_dist + humidity_euclid_dist
#                       | cohort^origin + czone,
#                       cluster = ~ czone, 
#                       # weights = ~ wt_agg_arrivals,
#                       data = data_main %>% filter(in_sample == 1))

# # with czone fe
# reg1mov_regionczfe <- feols(net_inflow ~ harmon_network + resid_contpop + resid_fbpop + resid_totpop
#                             + spatial_lag_arrivals
#                             + wage
#                             + hp
#                             + emp_pop_ratio 
#                             + lagged_popov25_4ycol
#                             + voteshare_president_democrat + avg_expend_decade_inmillions
#                             + temp_euclid_dist + humidity_euclid_dist
#                             | cohort^origin + origin^region + czone,
#                             cluster = ~ czone, 
#                             # weights = ~ wt_agg_arrivals,
#                             data = data_main %>% filter(in_sample == 1))

# etable(reg1mov, reg1mov_czfe, view = T)


# filter out refs with ties (imm cohorts present in U.S. prior to ref cohorts)
data_main_woties <- data_main %>% filter(origin %notin% c('cuba','iran','poland', 'romania','ussr'))

dict = c(
  "net_inflow" = "Net Inflow into CZ",
  "harmon_network" = "Lagged Origin Pop. + New Own-Origin Resett.",
  "arrivals_dreher" = "Number of Refugees Resettled",
  "lagged_originpop" = "Lagged Origin Population",
  "spatial_lag_arrivals" = "Own-Origin Resettlements in Nearby CZs",
  "wage" = "Lagged Wage",
  "hp" = "Lagged House Prices",
  "emp_pop_ratio" = "Avg. Employment-to-Population Ratio",
  "resid_lagged_contpop" = "Residual Lagged Continent Pop.",
  "resid_lagged_fbpop" = "Residual Lagged Foreign-Born Pop.",
  "resid_lagged_totpop" = "Residual Lagged Total Pop.",
  "lagged_popov25_4ycol" = "Lagged 4yr College Share",
  "voteshare_president_democrat" = "Avg. Dem. Presidential Vote Share",
  "avg_expend_decade_inmillions" = "Avg. State Expenditure in $ Millions",
  "temp_euclid_dist" = "State-Origin Temperature Similarity",
  "humidity_euclid_dist" = "State-Origin Humidity Similarity"
)

# # for regression versions baseline, a-e: 
# baseline) lagged_originpop + spatial_lag_arrivals
# a) spatial_lag_arrivals + EconFund (wage,hp,emppopratio)
# b) baseline + a
# c) b + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
# d) c + lagged_popov25_4ycol
# e) d + + voteshare_president_democrat + avg_expend_decade + temp_euclid_dist + humidity_euclid_dist

# filtering out refs with ties
reg2emov <- feols(net_inflow ~ placements + lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                 + nearby_arrivals_idw_norm + placements:nearby_arrivals_idw_norm
                 + lagged_nearby_originpop_idw_norm + placements:lagged_nearby_originpop_idw_norm
                 + wage + hp + emp_pop_ratio
                 + lagged_popov25_4ycol
                 + voteshare_president_democrat + expend_pcpi_norm
                 + temp_euclid_dist * temp_range_euclid_dist + precip_euclid_dist
                 | cohort^origin,
                 cluster = ~ czone^origin, 
                 # weights = ~ wt_agg_arrivals,
                 data = data_main_woties %>% filter(in_sample == 1))

etable(reg2emov, view = T)

etable(reg2mov, reg2amov, reg2bmov, reg2cmov, reg2dmov, reg2emov,
       dict = dict, view = T, tex = T)

# with czone fe
reg2emov_czfe <- feols(net_inflow ~ placements + lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                       + nearby_arrivals_idw
                       + lagged_nearby_originpop_idw
                       + wage + hp + emp_pop_ratio
                       + lagged_popov25_4ycol
                       + voteshare_president_democrat + expend_pcpi_norm
                       + temp_euclid_dist * temp_range_euclid_dist + precip_euclid_dist
                       | cohort^origin + czone,
                       cluster = ~ czone^origin, 
                       # weights = ~ wt_agg_arrivals,
                       data = data_main_woties %>% filter(in_sample == 1))

etable(reg2emov_czfe, view = T)

etable(reg2mov_czfe, reg2amov_czfe, reg2bmov_czfe, reg2cmov_czfe, reg2dmov_czfe, reg2emov_czfe,
       dict = dict, view = T, tex = T)

# reg2mov_regionczfe <- feols(net_inflow ~ harmon_network + resid_contpop + resid_fbpop + resid_totpop
#                             + spatial_lag_arrivals
#                             + wage
#                             + hp
#                             + emp_pop_ratio 
#                             + lagged_popov25_4ycol
#                             + voteshare_president_democrat + avg_expend_decade_inmillions
#                             + temp_euclid_dist + humidity_euclid_dist
#                             | cohort^origin + origin^region + czone,
#                             cluster = ~ czone, 
#                             # weights = ~ wt_agg_arrivals,
#                             data = data_main_woties %>% filter(in_sample == 1))

etable(reg1mov, reg1mov_czfe, reg2mov,reg2mov_czfe, view = T)


#### end pooled regressions ####

#### regressions: country-by-country ####

# which countries have single vs multiple cohorts

single_cohorts <- data_main_woties %>%
  # data_main %>% 
  filter(in_sample==1) %>% 
  count(origin,cohort) %>% 
  count(origin) %>% 
  filter(n==1) %>% 
  select(origin)

multiple_cohorts <- data_main_woties %>% 
  # data_main %>%
  filter(in_sample==1) %>% 
  count(origin,cohort) %>% 
  count(origin) %>% 
  filter(n>1) %>% 
  select(origin)

all_countries <- single_cohorts %>% 
  bind_rows(multiple_cohorts)

# country-by-country regs

yrfe_only <- list()
# yrfe_regionfe <- list()
yrfe_czonefe <- list()


for (i in all_countries$origin) { 
  
  print(i)
  
  # filtering out refs with ties
  yrfe_only[[paste(i)]] <- feols(net_inflow ~ harmon_network
                                 + spatial_lag_arrivals
                                 + wage
                                 + hp
                                 + emp_pop_ratio
                                 + lagged_popov25_4ycol
                                 + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                                 # + voteshare_president_democrat + avg_expend_decade_inmillions
                                 # + temp_euclid_dist + humidity_euclid_dist
                                 | cohort^origin,
                                 cluster = ~ czone, 
                                 # weights = ~ arrivals_dreher,
                                 data = data_main_woties %>% filter(in_sample == 1, origin == i)
                                 # data = data_main %>% filter(in_sample == 1, origin == i)
                                 )
  
}

etable(yrfe_only, headers = str_to_title(names(yrfe_only)), dict = dict, view = TRUE)

# for (i in all_countries$origin) { 
#   
#   print(i)
#   
#   # filtering out refs with ties
#   yrfe_regionfe[[paste(i)]] <- feols(net_inflow ~ harmon_network + resid_contpop + resid_fbpop + resid_totpop
#                                      + spatial_lag_arrivals
#                                  + wage
#                                  + hp
#                                  + emp_pop_ratio
#                                  + lagged_popov25_4ycol
#                                  + voteshare_president_democrat + avg_expend_decade_inmillions
#                                  + temp_euclid_dist + humidity_euclid_dist
#                                  | cohort + region,
#                                  cluster = ~ czone, 
#                                  # weights = ~ wt_agg_arrivals,
#                                  # data = data_main_woties %>% filter(in_sample == 1, origin == i)
#                                  data = data_main %>% filter(in_sample == 1, origin == i)
#   )
#   
# }


for (j in multiple_cohorts$origin) { 
  
  print(j)
  
  # filtering out refs with ties
  yrfe_czonefe[[paste(j)]] <- feols(net_inflow ~ harmon_network
                                    + spatial_lag_arrivals
                                    + wage
                                    + hp
                                    + emp_pop_ratio
                                    + lagged_popov25_4ycol
                                    + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                                    # + voteshare_president_democrat + avg_expend_decade_inmillions
                                    # + temp_euclid_dist + humidity_euclid_dist
                                    | cohort^origin + czone,
                                    cluster = ~ czone, 
                                    # weights = ~ arrivals_dreher,
                                    data = data_main_woties %>% filter(in_sample == 1, origin == j)
                                    # data = data_main %>% filter(in_sample == 1, origin == j)
  )
  
}

# etable(yrfe_regionfe, headers = names(yrfe_only), view = TRUE)
etable(yrfe_czonefe, headers = str_to_title(names(yrfe_czonefe)), dict = dict, view = TRUE)

for (i in all_countries$origin) { 
  
  print(i)
  
  # filtering out refs with ties
  yrfe_only[[paste(i)]] <- feols(net_inflow ~ harmon_network
                                 + spatial_lag_arrivals
                                 + wage
                                 + hp
                                 + emp_pop_ratio
                                 + lagged_popov25_4ycol
                                 + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                                 # + voteshare_president_democrat + avg_expend_decade_inmillions
                                 # + temp_euclid_dist + humidity_euclid_dist
                                 | cohort^origin,
                                 cluster = ~ czone, 
                                 # weights = ~ arrivals_dreher,
                                 data = data_main_woties %>% filter(in_sample == 1, origin == i)
                                 # data = data_main %>% filter(in_sample == 1, origin == i)
                                 )
  
}

etable(yrfe_only, headers = str_to_title(names(yrfe_only)), dict = dict, view = TRUE)

# for (i in all_countries$origin) { 
#   
#   print(i)
#   
#   # filtering out refs with ties
#   yrfe_regionfe[[paste(i)]] <- feols(net_inflow ~ harmon_network + resid_contpop + resid_fbpop + resid_totpop
#                                      + spatial_lag_arrivals
#                                  + wage
#                                  + hp
#                                  + emp_pop_ratio
#                                  + lagged_popov25_4ycol
#                                  + voteshare_president_democrat + avg_expend_decade_inmillions
#                                  + temp_euclid_dist + humidity_euclid_dist
#                                  | cohort + region,
#                                  cluster = ~ czone, 
#                                  # weights = ~ wt_agg_arrivals,
#                                  # data = data_main_woties %>% filter(in_sample == 1, origin == i)
#                                  data = data_main %>% filter(in_sample == 1, origin == i)
#   )
#   
# }


for (j in multiple_cohorts$origin) { 
  
  print(j)
  
  # filtering out refs with ties
  yrfe_czonefe[[paste(j)]] <- feols(net_inflow ~ harmon_network
                                    + spatial_lag_arrivals
                                    + wage
                                    + hp
                                    + emp_pop_ratio
                                    + lagged_popov25_4ycol
                                    + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                                    # + voteshare_president_democrat + avg_expend_decade_inmillions
                                    # + temp_euclid_dist + humidity_euclid_dist
                                    | cohort^origin + czone,
                                    cluster = ~ czone, 
                                    # weights = ~ arrivals_dreher,
                                    data = data_main_woties %>% filter(in_sample == 1, origin == j)
                                    # data = data_main %>% filter(in_sample == 1, origin == j)
  )
  
}

# etable(yrfe_regionfe, headers = names(yrfe_only), view = TRUE)
etable(yrfe_czonefe, headers = str_to_title(names(yrfe_czonefe)), dict = dict, view = TRUE)

# Only controlling for fundamentals

yrfe_only_fund_only <- list()

for (i in all_countries$origin) { 
  
  print(i)
  
  # filtering out refs with ties
  yrfe_only_fund_only[[paste(i)]] <- feols(net_inflow ~ # harmon_network
                                 # + spatial_lag_arrivals
                                 + wage
                                 + hp
                                 + emp_pop_ratio
                                 # + lagged_popov25_4ycol
                                 # + resid_lagged_contpop + resid_lagged_fbpop 
                                 # + resid_lagged_totpop
                                 # + voteshare_president_democrat + avg_expend_decade_inmillions
                                 # + temp_euclid_dist + humidity_euclid_dist
                                 | cohort^origin,
                                 cluster = ~ czone, 
                                 # weights = ~ arrivals_dreher,
                                 data = data_main_woties %>% filter(in_sample == 1, origin == i)
                                 # data = data_main %>% filter(in_sample == 1, origin == i)
  )
  
}

etable(yrfe_only_fund_only, 
       headers = str_to_title(names(yrfe_only_fund_only)), 
       dict = dict, view = TRUE)

# for (i in all_countries$origin) { 
#   
#   print(i)
#   
#   # filtering out refs with ties
#   yrfe_regionfe[[paste(i)]] <- feols(net_inflow ~ harmon_network + resid_contpop + resid_fbpop + resid_totpop
#                                      + spatial_lag_arrivals
#                                  + wage
#                                  + hp
#                                  + emp_pop_ratio
#                                  + lagged_popov25_4ycol
#                                  + voteshare_president_democrat + avg_expend_decade_inmillions
#                                  + temp_euclid_dist + humidity_euclid_dist
#                                  | cohort + region,
#                                  cluster = ~ czone, 
#                                  # weights = ~ wt_agg_arrivals,
#                                  # data = data_main_woties %>% filter(in_sample == 1, origin == i)
#                                  data = data_main %>% filter(in_sample == 1, origin == i)
#   )
#   
# }

yrfe_czonefe_fund_only <- list()

for (j in multiple_cohorts$origin) { 
  
  print(j)
  
  # filtering out refs with ties
  yrfe_czonefe_fund_only[[paste(j)]] <- feols(net_inflow ~ # harmon_network
                                    # + spatial_lag_arrivals
                                    + wage
                                    + hp
                                    + emp_pop_ratio
                                    # + lagged_popov25_4ycol
                                    # + resid_lagged_contpop + resid_lagged_fbpop 
                                    # + resid_lagged_totpop
                                    # + voteshare_president_democrat + avg_expend_decade_inmillions
                                    # + temp_euclid_dist + humidity_euclid_dist
                                    | cohort^origin + czone,
                                    cluster = ~ czone, 
                                    # weights = ~ arrivals_dreher,
                                    data = data_main_woties %>% filter(in_sample == 1, origin == j)
                                    # data = data_main %>% filter(in_sample == 1, origin == j)
  )
  
}

# etable(yrfe_regionfe, headers = names(yrfe_only), view = TRUE)
etable(yrfe_czonefe_fund_only, 
       headers = str_to_title(names(yrfe_czonefe_fund_only)), 
       dict = dict, view = TRUE)


# # regressions with quadratic originpop controls
# 
# quadriticregs_byorigin <- list()
# 
# for (k in all_countries$origin) { 
#   
#   print(k)
#   
#   # filtering out refs with ties
#   quadriticregs_byorigin[[paste(k)]] <- feols(net_inflow ~ harmon_network + harmon_network^2 + # resid_contpop + resid_fbpop + resid_totpop
#                                  + spatial_lag_arrivals
#                                               + wage
#                                  + hp
#                                  + emp_pop_ratio
#                                  # + lagged_popov25_4ycol
#                                  # + voteshare_president_democrat + avg_expend_decade_inmillions
#                                  # + temp_euclid_dist + humidity_euclid_dist
#                                  | cohort^origin,
#                                  cluster = ~ czone, 
#                                  # weights = ~ wt_agg_arrivals,
#                                  data = data_main_woties %>% filter(in_sample == 1, origin == k)
#                                  # data = data_main %>% filter(in_sample == 1, origin == i)
#   )
#   
# }
# 
# 
# etable(quadriticregs_byorigin, headers = names(quadriticregs_byorigin), view = T)
# 
# 
# #### end country-by-country regresssions ####