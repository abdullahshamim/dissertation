rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice/ACS1YearAnalysis")

library(tidyverse)
library(haven)
library(fixest)
library(stargazer)
library(RcppRoll)
library(readxl)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

#### load data + minor adjustments ####

data_main <- read_dta("Data/Clean/data_main_cohort_lvl_laggedpopcontrols.dta")
refarrivals_agg_cohort <- read_dta("Data/Clean/refugee_origin_cohorts_all_insample.dta")
czone_region_cross <- read_dta("Data/Clean/refugee_arrivals_czone_region_dreher23.dta") %>% 
  select(czone,region) %>% distinct()
invdist_df <- read_dta("Data/Clean/invdist_df_czone.dta")
invdist_df_threshold <- read_dta("Data/Clean/invdist_df_czone_within_threshold.dta")
emp_pop_ratio <- read_dta("Data/Clean/emppopratio_czone_cohort.dta")

  
# rename aggregate arrivals and aggregate ipums pop
refarrivals_agg_cohort <- refarrivals_agg_cohort %>% 
  rename(refpop_aggregate_ipums = arrivals_ipums, 
         arrivals_aggregate_dreher = arrivals_dreher,
         origin = cntryname) %>% 
  select(-year,-in_sample) 

data_main <- data_main %>% 
  left_join(refarrivals_agg_cohort) %>% 
  rename(wt_agg_arrivals = arrivals_aggregate_dreher, wt_agg_pop = refpop_aggregate_ipums)

# # join czone and region
# data_main <- data_main %>% left_join(czone_region_cross)
# 
# # join czone sf to calculate distance between czones
# data_main <- data_main %>% left_join(czone_sf)

# rescale some vars for easier display in reg tables
data_main <- data_main %>% 
  mutate(avg_expend_decade_inmillions = avg_expend_decade / 1e+6)

data_main <- data_main %>% 
  mutate(cohort = factor(cohort, levels = c('7580','8090','9199','0004')))

# # strong assumption: just for seeing what happens
# data_main <- data_main %>%
#   mutate(lagged_originpop = ifelse(cohort == 9199 & origin == 'yugoslavia', 0, lagged_originpop))

# ethnic network defined as lagged origin pop + refugee placement by origin
data_main <- data_main %>% mutate(harmon_network = arrivals_dreher + lagged_originpop)

#### calculate refarrivals in nearby czones; this is to control for the ease of secondary migration between czones ####

arrivals_czone_cohort_origin_wide <- data_main %>% 
  arrange(czone, cohort) %>% 
  select(cohort,origin,czone,arrivals_dreher) %>% # identifies all distinct observations
  pivot_wider(names_from = czone, 
              names_prefix = "arrivals_cz", 
              values_from = arrivals_dreher,
              values_fill = 0)

# aligning reference czone with refarrivals from other czones
test <- data_main %>% 
  arrange(czone, cohort) %>% 
  select(cohort,origin,czone) %>% 
  left_join(arrivals_czone_cohort_origin_wide)

test_long <- test %>% 
  rename(refer_cz = czone) %>% 
  pivot_longer(starts_with("arrivals"),
               names_to = "dest_cz",
               names_prefix = "arrivals_cz",
               values_to = "arrivals_dreher")

# inverse distance-weighted arrivals in czones within threshold
invdist_control_comp_df <- test_long %>% 
  left_join(invdist_df_threshold) %>% 
  mutate(spatial_lag_arrivals = arrivals_dreher * invdist)

invdist_controls_df <- invdist_control_comp_df %>% 
  group_by(cohort,origin,refer_cz) %>% 
  summarize(spatial_lag_arrivals = sum(spatial_lag_arrivals)) %>% 
  rename(czone = refer_cz)

# join spatial lag controls with main data
data_main <- data_main %>% left_join(invdist_controls_df)

#### end spatial lag controls ####

data_main <- data_main %>% 
  left_join(emp_pop_ratio)

# rename population over 25 with 4year college
data_main <- data_main %>% rename(lagged_popov25_4ycol = pop_ov25_4ycol)

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
reg2emov <- feols(net_inflow ~ harmon_network 
                 + spatial_lag_arrivals
                 + wage
                 + hp
                 + emp_pop_ratio
                 + lagged_popov25_4ycol
                 + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                 + voteshare_president_democrat + avg_expend_decade_inmillions
                 + temp_euclid_dist + humidity_euclid_dist
                 | cohort^origin,
                 cluster = ~ czone^origin, 
                 # weights = ~ wt_agg_arrivals,
                 data = data_main_woties %>% filter(in_sample == 1))

etable(reg2mov, reg2amov, reg2bmov, reg2cmov, reg2dmov, reg2emov,
       dict = dict, view = T, tex = T)

# with czone fe
reg2emov_czfe <- feols(net_inflow ~ harmon_network 
                      + spatial_lag_arrivals
                      + wage
                      + hp
                      + emp_pop_ratio
                      + lagged_popov25_4ycol
                      + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                      + voteshare_president_democrat + avg_expend_decade_inmillions
                      + temp_euclid_dist + humidity_euclid_dist
                      | cohort^origin + czone,
                      cluster = ~ czone^origin, 
                      # weights = ~ wt_agg_arrivals,
                      data = data_main_woties %>% filter(in_sample == 1))

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