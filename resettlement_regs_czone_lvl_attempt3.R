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
  rename(wt_agg_arrivals = arrivals_aggregate_dreher,
         wt_agg_pop = refpop_aggregate_ipums)

# rename the college share var (to explicitly state that it is lagged)
data_main <- data_main %>% rename(lagged_popov25_4ycol = pop_ov25_4ycol)

# # calculating residual foreign-born and continent pop after taking out continent and country pop
# data_main <- data_main %>% 
#   mutate(lagged_contpop_resid = lagged_contpop - lagged_originpop, .after = lagged_contpop) %>% 
#   mutate(lagged_fbpop_resid = lagged_fbpop - lagged_contpop, .after = lagged_fbpop) %>% 
#   mutate(lagged_totpop_resid = lagged_totpop - lagged_fbpop, .after = lagged_totpop)

# rescale some vars for easier display in reg tables
data_main <- data_main %>% 
  mutate(avg_expend_decade_inmillions = avg_expend_decade / 1e+6)

# ethnic network defined as lagged origin pop + refugee placement by origin
data_main <- data_main %>% mutate(harmon_network = arrivals_dreher + lagged_originpop)

data_main <- data_main %>% 
  mutate(cohort = factor(cohort, levels = c('7580','8090','9199','0004')))

#### attach new data (lagged spatial controls + emp-pop ratio) ####

# calculate refarrivals in nearby czones; this is to control for the ease of secondary migration between czones #

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

# join emp-pop ratio of czones

data_main <- data_main %>% 
  left_join(emp_pop_ratio)

#### end new data merge ####

dict = c(
  "arrivals_dreher" = "Number of Refugees Resettled in CZ",
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
  "avg_expend_decade" = "Avg. State Expenditure",
  "temp_euclid_dist" = "State-Origin Temperature Similarity",
  "humidity_euclid_dist" = "State-Origin Humidity Similarity"
  )

# # without czone fe
# reg1 <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
#               + spatial_lag_arrivals
#               + wage + hp + emp_pop_ratio
#               + lagged_popov25_4ycol
#               + voteshare_president_democrat + avg_expend_decade
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
#                        + voteshare_president_democrat + avg_expend_decade
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
# e) d + + voteshare_president_democrat + avg_expend_decade + temp_euclid_dist + humidity_euclid_dist


# filtering out refs with ties
reg2e <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop
                 + resid_lagged_totpop
               + spatial_lag_arrivals
               + wage + hp + emp_pop_ratio
               + lagged_popov25_4ycol
               + voteshare_president_democrat + avg_expend_decade
               + temp_euclid_dist + humidity_euclid_dist
               | cohort^origin,
               cluster = ~ czone^origin, 
               # weights = ~ arrivals_dreher, 
               data = data_main_woties %>% filter(in_sample == 1))

etable(reg2, reg2a, reg2b, reg2c, reg2d, reg2e,
       dict = dict, view = T, tex = T)

# with czone fe and filtering out refs with ties
reg2e_czone_fe <- feols(arrivals_dreher ~ lagged_originpop + resid_lagged_contpop + resid_lagged_fbpop
                        + resid_lagged_totpop
                        + spatial_lag_arrivals
                        + wage + hp + emp_pop_ratio
                        + lagged_popov25_4ycol
                        + voteshare_president_democrat + avg_expend_decade
                        + temp_euclid_dist + humidity_euclid_dist
                        | cohort^origin + czone,
                        cluster = ~ czone^origin, 
                        # weights = ~ arrivals_dreher, 
                        data = data_main_woties %>% filter(in_sample == 1))

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
          # + voteshare_president_democrat + avg_expend_decade
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
          # + voteshare_president_democrat + avg_expend_decade
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
#               # + voteshare_president_democrat + avg_expend_decade + lagged_popov25_4ycol
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
#               # + voteshare_president_democrat + avg_expend_decade + lagged_popov25_4ycol
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
#                        # + voteshare_president_democrat + avg_expend_decade + lagged_popov25_4ycol
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
#                        # + voteshare_president_democrat + avg_expend_decade + lagged_popov25_4ycol
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
#           # + voteshare_president_democrat + avg_expend_decade + lagged_popov25_4ycol
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
#           # + voteshare_president_democrat + avg_expend_decade + lagged_popov25_4ycol
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