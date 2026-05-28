rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(fixest)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

# load data

data_ussr_outcome_ethnic_network <- read_dta("Data/Clean/data_ussr_outcome_ethnic_network_vars.dta")
data_ussr_spatial_lag_controls <- read_dta("Data/Clean/data_ussr_spatial_lag_controls.dta")

data_main_econ_fund <- read_dta("Data/Clean/data_main_econ_fundamentals.dta")
data_main_political_economy <- read_dta("Data/Clean/data_main_political_economy.dta")
data_main_origin_czone_climate_euclid_dist <- read_dta("Data/Clean/data_main_origin_czone_climate_euclid_dist.dta")
# data_main_wts <- read_dta("Data/Clean/data_main_wts.dta")

# join

data_ethnic_network_ussr <- data_ussr_outcome_ethnic_network %>% 
  left_join(data_ussr_spatial_lag_controls) 

data_econ_fund_ussr <- data_main_econ_fund %>% 
  left_join(data_main_political_economy) %>% 
  mutate(cohort = ifelse(cohort == '8090', '8894', cohort)) %>% 
  filter(cohort == '8894')

data_main_ussr <- data_ethnic_network_ussr %>%
  left_join(data_econ_fund_ussr) %>% 
  left_join(data_main_origin_czone_climate_euclid_dist)

data_main_ussr <- data_main_ussr %>% 
  mutate(emp_pop_ratio = emp_pop_ratio * 100,
         resid_lagged_contpop = resid_lagged_contpop / 10,
         resid_lagged_fbpop = resid_lagged_fbpop / 100,
         resid_lagged_totpop = resid_lagged_totpop / 1000,
         lagged_popov25_4ycol = lagged_popov25_4ycol / 100) %>% # rescaling
  mutate(harmon_network = placements + lagged_originpop) %>% # harmonized ethnic networks
  mutate(temp_euclid_distance_interaction_temp_range_euclid_distance = temp_euclid_dist * temp_range_euclid_dist) # to be able to name the interaction in reg tables

# data_main_woties <- data_main %>% filter(origin %notin% c('cuba','iran','poland', 'romania','ussr'))
# data_main_wties <- data_main %>% filter(origin %in% c('cuba','iran','poland', 'romania','ussr'))

data_main_ussr <- data_main_ussr %>% 
  rename_with( ~ gsub('ussr', 'origin', .))

# # outcome var in shares
# data_main_woties_shares <- data_main_woties %>% 
#   group_by(origin, cohort) %>% 
#   mutate(across(c(placements, contains("pop")), ~ 100 * .x / sum(.x)))

### regs

## setup

# # regressors for regression versions: baseline, a-e: 
#
# baseline) lagged_originpop + nearby_arrivals_idw_norm
# a) nearby_arrivals_idw_norm + EconFund (wage,hp,emppopratio)
# b) baseline + a
# c) b + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
# d) c + lagged_popov25_4ycol
# e) d + + voteshare_president_democrat + expend_pcpi_norm + temp_euclid_dist + humidity_euclid_dist

# list of variable groups
baseline0_vars <- c("placements", "nearby_arrivals_idw_norm")
baseline_vars <- c("placements", "nearby_arrivals_idw_norm", "lagged_originpop", "lagged_nearby_originpop_idw_norm")
a1_vars <- c("wage", "emp_pop_ratio")
a2_vars <- c("hp", "emp_pop_ratio")
a_vars <- c("wage", "hp", "emp_pop_ratio")
c_vars <- c("resid_lagged_contpop", "resid_lagged_fbpop", "resid_lagged_totpop")
d_vars <- c("lagged_popov25_4ycol")
e_vars <- c("voteshare_president_democrat", "expend_pcpi_norm")
f_vars <- c("temp_czone","temp_range_czone","precip_czone", "temp_euclid_dist", "temp_range_euclid_dist", "precip_euclid_dist")

# build formulas
formulas <- list()

formulas$baseline0 <- as.formula(paste("net_inflow ~ ", paste(baseline0_vars, collapse = " + ")))
formulas$baseline <- as.formula(paste("net_inflow ~ ", paste(baseline_vars, collapse = " + ")))
formulas$a1 <- as.formula(paste("net_inflow ~ ", paste(c("nearby_arrivals_idw_norm", a1_vars), collapse = " + ")))
formulas$a2 <- as.formula(paste("net_inflow ~ ", paste(c("nearby_arrivals_idw_norm", a2_vars), collapse = " + ")))
formulas$a <- as.formula(paste("net_inflow ~ ", paste(c("nearby_arrivals_idw_norm", a_vars), collapse = " + ")))
#formulas$abaseline0 <- as.formula(paste("net_inflow ~ ", paste(c(baseline0_vars, a_vars), collapse = " + ")))
formulas$b <- as.formula(paste("net_inflow ~ ", paste(c(baseline_vars, a_vars), collapse = " + ")))
formulas$c <- as.formula(paste("net_inflow ~ ", paste(c(baseline_vars, a_vars, c_vars), collapse = " + ")))
formulas$d <- as.formula(paste("net_inflow ~ ", paste(c(baseline_vars, a_vars, c_vars, d_vars), collapse = " + ")))
# formulas$e <- as.formula(paste("net_inflow ~ ", paste(c(baseline_vars, a_vars, c_vars, d_vars, e_vars), collapse = " + ")))
formulas$fbaseline0 <- as.formula(paste("net_inflow ~ ", paste(c(baseline0_vars, a_vars, c_vars, d_vars, e_vars, f_vars), collapse = " + ")))
formulas$f <- as.formula(paste("net_inflow ~ ", paste(c(baseline_vars, a_vars, c_vars , d_vars, e_vars, f_vars), collapse = " + ")))

dict = c(
  "placements" = "Number of Refugees Resettled in CZ",
  "net_inflow" = "Net Inflow into CZ",
  "lagged_originpop" = "Lagged Same-Origin CZ Population",
  "nearby_arrivals_idw_norm" = "Same-Origin Resettlements in Nearby CZs",
  "lagged_nearby_originpop_idw_norm" = "Lagged Same-Origin Pop. in Nearby CZs",
  "wage" = "Lagged Wage (1999 Dollars)",
  "hp" = "Lagged Gross Rent (1999 Dollars)",
  "emp_pop_ratio" = "Emp-to-Pop Ratio (5/10 Year Rolling Avg)",
  "resid_lagged_contpop" = "Residual Lagged Continent Pop. / 10",
  "resid_lagged_fbpop" = "Residual Lagged Foreign-born Pop. / 100",
  "resid_lagged_totpop" = "Residual Lagged Total Pop. / 1000",
  "lagged_popov25_4ycol" = "Lagged College Educated Pop. / 100",
  "voteshare_president_democrat" = "Democrat Voteshare (5/10 Year Rolling Avg)",
  "expend_pcpi_norm" = "State Expenditure-to-Income Ratio (5/10 Year Rolling Avg)",
  "temp_czone" = "Temperature CZ",
  "temp_range_czone" = "Temperature Fluctuation CZ",
  "precip_czone" = "Rainfall CZ",
  "temp_euclid_dist" = "Temperature Euclid. Dist. (CZ vs. Origin)",
  "temp_range_euclid_dist" = "Temperature Fluctuation Euclid. Dist. (CZ vs. Origin)",
  "precip_euclid_dist" = "Rainfall Euclid. Dist. (CZ vs. Origin)",
  "temp_euclid_distance_interaction_temp_range_euclid_distance" = "Temp. Similarity  X Temp. Fluctuation Similarity (CZ vs. Origin)"
)

varorder <- unname(dict[c(baseline_vars, a_vars)]) %>% 
  gsub("\\(", "\\\\(", .) %>% 
  gsub("\\)", "\\\\)", .)

drop_vars <- unname(dict[c(e_vars, f_vars)]) %>% 
  gsub("\\(", "\\\\(", .) %>% 
  gsub("\\)", "\\\\)", .) %>% 
  gsub("\\.", "\\\\.", .)

groups <- list("^Voteshare, Expenditure, and Climate Controls" = drop_vars)

highlight_vars = list(
  "rowcol, lightblue!30, se" = c("Wage", "Rent","Emp"),
  "rowcol, orangered!15, se" = c("Number of Refugees Resettled in CZ", "Same-Origin Resettlements in Nearby CZs", "Lagged Same-Origin CZ Population", "Lagged Same-Origin Pop. in Nearby CZs")
)

## run regressions - pooled origin

# cohort-origin fixed effects

# fe_vars <- "cohort^origin"

reg1 <- lapply(formulas, function(fml) {
  feols(as.formula(fml),
        cluster = ~ czone,
        data = data_main_ussr)
})


reg1w <- lapply(formulas, function(fml) {
  feols(as.formula(fml),
        cluster= ~ czone, weights = ~ popwt_origin_cohort_dreher,
        data = data_main_woties %>% filter(in_sample == 1))
})

etable(reg1, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, group = groups,
       highlight = highlight_vars, view = T)

# etable(reg1, digits = "r3", digits.stats = "r4", dict = dict, tex = T, view = T, file = "Output/Tables/regs/latex_tables/SecondarymigDriversRegs/yrfe.tex")
# etable(reg1w, digits = "r3", digits.stats = "r4", dict = dict, tex = T, view = T, file = "Output/Tables/regs/latex_tables/SecondarymigDriversRegs/yrfe_wtd.tex")

# cohort-origin + czone fixed effects

fe_vars <- "cohort^origin + czone"

reg2 <- lapply(formulas, function(fml) {
  feols(as.formula(fml),
        cluster = ~ czone,
        data = data_main_woties %>% filter(in_sample == 1))
})


reg2w <- lapply(formulas, function(fml) {
  feols(as.formula(fml),
        cluster= ~ czone, weights = ~ popwt_origin_cohort_dreher,
        data = data_main_woties %>% filter(in_sample == 1))
})


etable(reg2, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       group = groups, highlight = highlight_vars)

# etable(reg2, digits = "r3", digits.stats = "r4", dict = dict, tex = T, view = T, file = "Output/Tables/regs/latex_tables/SecondarymigDriversRegs/yrfe_czonefe.tex")
# etable(reg2w, digits = "r3", digits.stats = "r4", dict = dict, tex = T, view = T, file = "Output/Tables/regs/latex_tables/SecondarymigDriversRegs/yrfe_czonefe_wtd.tex")


## regressions: country-by-country 

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
yrfe_czonefe <- list()


for (i in all_countries$origin) { 
  
  print(i)
  
  # filtering out refs with ties
  yrfe_only[[paste(i)]] <- feols(net_inflow ~ placements + lagged_originpop + nearby_arrivals_idw_norm + lagged_nearby_originpop_idw_norm
                                 + wage
                                 + hp
                                 + emp_pop_ratio
                                 + lagged_popov25_4ycol
                                 + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                                 # + voteshare_president_democrat + expend_pcpi_norm
                                 | cohort,
                                 cluster = ~ czone, 
                                 data = data_main_woties %>% filter(in_sample == 1, origin == i)
                                 # data = data_main %>% filter(in_sample == 1, origin == i)
                                 )
  
}

# etable(yrfe_only, headers = str_to_title(names(yrfe_only)),
       digits = "r3", digits.stats = "r4", dict = dict, view = TRUE,
       file = "Output/Tables/regs/latex_tables/SecondarymigDriversRegs/yrfe_byorigin.tex")


for (j in multiple_cohorts$origin) { 
  
  print(j)
  
  # filtering out refs with ties
  yrfe_czonefe[[paste(j)]] <- feols(net_inflow ~ placements + lagged_originpop + nearby_arrivals_idw_norm + lagged_nearby_originpop_idw_norm
                                    + wage
                                    + hp
                                    + emp_pop_ratio
                                    + lagged_popov25_4ycol
                                    + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
                                    # + voteshare_president_democrat + expend_pcpi_norm
                                    | cohort + czone,
                                    cluster = ~ czone, 
                                    data = data_main_woties %>% filter(in_sample == 1, origin == j)
                                    # data = data_main %>% filter(in_sample == 1, origin == j)
  )
  
}

# etable(yrfe_czonefe, headers = str_to_title(names(yrfe_czonefe)),
       digits = "r3", digits.stats = "r4", dict = dict, view = TRUE,
       file = "Output/Tables/regs/latex_tables/SecondarymigDriversRegs/yrfe_czonefe_byorigin.tex")


# Only controlling for fundamentals

yrfe_only_fund_only <- list()

for (i in all_countries$origin) { 
  
  print(i)
  
  # filtering out refs with ties
  yrfe_only_fund_only[[paste(i)]] <- feols(net_inflow ~
                                 + wage
                                 + hp
                                 + emp_pop_ratio
                                 | cohort,
                                 cluster = ~ czone, 
                                 data = data_main_woties %>% filter(in_sample == 1, origin == i)
                                 # data = data_main %>% filter(in_sample == 1, origin == i)
  )
  
}

# etable(yrfe_only_fund_only, headers = str_to_title(names(yrfe_only_fund_only)), 
       digits = "r3", digits.stats = "r4", dict = dict, view = TRUE,
       file = "Output/Tables/regs/latex_tables/SecondarymigDriversRegs/yrfe_byorigin_fundonly.tex")


yrfe_czonefe_fund_only <- list()

for (j in multiple_cohorts$origin) { 
  
  print(j)
  
  # filtering out refs with ties
  yrfe_czonefe_fund_only[[paste(j)]] <- feols(net_inflow ~ 
                                    + wage
                                    + hp
                                    + emp_pop_ratio
                                    | cohort + czone,
                                    cluster = ~ czone, 
                                    data = data_main_woties %>% filter(in_sample == 1, origin == j)
                                    # data = data_main %>% filter(in_sample == 1, origin == j)
  )
  
}

# # etable(yrfe_regionfe, headers = names(yrfe_only), view = TRUE)
# etable(yrfe_czonefe_fund_only, 
       headers = str_to_title(names(yrfe_czonefe_fund_only)), 
       digits = "r3", digits.stats = "r4", dict = dict, view = TRUE,
       file = "Output/Tables/regs/latex_tables/SecondarymigDriversRegs/yrfe_czonefe_byorigin_fundonly.tex")


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
# # etable(quadriticregs_byorigin, headers = names(quadriticregs_byorigin), view = T)
# 
# 
# #### end country-by-country regresssions ####