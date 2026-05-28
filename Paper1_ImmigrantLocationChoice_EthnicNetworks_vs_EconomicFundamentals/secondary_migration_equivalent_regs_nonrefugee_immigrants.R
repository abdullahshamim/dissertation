rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(fixest)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

# load data
data_imm_outcome_ethnic_network <- read_dta("Data/Clean/data_nonrefugee_immigrant_outcome_ethnic_network_vars.dta")
data_imm_spatial_lag_controls <- read_dta("Data/Clean/data_nonrefugee_immigrant_spatial_lag_controls.dta")
data_main_econ_fund <- read_dta("Data/Clean/data_main_econ_fundamentals.dta")
data_main_political_economy <- read_dta("Data/Clean/data_main_political_economy.dta")
data_main_origin_czone_climate_euclid_dist <- read_dta("Data/Clean/data_main_origin_czone_climate_euclid_dist.dta")
data_imm_wts <- read_dta("Data/Clean/data_nonrefugee_immigrant_wts.dta")

immpop_propwgtd_cohort_czone <- read_dta("Data/Clean/nonrefugee_immigrant_propwgtdpop_cohort_origin_czone_ipums.dta") %>% 
  select(-year) %>% arrange(cohort) %>% rename(propwgtd_pop = pop)
data_imm_propwts <- read_dta("Data/Clean/data_nonrefugee_immigrant_propwts.dta")

# join
data_imm <- data_imm_outcome_ethnic_network %>% 
  left_join(data_imm_spatial_lag_controls) %>% 
  left_join(data_main_econ_fund) %>% 
  left_join(data_main_political_economy) %>% 
  left_join(data_main_origin_czone_climate_euclid_dist) %>% 
  left_join(data_imm_wts) %>% 
  filter(cohort != 'pre1975')

# propensity wgtd cohort-origin-czone pop and cohort-origin pop weights
data_imm <- data_imm %>% 
  left_join(immpop_propwgtd_cohort_czone) %>% 
  left_join(data_imm_propwts)

not_countries <- unique(data_imm_outcome_ethnic_network$origin)[unique(data_imm_outcome_ethnic_network$origin) %notin% unique(data_main_origin_czone_climate_euclid_dist$origin)]

data_imm <- data_imm %>% filter(origin %notin% not_countries)

data_imm <- data_imm %>% 
  mutate(cohort = factor(cohort, levels = c('7580','8090','9199','0004'))) %>% # as factor with levels
  mutate(emp_pop_ratio = emp_pop_ratio * 100,
         resid_lagged_contpop = resid_lagged_contpop / 10,
         resid_lagged_fbpop = resid_lagged_fbpop / 100,
         resid_lagged_totpop = resid_lagged_totpop / 1000,
         lagged_popov25_4ycol = lagged_popov25_4ycol / 100) # %>% 
  # mutate(temp_euclid_distance_interaction_temp_range_euclid_distance = temp_euclid_dist * temp_range_euclid_dist) # to be able to name the interaction in reg tables

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
# baseline0_vars <- c("nearby_arrivals_idw_norm")
baseline_vars <- c("lagged_originpop", "nearby_arrivals_idw_norm", "lagged_nearby_originpop_idw_norm")
a1_vars <- c("wage", "emp_pop_ratio")
a2_vars <- c("hp", "emp_pop_ratio")
a_vars <- c("wage", "hp", "emp_pop_ratio")
c_vars <- c("resid_lagged_contpop", "resid_lagged_fbpop", "resid_lagged_totpop")
d_vars <- c("lagged_popov25_4ycol")
e_vars <- c("voteshare_president_democrat", "expend_pcpi_norm")
f_vars <- c("temp_czone","temp_range_czone","precip_czone", "temp_euclid_dist", "temp_range_euclid_dist", "precip_euclid_dist")

# build formulas
rhs_formulas <- list()

# rhs_formulas$baseline0 <- paste(paste(baseline0_vars, collapse = " + "), " | fe_vars")
rhs_formulas$baseline <- paste(paste(baseline_vars, collapse = " + "), " | fe_vars")
rhs_formulas$a1 <- paste(paste(c("lagged_originpop", a1_vars), collapse = " + "), " | fe_vars")
rhs_formulas$a2 <- paste(paste(c("lagged_originpop", a2_vars), collapse = " + "), " | fe_vars")
rhs_formulas$a <- paste(paste(c("lagged_originpop", a_vars), collapse = " + "), " | fe_vars")
# rhs_formulas$abaseline0 <- paste(paste(c(baseline0_vars, a_vars), collapse = " + "), " | fe_vars")
rhs_formulas$b <- paste(paste(c(baseline_vars, a_vars), collapse = " + "), " | fe_vars")
rhs_formulas$c <- paste(paste(c(baseline_vars, a_vars, c_vars), collapse = " + "), " | fe_vars")
rhs_formulas$d <- paste(paste(c(baseline_vars, a_vars, c_vars, d_vars), collapse = " + "), " | fe_vars")
# rhs_formulas$e <- paste(paste(c(baseline_vars, a_vars, c_vars, d_vars, e_vars), collapse = " + "), " | fe_vars")
# rhs_formulas$fbaseline0 <- paste(paste(c(baseline0_vars, a_vars, c_vars, d_vars, e_vars, f_vars), collapse = " + "), " | fe_vars")
rhs_formulas$f <- paste(paste(c(baseline_vars, a_vars, c_vars , d_vars, e_vars, f_vars), collapse = " + "), " | fe_vars")

dict = c(
  "pop" = "Immigrant Arrivals into CZ",
  "propwgtd_pop" = "Propensity Reweighted Immigrant Arrivals into CZ",
  "placements" = "Number of Refugees Resettled in CZ",
  "net_inflow" = "Net Inflow into CZ",
  "lagged_originpop" = "Lag Ethnic CZ Population",
  "nearby_arrivals_idw_norm" = "Ethnic Arrivals in Neighboring CZs",
  "lagged_nearby_originpop_idw_norm" = "Lag Ethnic Pop. in Neighboring CZs",
  "wage" = "Lag Wage Index",
  "hp" = "Lag Rent Index",
  "emp_pop_ratio" = "Avg. Emp-to-Pop Ratio",
  "resid_lagged_contpop" = "Residual Lag Continent Pop. / 10",
  "resid_lagged_fbpop" = "Residual Lag Foreign-born Pop. / 100",
  "resid_lagged_totpop" = "Residual Lag Total Pop. / 1000",
  "lagged_popov25_4ycol" = "Lag College Educated Pop. / 100",
  "voteshare_president_democrat" = "Democrat Voteshare (5/10 Year Rolling Avg)",
  "expend_pcpi_norm" = "State Expenditure-to-Income Ratio (5/10 Year Rolling Avg)",
  "temp_czone" = "Temperature CZ",
  "temp_range_czone" = "Temperature Fluctuation CZ",
  "precip_czone" = "Rainfall CZ",
  "temp_euclid_dist" = "Temperature Euclid. Dist. (CZ vs. Origin)",
  "temp_range_euclid_dist" = "Temperature Fluctuation Euclid. Dist. (CZ vs. Origin)",
  "precip_euclid_dist" = "Rainfall Euclid. Dist. (CZ vs. Origin)",
  "cohort" = "Cohort",
  "origin" = "Origin",
  "czone" = "Commuting Zone",
  "temp_euclid_distance_interaction_temp_range_euclid_distance" = "Temp. Similarity  X Temp. Fluctuation Similarity (CZ vs. Origin)"
)

varorder <- unname(dict[c(baseline_vars, a_vars)]) %>% 
  gsub("\\(", "\\\\(", .) %>% 
  gsub("\\)", "\\\\)", .)

drop_vars <- unname(dict[c(e_vars, f_vars)]) %>% 
  gsub("\\(", "\\\\(", .) %>% 
  gsub("\\)", "\\\\)", .) %>% 
  gsub("\\.", "\\\\.", .)

drop_vars_slides <- unname(dict[c(c_vars, d_vars, e_vars, f_vars)]) %>% 
  gsub("\\(", "\\\\(", .) %>% 
  gsub("\\)", "\\\\)", .) %>% 
  gsub("\\.", "\\\\.", .)

groups <- list("State Democrat Voteshare" = drop_vars[1], 
               "State Expend. to per-capita Inc." = drop_vars[2],
               # "CZ Climate" = drop_vars[3:5],
               "CZ-Origin Climate Difference" = drop_vars[3:8])

groups_slides <- list("Population Measures" = drop_vars_slides[1:4],
                      "State Democrat Voteshare" = drop_vars_slides[5], 
                      "State Expend. to per-capita Inc." = drop_vars_slides[6],
                      # "CZ Climate" = drop_vars[3:5],
                      "CZ-Origin Climate Difference" = drop_vars_slides[7:12])

highlight_vars = list(
  "rowcol, lightblue!30, se" = c("Wage", "Rent","Emp"),
  "rowcol, orangered!15, se" = c("Ethnic Arrivals in Neighboring CZs", "Lag Ethnic CZ Population", "Lag Ethnic Pop. in Neighboring CZs")
)

note <- "\\textit{Note:} Clustered (commuting zone) standard-errors in parentheses. The term \"ethnic\" refers to origin-specific measures. Residual population measures are calculated by subracting the subset of population already controlled for in the preceding population controls. Lagged variables are measured in the initial year of the period corresponding to each arrival cohort. Average values are computed over the full span of arrival years for each cohort."

## run regressions - pooled origin

# cohort-origin fixed effects

# set fixed effects and depvar
fe_vars <- "cohort^origin"
depvar <- "pop"
depvar2 <- "propwgtd_pop"
obswts <- "popwt_origin_cohort"
obswts2 <- "propwgtd_popwt_origin_cohort"

# run regressions

reg1 <- lapply(rhs_formulas, function(rhs) {
  fml <- as.formula(paste(depvar, "~", rhs))
  feols(fml, cluster = ~ czone,
        data = data_imm)
})

reg1w <- lapply(rhs_formulas, function(rhs) {
  fml <- as.formula(paste(depvar, "~", rhs))
  feols(fml, cluster = ~ czone, weights = ~ popwt_origin_cohort, # change weights with depvar
        data = data_imm)
})

reg1_prpw <- lapply(rhs_formulas, function(rhs) {
  fml <- as.formula(paste(depvar2, "~", rhs))
  feols(fml, cluster = ~ czone,
        data = data_imm)
})

reg1w_prpw <- lapply(rhs_formulas, function(rhs) {
  fml <- as.formula(paste(depvar2, "~", rhs))
  feols(fml, cluster = ~ czone, weights = ~ propwgtd_popwt_origin_cohort, # change weights with depvar
        data = data_imm)
})

etable(reg1, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       group = groups,
       # highlight = highlight_vars, 
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ImmArrivalRegs/yrfe.tex", replace = T)

etable(reg1w, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       group = groups,
       # highlight = highlight_vars,
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ImmArrivalRegs/yrfe_wtd.tex", replace = T)

etable(reg1_prpw, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       group = groups,
       # highlight = highlight_vars,
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ImmArrivalRegs/prpw_yrfe.tex", replace = T)

etable(reg1w_prpw, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       group = groups,
       # highlight = highlight_vars,
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ImmArrivalRegs/prpw_yrfe_wtd.tex", replace = T)

# cohort-origin + czone fixed effects

fe_vars <- "cohort^origin + czone"

reg2 <- lapply(rhs_formulas, function(rhs) {
  fml <- as.formula(paste(depvar, "~", rhs))
  feols(fml, cluster = ~ czone,
        data = data_imm)
})

reg2w <- lapply(rhs_formulas, function(rhs) {
  fml <- as.formula(paste(depvar, "~", rhs))
  feols(fml, cluster = ~ czone, weights = ~ popwt_origin_cohort, # change weights with depvar
        data = data_imm)
})

reg2_prpw <- lapply(rhs_formulas, function(rhs) {
  fml <- as.formula(paste(depvar2, "~", rhs))
  feols(fml, cluster = ~ czone,
        data = data_imm)
})

reg2w_prpw <- lapply(rhs_formulas, function(rhs) {
  fml <- as.formula(paste(depvar2, "~", rhs))
  feols(fml, cluster = ~ czone, weights = ~ propwgtd_popwt_origin_cohort, # change weights with depvar
        data = data_imm)
})

etable(reg2, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       group = groups,
       # highlight = highlight_vars,
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ImmArrivalRegs/yrfe_czonefe.tex", replace = T)

etable(reg2w, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       group = groups,
       # highlight = highlight_vars,
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ImmArrivalRegs/yrfe_czonefe_wtd.tex", replace = T)

etable(reg2_prpw, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       group = groups,
       # highlight = highlight_vars,
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ImmArrivalRegs/prpw_yrfe_czonefe.tex", replace = T)

etable(reg2w_prpw, digits = "r3", digits.stats = "r4",
       dict = dict, order = varorder, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       group = groups,
       # highlight = highlight_vars,
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ImmArrivalRegs/prpw_yrfe_czonefe_wtd.tex", replace = T)


# ## regressions: country-by-country 
# 
# # which countries have single vs multiple cohorts
# 
# single_cohorts <- data_main_woties %>%
#   # data_main %>% 
#   filter(in_sample==1) %>% 
#   count(origin,cohort) %>% 
#   count(origin) %>% 
#   filter(n==1) %>% 
#   select(origin)
# 
# multiple_cohorts <- data_main_woties %>% 
#   # data_main %>%
#   filter(in_sample==1) %>% 
#   count(origin,cohort) %>% 
#   count(origin) %>% 
#   filter(n>1) %>% 
#   select(origin)
# 
# all_countries <- single_cohorts %>% 
#   bind_rows(multiple_cohorts)
# 
# # country-by-country regs
# 
# yrfe_only <- list()
# yrfe_czonefe <- list()
# 
# 
# for (i in all_countries$origin) { 
#   
#   print(i)
#   
#   # filtering out refs with ties
#   yrfe_only[[paste(i)]] <- feols(net_inflow ~ placements + lagged_originpop + nearby_arrivals_idw_norm + lagged_nearby_originpop_idw_norm
#                                  + wage
#                                  + hp
#                                  + emp_pop_ratio
#                                  + lagged_popov25_4ycol
#                                  + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
#                                  # + voteshare_president_democrat + expend_pcpi_norm
#                                  | cohort,
#                                  cluster = ~ czone, 
#                                  data = data_main_woties %>% filter(in_sample == 1, origin == i)
#                                  # data = data_main %>% filter(in_sample == 1, origin == i)
#                                  )
#   
# }
# 
# # etable(yrfe_only, headers = str_to_title(names(yrfe_only)),
#        digits = "r3", digits.stats = "r4", dict = dict, view = TRUE,
#        file = "Output/Tables/regs/latex_tables_paper/SecondarymigDriversRegs/yrfe_byorigin.tex", replace = T)
# 
# 
# for (j in multiple_cohorts$origin) { 
#   
#   print(j)
#   
#   # filtering out refs with ties
#   yrfe_czonefe[[paste(j)]] <- feols(net_inflow ~ placements + lagged_originpop + nearby_arrivals_idw_norm + lagged_nearby_originpop_idw_norm
#                                     + wage
#                                     + hp
#                                     + emp_pop_ratio
#                                     + lagged_popov25_4ycol
#                                     + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
#                                     # + voteshare_president_democrat + expend_pcpi_norm
#                                     | cohort + czone,
#                                     cluster = ~ czone, 
#                                     data = data_main_woties %>% filter(in_sample == 1, origin == j)
#                                     # data = data_main %>% filter(in_sample == 1, origin == j)
#   )
#   
# }
# 
# # etable(yrfe_czonefe, headers = str_to_title(names(yrfe_czonefe)),
#        digits = "r3", digits.stats = "r4", dict = dict, view = TRUE,
#        file = "Output/Tables/regs/latex_tables_paper/SecondarymigDriversRegs/yrfe_czonefe_byorigin.tex", replace = T)
# 
# 
# # Only controlling for fundamentals
# 
# yrfe_only_fund_only <- list()
# 
# for (i in all_countries$origin) { 
#   
#   print(i)
#   
#   # filtering out refs with ties
#   yrfe_only_fund_only[[paste(i)]] <- feols(net_inflow ~
#                                  + wage
#                                  + hp
#                                  + emp_pop_ratio
#                                  | cohort,
#                                  cluster = ~ czone, 
#                                  data = data_main_woties %>% filter(in_sample == 1, origin == i)
#                                  # data = data_main %>% filter(in_sample == 1, origin == i)
#   )
#   
# }
# 
# # etable(yrfe_only_fund_only, headers = str_to_title(names(yrfe_only_fund_only)), 
#        digits = "r3", digits.stats = "r4", dict = dict, view = TRUE,
#        file = "Output/Tables/regs/latex_tables_paper/SecondarymigDriversRegs/yrfe_byorigin_fundonly.tex", replace = T)
# 
# 
# yrfe_czonefe_fund_only <- list()
# 
# for (j in multiple_cohorts$origin) { 
#   
#   print(j)
#   
#   # filtering out refs with ties
#   yrfe_czonefe_fund_only[[paste(j)]] <- feols(net_inflow ~ 
#                                     + wage
#                                     + hp
#                                     + emp_pop_ratio
#                                     | cohort + czone,
#                                     cluster = ~ czone, 
#                                     data = data_main_woties %>% filter(in_sample == 1, origin == j)
#                                     # data = data_main %>% filter(in_sample == 1, origin == j)
#   )
#   
# }
# 
# # # etable(yrfe_regionfe, headers = names(yrfe_only), view = TRUE)
# # etable(yrfe_czonefe_fund_only, 
#        headers = str_to_title(names(yrfe_czonefe_fund_only)), 
#        digits = "r3", digits.stats = "r4", dict = dict, view = TRUE,
#        file = "Output/Tables/regs/latex_tables_paper/SecondarymigDriversRegs/yrfe_czonefe_byorigin_fundonly.tex", replace = T)


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