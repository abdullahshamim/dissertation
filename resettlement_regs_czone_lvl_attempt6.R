rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(fixest)

source("Code/functions.R")

# do i have to normalize by pop_nat to get quasi-exogenous allocations?

# load data
data_main_outcome_ethnic_network <- read_dta("Data/Clean/data_main_outcome_ethnic_network_vars.dta")
data_main_spatial_lag_controls <- read_dta("Data/Clean/data_main_spatial_lag_controls.dta")
data_main_econ_fund <- read_dta("Data/Clean/data_main_econ_fundamentals.dta")
data_main_political_economy <- read_dta("Data/Clean/data_main_political_economy.dta")
data_main_origin_czone_climate_euclid_dist <- read_dta("Data/Clean/data_main_origin_czone_climate_euclid_dist.dta")
data_main_wts <- read_dta("Data/Clean/data_main_wts.dta")

# join
data_main <- data_main_outcome_ethnic_network %>% 
  left_join(data_main_spatial_lag_controls) %>% 
  left_join(data_main_econ_fund) %>% 
  left_join(data_main_political_economy) %>% 
  left_join(data_main_origin_czone_climate_euclid_dist) %>% 
  left_join(data_main_wts) %>% 
  relocate(in_sample, .after = everything())

data_main <- data_main %>% 
  mutate(cohort = factor(cohort, levels = c('7580','8090','9199','0004'))) %>% # as factor with levels
  mutate(emp_pop_ratio = emp_pop_ratio * 100,
         resid_lagged_contpop = resid_lagged_contpop / 10,
         resid_lagged_fbpop = resid_lagged_fbpop / 100,
         resid_lagged_totpop = resid_lagged_totpop / 1000,
         lagged_popov25_4ycol = lagged_popov25_4ycol / 100) %>% # rescaling
  mutate(harmon_network = placements + lagged_originpop) %>% # harmonized ethnic networks
  mutate(temp_euclid_distance_interaction_temp_range_euclid_distance = temp_euclid_dist * temp_range_euclid_dist) # to be able to name the interaction in reg tables

data_main_woties <- data_main %>% filter(origin %notin% c('cuba','iran','poland', 'romania','ussr'))
data_main_wties <- data_main %>% filter(origin %in% c('cuba','iran','poland', 'romania','ussr'))

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
baseline_vars <- c("lagged_originpop", "nearby_arrivals_idw_norm", "lagged_nearby_originpop_idw_norm")
a1_vars <- c("wage", "emp_pop_ratio")
a2_vars <- c("hp", "emp_pop_ratio")
a_vars <- c("wage", "hp", "emp_pop_ratio")
c_vars <- c("resid_lagged_contpop", "resid_lagged_fbpop", "resid_lagged_totpop")
d_vars <- c("lagged_popov25_4ycol")
e_vars <- c("voteshare_president_democrat", "expend_pcpi_norm")
f_vars <- c("temp_czone","temp_range_czone","precip_czone", "temp_euclid_dist", "temp_range_euclid_dist", "precip_euclid_dist")

# build formulas
formulas <- list()

formulas$baseline <- as.formula(paste("placements ~ ", paste(baseline_vars, collapse = " + "), " |  fe_vars"))
formulas$a1 <- as.formula(paste("placements ~ ", paste(c("nearby_arrivals_idw_norm", a1_vars), collapse = " + "), " |  fe_vars"))
formulas$a2 <- as.formula(paste("placements ~ ", paste(c("nearby_arrivals_idw_norm", a2_vars), collapse = " + "), " |  fe_vars"))
formulas$a <- as.formula(paste("placements ~ ", paste(c("nearby_arrivals_idw_norm", a_vars), collapse = " + "), " |  fe_vars"))
formulas$b <- as.formula(paste("placements ~ ", paste(c(baseline_vars, a_vars), collapse = " + "), " |  fe_vars"))
formulas$c <- as.formula(paste("placements ~ ", paste(c(baseline_vars, a_vars, c_vars), collapse = " + "), " |  fe_vars"))
formulas$d <- as.formula(paste("placements ~ ", paste(c(baseline_vars, a_vars, c_vars, d_vars), collapse = " + "), " |  fe_vars"))
# formulas$e <- as.formula(paste("placements ~ ", paste(c(baseline_vars, a_vars, c_vars, d_vars, e_vars), collapse = " + "), " |  fe_vars"))
formulas$f <- as.formula(paste("placements ~ ", paste(c(baseline_vars, a_vars, d_vars, c_vars, e_vars, f_vars), collapse = " + "), " |  fe_vars"))

dict = c(
  "placements" = "Number of Refugees Resettled in CZ",
  "lagged_originpop" = "Lag Ethnic CZ Population",
  "nearby_arrivals_idw_norm" = "Ethnic Resettl. in Neighboring CZs",
  "lagged_nearby_originpop_idw_norm" = "Lag Ethnic Pop. in Neighboring CZs",
  "wage" = "Lag Hourly Wage Index",
  "hp" = "Lag Gross Rent Index",
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
  "\\-" = " \\X ",
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
  "rowcol, orangered!15, se" = c("Ethnic Resettl. in Neighboring CZs", "Lag Ethnic CZ Population", "Lag Ethnic Pop. in Neighboring CZs")
)

note <- "\\textit{Notes:} Clustered (commuting zone) standard-errors in parentheses. The term \"ethnic\" refers to origin-specific measures. Residual population measures are calculated by subracting the subset of population already controlled for in the preceding population controls. Lagged variables are measured in the initial year of the period corresponding to each arrival cohort. Average values are computed over the full span of arrival years for each cohort."

## run regressions - pooled origin

# cohort-origin fixed effects

fe_vars <- "cohort^origin"

reg1 <- lapply(formulas, function(fml) {
  feols(as.formula(fml),
        cluster = ~ czone,
        data = data_main_woties %>% filter(in_sample == 1))
})


reg1w <- lapply(formulas, function(fml) {
  feols(as.formula(fml),
        cluster= ~ czone, weights = ~ popwt_origin_cohort_dreher,
        data = data_main_woties %>% filter(in_sample == 1))
})

etable(reg1, digits = "r3", digits.stats = "r4",
       order = varorder, dict = dict, view = T,
       group = groups,
       # highlight = highlight_vars, 
       style.tex = style.tex(yesNo = c("Y","N")),
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe.tex", replace = T)

etable(reg1w, digits = "r3", digits.stats = "r4",
       order = varorder, dict = dict, view = T, 
       group = groups, 
       # highlight = highlight_vars,
       style.tex = style.tex(yesNo = c("Y","N")),
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe_wtd.tex", replace = T)

# etable(reg1, digits = "r3", digits.stats = "r4", order = order, dict = dict, tex = T, view = T, file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe.tex", replace = T)
# etable(reg1w, digits = "r3", digits.stats = "r4", dict = dict, view = T, file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe_wtd.tex", replace = T)

# cohort-origin + czone fixed effects

fe_vars <- "cohort^origin + czone"

reg2 <- lapply(formulas, function(fml) {
  feols(as.formula(fml),
        cluster= ~ czone,
        data = data_main_woties %>% filter(in_sample == 1))
})

reg2w <- lapply(formulas, function(fml) {
  feols(as.formula(fml),
        cluster= ~ czone, weights = ~ popwt_origin_cohort_dreher,
        data = data_main_woties %>% filter(in_sample == 1))
})

etable(reg2, digits = "r3", digits.stats = "r4",
       order = varorder, dict = dict, view = T,
       group = groups, 
       # highlight = highlight_vars,
       style.tex = style.tex(yesNo = c("Y","N")),
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe_czonefe.tex", replace = T)

etable(reg2w, digits = "r3", digits.stats = "r4",
       order = varorder, dict = dict, view = T,
       group = groups, 
       # highlight = highlight_vars,
       style.tex = style.tex(yesNo = c("Y","N")),
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe_czonefe_wtd.tex", replace = T)

# etable(reg2, digits = "r3", digits.stats = "r4", dict = dict, view = T, file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe_czonefe.tex", replace = T)
# etable(reg2w, digits = "r3", digits.stats = "r4", dict = dict, view = T, file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe_czonefe_wtd.tex", replace = T)

## run regressions: country-by-country

origins_woties_insample <- data_main_woties %>% 
  filter(in_sample == 1) %>% select(origin) %>% unique()

regs_byorigin <- list()

# cohort fe

for (k in origins_woties_insample$origin) { 
  
  print(k)
  
  # filtering out refs with ties
  regs_byorigin[[paste(k)]] <- 
    feols(placements ~ lagged_originpop 
          + nearby_arrivals_idw_norm
          + lagged_nearby_originpop_idw_norm
          + wage + hp + emp_pop_ratio
          + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
          + lagged_popov25_4ycol
          + voteshare_president_democrat + expend_pcpi_norm
          + temp_euclid_dist + temp_range_euclid_dist + precip_euclid_dist
          | cohort,
          cluster = ~ czone, 
          data = data_main_woties %>% filter(origin == k, in_sample == 1))
  
}

etable(regs_byorigin, headers = str_to_title(names(regs_byorigin)),
       digits = "r3", digits.stats = "r4", view = T,
       order = varorder, dict = dict,
       group = groups, 
       # highlight = highlight_vars,
       style.tex = style.tex(yesNo = c("Y","N")),
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe_byorigin.tex", replace = T)

# cohort + czone fe

origins_woties_multiple_cohorts_insample <- data_main_woties %>% 
  # data_main %>%
  filter(in_sample==1) %>% 
  count(origin,cohort) %>% 
  count(origin) %>% 
  filter(n>1) %>% 
  select(origin)

regs_byorigin_czonefe <- list()

for (k in origins_woties_multiple_cohorts_insample$origin) { 
  
  print(k)
  
  # filtering out refs with ties
  regs_byorigin_czonefe[[paste(k)]] <- 
    feols(placements ~ lagged_originpop 
          + nearby_arrivals_idw_norm
          + lagged_nearby_originpop_idw_norm
          + wage + hp + emp_pop_ratio
          + resid_lagged_contpop + resid_lagged_fbpop + resid_lagged_totpop
          + lagged_popov25_4ycol
          + voteshare_president_democrat + expend_pcpi_norm 
          | cohort + czone,
          cluster = ~ czone, 
          data = data_main_woties %>% filter(origin == k, in_sample == 1))
  
}

etable(regs_byorigin_czonefe, headers = str_to_title(names(regs_byorigin_czonefe)), 
       digits = "r3", digits.stats = "r4", 
       # highlight = highlight_vars,
       group = groups[1:2],
       order = varorder, dict = dict, view = T,
       style.tex = style.tex(yesNo = c("Y","N")),
       tpt = T, notes = note,
       file = "Output/Tables/regs/latex_tables_paper/ResettlementDriversRegs/yrfe_czonefe_byorigin.tex", replace = T)

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
# # etable(reg3_firstcoh, reg3_czonefe_firstcoh, reg3_latercoh, reg3_czonefe_latercoh,
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
# # etable(resettlementregs_firstcohort_byorigin, headers = names(resettlementregs_firstcohort_byorigin), view = TRUE)
# # etable(resettlementregs_latercohort_byorigin, headers = names(resettlementregs_latercohort_byorigin), view = TRUE)