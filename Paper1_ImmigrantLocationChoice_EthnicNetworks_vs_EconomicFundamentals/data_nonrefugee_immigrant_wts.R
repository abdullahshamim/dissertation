rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)

source("Code/functions.R")

# read data
immarrivals_origin_czone <- read_dta("Data/Clean/nonrefugee_immigrant_pop_cohort_origin_czone_ipums.dta")
propwgtd_immarrivals_origin_czone <- read_dta("Data/Clean/nonrefugee_immigrant_propwgtdpop_cohort_origin_czone_ipums.dta")


# origin-cohort overall population weights across US
immarrivals_origin_us <- immarrivals_origin_czone %>% 
  group_by(cohort,origin) %>% 
  summarize(pop = sum(pop)) %>% 
  rename(popwt_origin_cohort = pop)

# origin-cohort overall population weights across US
propwgtd_immarrivals_origin_us <- propwgtd_immarrivals_origin_czone %>% 
  group_by(cohort,origin) %>% 
  summarize(pop = sum(pop)) %>% 
  rename(propwgtd_popwt_origin_cohort = pop)

write_dta(immarrivals_origin_us, 'Data/Clean/data_nonrefugee_immigrant_wts.dta')
write_dta(propwgtd_immarrivals_origin_us, 'Data/Clean/data_nonrefugee_immigrant_propwts.dta')
