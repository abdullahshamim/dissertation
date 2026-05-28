rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(ipumsr)
library(fixest)
library(labelled)
library(countrycode)
library(scales)

source("Code/functions.R")

# read refpop from ipums data
refpop_czone_ipums <- read_dta("Data/Clean/refugee_pop_czone_ipums.dta")

# read refarrivals data from Dreher et al
refarrivals_czone <- read_dta("Data/Clean/refugee_arrivals_czone_dreher23.dta")

# # read refpop from nhgis data
# refpop_czone_nhgis <- read_dta("Data/Clean/refpop_czone_census.dta")

# cb_palette <- c(
#   "#E69F00", # orange
#   "#56B4E9", # sky blue
#   "#009E73", # bluish green
#   "#F0E442", # yellow
#   "#0072B2", # blue
#   "#D55E00", # vermillion
#   "#CC79A7"  # reddish purple
# )

# aggregate pop from refugee origins - ipums
refpop_us_ipums <- refpop_czone_ipums %>% 
  group_by(year,cntryname) %>% 
  summarize(pop_ipums = sum(refpop_ipums))

# # aggregate pop from refugee origins - nhgis
# refpop_us_nhgis <- refpop_czone_nhgis %>% 
#   group_by(year,origin) %>% 
#   summarize(pop_nhgis=sum(pop, na.rm = T))

# aggregate refugee arrivals - dreher
refarrivals_us_year <- refarrivals_czone %>% 
  group_by(year,citizenship_stable) %>% 
  summarize(refugees = sum(refugees)) %>% 
  group_by(citizenship_stable) %>% 
  mutate(cum_refugees = cumsum(refugees))

refarrivals_us <- refarrivals_us_year %>% 
  # filter(year %in% c(1970,1980,1990,2000,2004,2008)) %>% 
  filter(year %in% unique(refpop_us_ipums$year)) %>% 
  select(year,citizenship_stable,cum_refugees)

# aggregate pop from refugee origins - ipums
refpop_comparison_sources <- refpop_us_ipums %>% 
  mutate(cntryname = tolower(cntryname)) %>% 
  # left_join(refpop_us_nhgis, join_by(year,cntryname == origin)) %>% 
  left_join(refarrivals_us, join_by(year, cntryname == citizenship_stable)) %>% 
  # group_by(cntryname) %>% 
  # mutate(refpop_cohort_decade = refpop - lag(refpop),
  #        refarrivals_cohort_decade = cum_refugees - lag(cum_refugees)) %>% 
  arrange(cntryname,year)

refpop_comparison_sources <- refpop_comparison_sources %>% 
  select(year, cntryname, pop_ipums, cum_refugees) %>% 
  rename(ipums = pop_ipums, dreher = cum_refugees) %>% 
  mutate(cntryname = str_to_title(cntryname))

refpop_comparison_sources <- refpop_comparison_sources %>% 
  pivot_longer(c(ipums, dreher), names_to = "data_source", values_to = "count")

# important figure
figure <- ggplot(refpop_comparison_sources %>% filter(data_source != 'nhgis'),
                 aes(x=year,y=count,color=data_source,shape=data_source)) + 
  geom_line() + 
  geom_point()+
  theme_bw() +
  labs(x = NULL,
       y = "Estimated Population Size in U.S. (1k = 1,000)",
       color = "Data Source", shape = "Data Source") +
  scale_color_discrete(labels = c("Dreher et al (2023)", "Census/ACS")) +
  scale_shape_manual(values = c(17, 16), labels = c("Dreher et al (2023)", "Census/ACS")) +
  theme(axis.text.x = element_text(angle = 90))+
  theme(legend.position = "top",
        legend.box.margin = margin(t = -10, b = -10))+
  theme(axis.title.y = element_text(margin = margin(r = 5)))+
  scale_y_continuous(labels = label_number(scale = 1/1000, suffix = "k")) +
  facet_wrap(~cntryname, scales = "free_y")

figure

ggsave("Output/Figures/Refarrivals_Dreher_Refpop_Ipums_Comparison.png",
       height = 6, width = 7, units = "in", figure) 

# ## table for origin-cohort level refugee probability (refugee-to-immigrant ratio)
# 
# # read refpop by cohort from ipums data
# refpop_cohort_czone_ipums <- read_dta("Data/Clean/refpop_cohort_czone_ipums.dta") %>% 
#   mutate(cntryname = tolower(cntryname))
# 
# refpop_cohort_us_ipums <- refpop_cohort_czone_ipums %>%
#   group_by(year,cohort,cntryname) %>%
#   summarize(refpop_ipums=sum(refpop_ipums)) %>% 
#   filter( (year == 1970 & cohort == 'pre1975') |
#            (year == 1980 & cohort == '7580') | 
#            (year == 1990 & cohort == '8090') |
#            (year == 2000 & cohort == '9199') |
#            (year == 2007 & cohort == '0004')) %>% 
#   ungroup() %>% 
#   # select(-year) %>% 
#   arrange(cntryname,cohort)
# 
# # had to duplicate yrimmig 1980 since arrivals in 1980 not separately identifiable
# refarrivals_cohort_us <- refarrivals_us_year %>%
#   filter(year %in% c(1979,1990,1999,2004)) %>% # cohort ending years
#   group_by(citizenship_stable) %>%
#   mutate(refugees = cum_refugees-lag(cum_refugees, default = 0)) %>% # cohort level arrivals
#   bind_rows(refarrivals_us_year %>% filter(year == 1980) %>% mutate(refugees = cum_refugees)) %>% 
#   filter(year != 1979) %>% 
#   mutate(cohort = case_match(year, 
#                              1980 ~ "7580",
#                              1990 ~ "8090", 
#                              1999 ~ "9199",
#                              2004 ~ "0004"), .after = year) %>% 
#   select(-year,-cum_refugees) %>% 
#   arrange(citizenship_stable, cohort)
# 
# refprob_cohort <- refpop_cohort_us_ipums %>% 
#   left_join(refarrivals_cohort_us, join_by(cohort, cntryname==citizenship_stable)) %>% 
#   rename(arrivals_ipums = refpop_ipums, arrivals_dreher = refugees) %>% 
#   mutate(refprob = arrivals_dreher / arrivals_ipums)
# 
# refcohorts_insample <- refprob_cohort %>% 
#   filter(arrivals_dreher > 15000, refprob > 0.7) %>% 
#   mutate(in_sample = 1)
# 
# refcohorts_all_insample <- refprob_cohort %>% 
#   mutate(across(c(arrivals_dreher,refprob), ~replace_na(., 0))) %>% 
#   mutate(in_sample = ifelse(arrivals_dreher > 15000 & refprob > 0.7, 1, 0))
# 
# # for more intuitive arrangement
# refcohorts_all_insample <- refcohorts_all_insample %>% 
#   mutate(cohort=factor(cohort,levels=c('7580','8090','9199','0004'))) %>% 
#   arrange(cntryname,cohort)
# 
# # table showing origins and cohorts in my sample
# # along with arrivals as per Dreher23, ipums, and the refugee probability
# knitr::kable(refcohorts_all_insample %>% 
#                filter(in_sample==1) %>% 
#                select(-year),
#              format = 'latex', booktabs = T)
# 
# # the sample selection criteria retains over 65 percent of all refugees in the time period I study (arrivals b/w 1975-2004) 
# sum(refcohorts_insample$arrivals_dreher) / sum((refarrivals_us_year %>% filter(year == 2004))$cum_refugees)
# 
# # save sample selection origin and cohort
# write_dta(refcohorts_insample, "Data/Clean/refugee_origin_cohorts_insample.dta")
# write_dta(refcohorts_all_insample, "Data/Clean/refugee_origin_cohorts_all_insample.dta")


# # ussr 
# 
# test_ussr <- data_refugee %>% 
#   filter(cntryname == 'Ussr', year == 2000, yrimmig %in% 1991:1999) %>% 
#   group_by(year,cntryname,yrimmig) %>% 
#   summarize(arrivals_ipums = sum(perwt))
# 
# refarrivals_us_ussr_year <- refarrivals_us_year %>% 
#   filter(year %in% 1991:1999, citizenship_stable == 'ussr') %>% 
#   mutate(citizenship_stable = str_to_title(citizenship_stable))
# 
# # 1991-1994 arrivals around 80% refugees
# ussr_prob <- test_ussr %>% 
#   left_join(refarrivals_us_ussr_year, join_by(yrimmig==year, cntryname==citizenship_stable)) %>% 
#   select(-year,-cum_refugees) %>% 
#   rename(arrivals_dreher=refugees) %>% 
#   mutate(refprob = arrivals_dreher / arrivals_ipums)

