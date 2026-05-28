rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(haven)
library(tigris)
library(scales)

source("Code/functions.R")

states <- as_tibble(states()) %>% rename_with(tolower) %>% select(statefp,name)
pcpi <- read_dta("Data/Clean/pc_perinc_state.dta")
cpi_1999base <- read_dta("Data/Clean/cpi_1999base.dta")

# state_gdppc <- read_csv("Data/Raw/_IndFin_1967-2012/state3.csv", skip = 5)
# 
# state_gdppc <- state_gdppc %>% select(State, contains('1999'))
# names(state_gdppc) <- c('state','year_1999','year_1989','year_1979','year_1969','year_1959')
# 
# state_gdppc <- state_gdppc %>% 
#   filter(state %notin% c("United States", "Alaska", "Hawaii"), !is.na(state)) %>% 
#   mutate(across(contains("year"), ~ gsub(",", "", .x))) %>% 
#   mutate(across(contains("year"), as.numeric)) %>% 
#   left_join(states %>% ungroup() %>% select(name,statefp), join_by(state == name)) %>% 
#   mutate(statefp = ifelse(state == "D.C", '11', statefp), .after = state)
# 
# state_gdppc %>% filter(is.na(statefp))

# test for 2000 only

test <- read_csv("Data/Raw/_IndFin_1967-2012/IndFin00a.Txt")

test_simple <- test %>% select(SortCode:'Imputed Record', contains("Total"))

test_bb <- test %>% 
  rename_with(tolower) %>% 
  rename_with(~str_replace_all(., "\\s+", "")) %>% 
  rename(statefp = `fipscode-state`) %>% 
  select(year4,yearofdata,statefp,county,name,totalrevenue,totalexpenditure) %>% 
  filter(statefp != '02' & statefp != '15' & county == '000' | (statefp == '11' & county == '001')) %>% 
  select(-county)

# get all years

files <- list.files(
  path = "Data/Raw/_IndFin_1967-2012",
  pattern = "^IndFin\\d{2}a\\.Txt$",
  full.names = TRUE
)

# read each file into list
file_list <- map(files, ~ read_csv(.x) %>%
                   rename_with(tolower) %>%
                   rename_with(~ str_replace_all(., "\\s+", "")) %>%
                   rename(statefp = `fipscode-state`) %>%
                   select(year4, yearofdata, statefp, county, name, typecode, totalrevenue, totalexpenditure) %>%
                   filter(statefp != '02' & statefp != '15' & county == '000' | (statefp == '11' & typecode == 2)) %>% # should retain Washington DC
                   select(-county,-typecode)
)

# combine into dataframe
data <- bind_rows(file_list) 
names(data) <- c('year','year_survey','statefp', 'statename', 'revenue_tot', 'expend_tot')

data <- data %>% 
  arrange(year,statefp)

# Step 4: look at the result
glimpse(data)

# cleaning
data <- data %>% 
  filter(year_survey!="BB", year != 2012) %>% 
  group_by(year,statefp) %>% 
  summarize(revenue_tot = sum(revenue_tot), expend_tot = sum(expend_tot)) %>% 
  left_join(states) %>% 
  rename(statename = name) %>% 
  select(year,statefp,statename,revenue_tot,expend_tot)

data %>% ungroup() %>% count(year) %>% print(n=50)

data <- data %>% filter(year %notin% c(1967:1971, 1973:1976)) # only data for Washington DC available in these years

# plot some data
p <- ggplot(data %>% filter(statefp %in% c('01', '10', '20', '25', '30', '35', '40', '45', '50')),
       aes(x = year)) +
  geom_line(aes(y = revenue_tot, linetype = "Revenue"), color = "red3") +
  geom_line(aes(y = expend_tot, linetype = "Expenditure"), color = 'blue4') +
  facet_wrap(~statename) +
  labs(y="Total State Revenue (1m = 1,000,000)", x=NULL, linetype=NULL)+
  scale_y_continuous(labels = label_number(scale = 1/1000000, suffix = "m")) +
  theme_bw() +
  theme(legend.position = 'top',
        legend.box.margin = margin(t = -10, b = -10)) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))
  
p

ggsave('Output/Figures/state_revenue_expenditure_timeseries.png',
       height = 6, width = 7, units = "in", p)


# new

expend_norm <- data %>% 
  left_join(pcpi %>% select(year,statefp10,pcpi), join_by(year, statefp == statefp10)) %>% 
  mutate(expend_pcpi_norm = expend_tot / pcpi)

expend_norm_real <- expend_norm %>% 
  left_join(cpi_1999base) %>% 
  mutate(expend_pcpi_norm_real = expend_pcpi_norm / cpi_1999base, .after = expend_pcpi_norm)

# end new

write_dta(data, "Data/Clean/state_revenue_expenditure.dta")
write_dta(expend_norm, "Data/Clean/state_expenditure_pcinc_normalized.dta")
