rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(jsonlite)
library(httr)
library(haven)
library(tigris)

state_voteshare_president <- read_csv("Data/Raw/historic_voting_patterns/president/1976-2020-president.csv")
state_voteshare_senate <- read_csv("Data/Raw/historic_voting_patterns/senate/1976-2020-senate.csv")
state_voteshare_representative <- read_csv("Data/Raw/historic_voting_patterns/representative/1976-2022-house.csv")

state_voteshare_president <- state_voteshare_president %>% select(!contains(c('_po','_ic','_cen')))
state_voteshare_senate <- state_voteshare_senate %>% select(!contains(c('_po','_ic','_cen')))

state_voteshare_president_party <- state_voteshare_president %>% 
  group_by(year, state, state_fips, party_simplified) %>% 
  summarize(partyvotes = sum(candidatevotes), totalvotes = mean(totalvotes))

state_voteshare_senate_party <- state_voteshare_senate %>% 
  group_by(year, state, state_fips, special, party_simplified) %>% 
  summarize(partyvotes = sum(candidatevotes), totalvotes = mean(totalvotes))

# correction for uncontested elections
state_voteshare_senate_party <- state_voteshare_senate_party %>% ungroup() %>% 
  expand(nesting(year, state, state_fips,special), party_simplified) %>% 
  left_join(state_voteshare_senate_party) %>% 
  mutate(partyvotes=ifelse(is.na(partyvotes), 0, partyvotes)) %>% 
  group_by(year,state_fips,special) %>% 
  mutate(totalvotes=mean(totalvotes,na.rm=T))

state_voteshare_president_party %>% 
  ungroup %>% 
  mutate(num_na = rowSums(across(everything(), is.na))) %>% 
  filter(num_na > 0) # no na values

state_voteshare_senate_party %>% 
  ungroup %>% 
  mutate(num_na = rowSums(across(everything(), is.na))) %>% 
  filter(num_na > 0) # no na values

#### new ####

# senate
state_voteshare_senate_party <- state_voteshare_senate_party %>%
  mutate(voteshare_senate = round(partyvotes / totalvotes, digits = 2)) %>% # first calculate voteshare keeping all parties
  select(year,special,state_fips,state,party_simplified,voteshare_senate) %>%
  filter(party_simplified %in% c('DEMOCRAT','REPUBLICAN')) %>% # filter only major parties
  mutate(state = str_to_title(state), party_simplified = tolower(party_simplified)) %>%
  rename(statefp = state_fips, statename=state) %>%
  mutate(statefp = str_pad(statefp, width = 2, pad = '0')) %>%
  mutate(statename = ifelse(statefp == '11', 'District of Columbia', statename)) # for conformity with other data

# president
state_voteshare_president_party <- state_voteshare_president_party %>%
  mutate(voteshare_president = round(partyvotes / totalvotes, digits = 2)) %>%
  select(year,state_fips,state,party_simplified,voteshare_president) %>%
  filter(party_simplified %in% c('DEMOCRAT','REPUBLICAN')) %>%
  mutate(state = str_to_title(state), party_simplified = tolower(party_simplified)) %>%
  rename(statefp = state_fips, statename=state) %>%
  mutate(statefp = str_pad(statefp, width = 2, pad = '0')) %>%
  mutate(statename = ifelse(statefp == '11', 'District of Columbia', statename)) # for conformity with other data

#### end new ####

write_dta(state_voteshare_president_party, "Data/Clean/state_voteshare_president.dta")
write_dta(state_voteshare_senate_party, "Data/Clean/state_voteshare_senate.dta")
