rm(list = ls())

setwd("C:/Users/48070546/Desktop/GradSchool/Research/RefugeeLocationChoice_EthnicNetworks_versus_EconFund")

library(tidyverse)
library(tigris)
library(sf)
library(haven)
library(scales)
library(viridisLite)
library(paletteer)
library(viridis) 

source("Code/functions.R")

# theme_set(theme_void())

# Notes from NHGIS documentation for Foreign-Born Persons by Place of Birth [78*] - Soviet Union: In the "(Former) Soviet Union [n.e.c. 1990, 2000, ACS]" time series, the counts for years after 1990 are sums of any counts available for countries that had been part of the Soviet Union as classified at the time of the 1990 census. Specifically, because 1990 summary files provided counts for the Baltic States (Estonia, Latvia, Lithuania) separately from the Soviet Union count, this "(Former) Soviet Union" time series omits counts for the Baltic States. The 1990 summary files did not provide separate counts for any of the other countries to emerge from the Soviet Union.
# For 2000, this "(Former) Soviet Union" time series includes persons who reported Armenia, Belarus, Russia, or Ukraine as their country of birth. For years of the American Community Survey (ACS), this time series includes persons who reported Armenia, Belarus, Kazakhstan, Moldova, Russia, Ukraine, or Uzbekistan as their country of birth. The 2000 and ACS summary files do not provide distinct counts for any other countries of the former Soviet Union, nor do they provide a distinct count for the substantial number of persons who reported "USSR" for their country of birth, so NHGIS includes those categories in the "Other Europe" or "Other Asia" time series in accord with census practices.
# Because Kazakhstan, Moldova, and Uzbekistan are omitted from this "(Former) Soviet Union" time series in 2000 only, users should consider skipping over 2000 when analyzing changes in this time series.

imm_codes <- c(
  "AK2AA", "AK2AB", "AK2AC", "AK2AD", "AK2AE", "AK2AF", "AK2AG", "AK2AH", "AK2AI", "AK2AJ",
  "AK2AK", "AK2AL", "AK2AM", "AK2AN", "AK2AO", "AK2AP", "AK2AQ", "AK2AR",
  "AK2AV", "AK2AW", "AK2AX", "AK2AY", "AK2AZ", "AK2BA", "AK2BB", "AK2BC", "AK2BD",
  "AK2BE", "AK2BF", "AK2BG", "AK2BH", "AK2BI", "AK2BJ", "AK2BK", "AK2BL", "AK2BM", "AK2BN",
  "AK2BO", "AK2BP", "AK2BQ", "AK2BR", "AK2BS", "AK2BT", "AK2BU", "AK2BV", "AK2BW", "AK2BX",
  "AK2BY", "AK2BZ", "AK2CA", "AK2CB", "AK2CC", "AK2CD", "AK2CE", "AK2CF", "AK2CG", "AK2CH",
  "AK2CI", "AK2CJ", "AK2CK", "AK2CL", "AK2CM", "AK2CN", "AK2CO", "AK2CP", "AK2CQ", "AK2CR",
  "AK2CS", "AK2CT", "AK2CU", "AK2CV", "AK2CW", "AK2CX", "AK2CY", "AK2CZ"
)

imm_countries <- c(
  "Ireland", "Sweden", "United Kingdom", "Austria", "France", "Germany", "Netherlands", "Greece", 
  "Italy", "Portugal", "Spain", "Czechoslovakia (includes Czech Republic and Slovakia)", "Hungary", 
  "Poland", "Romania", "Other Europe", "(Former) Soviet Union", "China (includes Hong Kong and Taiwan)", 
  "Japan", "Korea", "Afghanistan", 
  "India", "Iran", "Pakistan", "Cambodia", "Indonesia", "Laos", "Malaysia", "Philippines", 
  "Thailand", "Vietnam", "Iraq", "Israel", "Jordan", "Lebanon", "Syria", "Turkey", "Other Asia", 
  "Ethiopia", "Egypt", "South Africa", "Ghana", "Nigeria", "Other Africa", "Australia", 
  "Other Australia and New Zealand Subregion", "Other Oceania", "Barbados", "Cuba", 
  "Dominican Republic", "Haiti", "Jamaica", "Trinidad and Tobago", "Other Caribbean", 
  "Mexico", "Costa Rica", "El Salvador", "Guatemala", "Honduras", "Nicaragua", "Panama", 
  "Other Central America", "Argentina", "Bolivia", "Brazil", "Chile", "Colombia", 
  "Ecuador", "Guyana", "Peru", "Venezuela", "Other South America", "Canada", 
  "Other Northern America", "Born at sea or country not reported"
)

imm_dict <- tibble(country_code = tolower(imm_codes), country_name = imm_countries)

immpop78_year_country <- read_csv("Data/Raw/nhgis/nhgis_nationalpop_imms_78countries/nhgis0021_ts_nominal_nation.csv")

immpop78_year_country <- immpop78_year_country %>% 
  rename_with(tolower) %>% 
  rename_with(~ gsub("125","2010", .)) %>% 
  rename_with(~ gsub("225","2020", .)) %>% 
  select(!contains('0m')) # drop pop error terms; shouldn't affect national estimates


immpop78_year_country <- immpop78_year_country %>% 
  pivot_longer(contains(c("1990", "2000","2010","2020")),
               names_to = c("country_code","year"),
               names_pattern = "(.*)(1990|2000|2010|2020)",
               values_to = "immpop_year_country") %>% 
  filter(country_code %in% imm_dict$country_code) %>% 
  left_join(imm_dict) %>% 
  select(year,country_name,country_code,immpop_year_country)

immpop78_year_country <- immpop78_year_country %>% 
  ungroup() %>% 
  complete(year,nesting(country_name,country_code)) %>%  # %>% filter(is.na(immpop_year_country)) # only 2010 and 2020 "born at sea" rows missing 
  mutate(immpop_year_country = ifelse(is.na(immpop_year_country), 0, immpop_year_country))
  
immpop78_year <- immpop78_year_country %>% 
  group_by(year) %>% 
  summarize(tot_immpop_year = sum(immpop_year_country))

immcomp78_year_country <- immpop78_year_country %>% 
  left_join(immpop78_year) %>% 
  mutate(imm_composition_year = immpop_year_country / tot_immpop_year)

imm_country_rank_2010 <- immpop78_year_country %>% 
  filter(year == 2010) %>% 
  arrange(desc(immpop_year_country)) %>% 
  mutate(imm_country_rank = row_number()) %>% 
  mutate(imm_country_rank = ifelse(imm_country_rank > 16, 17, imm_country_rank)) %>% 
  select(-year, - immpop_year_country)

b <- immpop78_year_country %>% left_join(imm_country_rank_2010) %>% 
  mutate(country_name = ifelse(imm_country_rank == 17, 'Other', country_name)) %>%
  group_by(year, country_name, imm_country_rank) %>% 
  summarize(immpop_year_country = sum(immpop_year_country))

bb <- b %>% left_join(immpop78_year) %>% 
  mutate(imm_composition_year = immpop_year_country / tot_immpop_year)

bb$country_name[bb$country_name == "China (includes Hong Kong and Taiwan)"] <- "China"
bb$country_name[bb$country_name == "(Former) Soviet Union"] <- "Former USSR"


c17 <- rev(c(
  "dodgerblue2", "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", "gold1",
  "skyblue2", "#FB9A99", # lt pink
  "palegreen2",
  "#CAB2D6", # lt purple
  "#FDBF6F", # lt orange
  "gray70", "khaki2",
  "maroon", "orchid1", "deeppink1"))

imm_composition <- ggplot() +
  # Stacked area chart
  geom_area(data = bb, 
            aes(x = as.numeric(year), y = imm_composition_year,
                fill = country_name, color = country_name),
            alpha = 0.8, size = 0.2) + 
  # # Overlayed line for total arrivals
  # geom_line(data = refarrivals_year, 
  #           aes(x = year, y = tot_refugees_year), 
  #           color = "black", size = 1) +
  # scale_y_continuous(labels = label_number(suffix = "K", scale = 1e-3)) +
  scale_fill_manual(values = c17,
                    breaks = unique(bb$country_name)) +  
  # Labels and theme
  # Labels and theme
  labs(x = NULL,
       y = "Immigrant Composition",
       fill = "") +
  theme_bw() + 
  theme(legend.position = 'top',
        legend.key.size = unit(0.3, "cm")) +
  theme(axis.title.y = element_text(margin = margin(r = 5)))+
  guides(fill = guide_legend(nrow = 3), color = 'none')

ggsave("Output/Figures/immigrant_composition.png",
       width = 7, height = 4, units = "in", imm_composition)

