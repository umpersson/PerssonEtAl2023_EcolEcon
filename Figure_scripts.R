library(tidyr)
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(ggrepel)
library(RColorBrewer)
library(viridis)
library(readxl)

# LOAD & PREPARE DATA ##########################################################

Kastner_trade <- read.csv("C:/Users/mpersson/Box/Land-balance & deforestation emissions model/Output/Zenodo/Version 1.1/Kastner_sheet.csv", sep = ",", stringsAsFactors = FALSE) # Loads data on bilateral trade flows of embodied deforestation from Pendrill et al. (2022)
Crop_groups <- read_excel("C:/Users/mpersson/Box/My folders/Trade & environment/Changing landscape - EE Special Issue '22/R script & output/Lookup_tables.xlsx", sheet = "Crops_lookup")
Country_groups <- read_excel("C:/Users/mpersson/Box/My folders/Trade & environment/Changing landscape - EE Special Issue '22/R script & output//Lookup_tables.xlsx", sheet = "Country_lookup")


# Create a dataframe with domestic & export volumes, export shares, by country, commodity group & year
Dom_exp_shares <- select(Kastner_trade, ProducerCountry, ConsumerCountry, FAO_name, Year, Deforestation_Area) %>%
  mutate(ConsumerCountry = ifelse(ProducerCountry == ConsumerCountry, "Domestic", "Export")) %>%
  left_join(., select(Crop_groups, FAO_name, Crop_group), by = "FAO_name") %>%
  group_by(ProducerCountry, Crop_group, ConsumerCountry, Year) %>%
  summarise(Deforestation_Area = sum(Deforestation_Area)) %>%
  pivot_wider(., names_from = ConsumerCountry, values_from = Deforestation_Area) %>%
  mutate(Export = ifelse(is.na(Export), 0, Export)) %>%
  mutate(Domestic = ifelse(is.na(Domestic), 0, Domestic)) %>%
  mutate(Export_share = 100 * Export / (Export + Domestic)) %>%
  ungroup()

# Create a dataframe with domestic & export volumes, export shares, average by producer country and major consumer regions
Dom_exp_shares_av <- select(Kastner_trade, ProducerCountry, ConsumerCountry, FAO_name, Year, Deforestation_Area) %>%
  left_join(., select(Country_groups, Country, Cons_region), by = c("ConsumerCountry" = "Country")) %>%
  mutate(Cons_region = ifelse(is.na(Cons_region), ifelse(ProducerCountry == ConsumerCountry, "Domestic", "Other"), Cons_region)) %>%
  filter(., Year >= 2015) %>%
  group_by(ProducerCountry, Cons_region) %>%
  summarise(Deforestation_Area = sum(Deforestation_Area)) %>%
  ungroup() %>%
  pivot_wider(., names_from = Cons_region, values_from = Deforestation_Area) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(Tot_attr_def = Domestic + Other + China + EU_UK + USA) %>%
  mutate(Domestic = 100 * Domestic / Tot_attr_def) %>%
  mutate(Other = 100 * Other / Tot_attr_def) %>%
  mutate(China = 100 * China / Tot_attr_def) %>%
  mutate(EU_UK = 100 * EU_UK / Tot_attr_def) %>%
  mutate(USA = 100 * USA / Tot_attr_def)

# Create a dataframe with import volumes for major consumer regions/countries, by producer country, crop group & year
Imp_volumes <- select(Kastner_trade, ProducerCountry, ConsumerCountry, FAO_name, Year, Deforestation_Area) %>%
  left_join(., select(Crop_groups, FAO_name, Crop_group), by = "FAO_name") %>%
  left_join(., select(Country_groups, Country, Cons_region), by = c("ConsumerCountry" = "Country")) %>%
  filter(., !is.na(Cons_region)) %>%
  group_by(ProducerCountry, Crop_group, Cons_region, Year) %>%
  summarise(Deforestation_Area = sum(Deforestation_Area)) %>%
  pivot_wider(., names_from = Cons_region, values_from = Deforestation_Area) %>%
  mutate_all(~replace(., is.na(.), 0)) %>%                                      # fills the df with zeroes where there are empty values
  ungroup()

# VISUALIZATIONS ###############################################################