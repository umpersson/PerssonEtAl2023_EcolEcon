# LOAD & PREPARE DATA ##########################################################

Def_attrib <- read.csv("Deforestation_attribution_sheet.csv", sep = ",", stringsAsFactors = FALSE) # Loads data on deforestation attribution to commodities from Pendrill et al. (2022)
Kastner_trade <- read.csv("Kastner_sheet.csv", sep = ",", stringsAsFactors = FALSE)                # Loads data on bilateral trade flows of embodied deforestation from Pendrill et al. (2022)
LU_area_data <- read.csv("FAO_area_data.csv", sep = ",", stringsAsFactors = FALSE)                 # Loads data on pasture extent and area harvested by commodity, for year 2018, from FAOSTAT (2021)
Crop_groups <- read.csv("Crop_lookup.csv", sep = ",", stringsAsFactors = FALSE)                    # Loads data key to summarize individual crop data into aggregate crop groups
Country_groups <- read.csv("Country_lookup.csv", sep = ",", stringsAsFactors = FALSE)              # Loads data key to summarize individual country data into aggregate regions

# Create a dataframe with the share of 2018 harvested area 
Area_shares <- select(Def_attrib, ISO, Commodity, Year, Def_attr_ha) %>%
  filter(Commodity != "Forest plantation") %>%
  group_by(ISO, Commodity) %>%
  mutate (CumDef = rev(cumsum(rev(Def_attr_ha)))) %>% # Calculate the cumulative sum of deforestation in reveres order - i.e., the amount of land use for a given commodity deforested before a given year
  ungroup() %>%
  left_join(., select(LU_area_data, ISO, Item, Value), by = c("ISO" = "ISO", "Commodity" = "Item")) %>%
  left_join(., select(Crop_groups, FAO_name, CropGroup), by = c("Commodity" = "FAO_name")) %>%
  select(ISO, CropGroup, Year, CumDef, Value) %>%
  group_by(ISO, CropGroup, Year) %>%
  summarise_all(sum, na.rm = TRUE) %>%
  mutate(DefShare = CumDef / Value)

# Create a dataframe with domestic & export volumes, export shares, by country, commodity group & year
Dom_exp_shares <- select(Kastner_trade, ProducerCountry, ConsumerCountry, FAO_name, Year, Deforestation_Area) %>%
  mutate(ConsumerCountry = ifelse(ProducerCountry == ConsumerCountry, "Domestic", "Export")) %>%
  left_join(., select(Crop_groups, FAO_name, CropGroup), by = "FAO_name") %>%
  group_by(ProducerCountry, CropGroup, ConsumerCountry, Year) %>%
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

