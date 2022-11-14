# LOAD & PREPARE FAOSTAT AREA DATA #############################################

FAO_land_use <- read.csv("C:/Users/mpersson/Box/Land-balance & deforestation emissions model/Data/Global/Inputs_LandUse_E_All_Data_(Normalized).csv", sep = ",", stringsAsFactors = FALSE)                    # FAOSTAT Land Use data, downlaoded from http://www.fao.org/faostat/en/#data/RL (retrieved 20211007; data update 20210719)
FAO_crops_livestock <- read.csv("C:/Users/mpersson/Box/Land-balance & deforestation emissions model/Data/Global/Production_Crops_Livestock_E_All_Data_(Normalized).csv", sep = ",", stringsAsFactors = FALSE) # FAOSTAT livestock primary data, downlaoded from http://www.fao.org/faostat/en/#data/QCL (retrieved 20211007; data update 20210915)

FAO_area_data <- rbind(
                    filter(
                       select(FAO_land_use, Area.Code, Area, Item.Code, Item, Element, Year, Value),
                       Year == 2018 & Item == "Land under perm. meadows and pastures"
                       ),
                    filter(
                       select(FAO_crops_livestock, Area.Code, Area, Item.Code, Item, Element, Year, Value),
                       Year == 2018 & Element == "Area harvested"
                    ) 
                  ) %>%
                  mutate(Value = ifelse(Item == "Land under perm. meadows and pastures", Value * 1000, Value)) # Converting pasture area to ha, as it is given in 1,000 ha in the FAOSTAT land use database

write.csv(FAO_area_data, "FAO_area_data.csv")
