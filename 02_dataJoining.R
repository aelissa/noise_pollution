library(tidyverse)
library(sf)

#########################################################
###########JOIN COMMUTING WITH HEX GRID##################
#########################################################

#####London#####

all_modes_commuters_L <- readRDS("Commuting/London/travel_data_commuters_l.rds")
london <- st_read("Aggregated_Data/London_data.gpkg")
london <- london %>%
  mutate(
    id = as.character(id)
  )

all_modes_commuters_functions_L <- all_modes_commuters_L %>%
  inner_join(london, by = "id")

saveRDS(all_modes_commuters_functions_L, "Modelling_input/modelling_london.rds")

#####Brisbane#####
#load sa2 and dzn for Brisbane Urban Area
hex <- st_read("Aggregated_Data/Brisbane_data_admin.gpkg")

commuting_bike <- readRDS("Commuting/Brisbane/output/BICYCLE.rds") %>%
  st_transform(28356) %>%
  filter(from_id %in% hex$SA2_NAME_2016) %>%
  filter(to_id %in% hex$DZN_CODE_2016)

commuting_bike_j <- st_join(commuting_bike, hex)

saveRDS(commuting_bike_j, "Commuting/Brisbane/output/BICYCLE_J.rds")

commuting_car <- readRDS("Commuting/Brisbane/output/CAR.rds") %>%
  st_transform(28356) %>%
  filter(from_id %in% hex$SA2_NAME_2016) %>%
  filter(to_id %in% hex$DZN_CODE_2016)

commuting_car_j <- st_join(commuting_car, hex) 

saveRDS(commuting_car_j, "Commuting/Brisbane/output/CAR_J.rds")

commuting_walk <- readRDS("Commuting/Brisbane/output/WALK.rds") %>%
  st_transform(28356) %>%
  filter(from_id %in% hex$SA2_NAME_2016) %>%
  filter(to_id %in% hex$DZN_CODE_2016)

commuting_walk_j <- st_join(commuting_walk, hex) 

saveRDS(commuting_walk_j, "Commuting/Brisbane/output/WALK_J.rds")

remove(commuting_bike, commuting_car, commuting_walk)

### add number of commuters

OD_mode_Queensland <- read_csv("Commuting/Brisbane/input/OD_mode_Queensland.csv") %>%
  select(`DZN (POW)`, `SA2 (UR)`, Bicycle, `Walked only`, `Car, as driver`) %>%
  rename(
    BICYCLE = Bicycle,
    WALK = `Walked only`,
    CAR = `Car, as driver`
  ) %>%
  pivot_longer(cols = where(is.numeric), names_to = "mode", values_to = "commuters")

commuting_walk_j$mode <- "WALK"
commuting_car_j$mode <- "CAR"
commuting_bike_j$mode <- "BICYCLE"

all_modes <- rbind(commuting_bike_j, commuting_car_j, commuting_walk_j)

all_modes_commuters <- all_modes %>%
  inner_join(OD_mode_Queensland, by = c("from_id" = "SA2 (UR)",
                                        "to_id" = "DZN (POW)",
                                        "mode" = "mode")) %>%
  filter(commuters != 0) %>%
  select(from_id, 
         to_id, 
         total_duration, 
         total_distance,
         mode,
         id, 
         Street_length, 
         NoiseCAT, 
         EDU, 
         REC,
         MED,
         PUB,
         RET,
         OTH,
         ALLPOI,
         Bld_area,
         commuters) %>%
  mutate(
    NoiseCAT = case_when(NoiseCAT == "noise < 58dB" ~ 0,
                         NoiseCAT == "58dB < noise < 63dB" ~ 1,
                         NoiseCAT == "63dB < noise < 68dB" ~ 2,
                         NoiseCAT == "68dB < noise < 73dB" ~ 3,
                         NoiseCAT == "noise >= 73dB" ~ 4,
                         is.na(NoiseCAT) ~ 0)
  )

saveRDS(all_modes_commuters, "Modelling_input/modelling_brisbane.rds")

