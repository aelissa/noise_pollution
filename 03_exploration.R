########################################################
##################Preliminary Exploration ##############
########################################################
library(sf)
library(tidyverse)
library(corrplot)
library(ggplot2)


####helper
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

###Brisbane

modelling_b <- readRDS("Modelling_input/modelling_brisbane.rds") 

###London

modelling_l <- readRDS("Modelling_input/modelling_london.rds")

modelling_l <- modelling_l %>%
  mutate(
    NoiseCAT = case_when(NoiseCAT == "55.0-59.9" ~ 1,
                         NoiseCAT == "60.0-64.9" ~ 2,
                         NoiseCAT == "65.0-69.9" ~ 3,
                         NoiseCAT == "70.0-74.9" ~ 4,
                         NoiseCAT == ">=75.0" ~ 5,
                         is.na(NoiseCAT) ~ 0)
  ) %>%
  select(-Noise_Road) %>%
  select(-bld_area.x) %>%
  rename(Bld_area = bld_area.y)


##### Spatial Grid

hex_l <- st_read("Aggregated_Data/London_data.gpkg") %>%
  mutate(
    id = as.character(id),
    NoiseCAT = case_when(NoiseCAT == "55.0-59.9" ~ 1,
                         NoiseCAT == "60.0-64.9" ~ 2,
                         NoiseCAT == "65.0-69.9" ~ 3,
                         NoiseCAT == "70.0-74.9" ~ 4,
                         NoiseCAT == ">=75.0" ~ 5,
                         is.na(NoiseCAT) ~ 0)
  ) %>%
  rename(Bld_area = bld_area)
  
hex_b <- st_read("Aggregated_Data/Brisbane_data.gpkg") %>%
  mutate(
    id = as.character(id),
    NoiseCAT = case_when(NoiseCAT == "noise < 58dB" ~ 0,
                         NoiseCAT == "58dB < noise < 63dB" ~ 2,
                         NoiseCAT == "63dB < noise < 68dB" ~ 3,
                         NoiseCAT == "68dB < noise < 73dB" ~ 4,
                         NoiseCAT == "noise >= 73dB" ~ 5,
                         is.na(NoiseCAT) ~ 0)
  )


### Area based exploration

##VISUALISATION 1 - mapping commuters by mode in the two cities


commuters_l <- modelling_l %>%
  st_drop_geometry() %>%
  group_by(id, mode) %>%
  summarise(
    commuters_count = sum(commuters)
  ) %>%
  pivot_wider(names_from = "mode", values_from = "commuters_count") %>%
  inner_join(hex_l, by = "id") 

commuters_b <- modelling_b %>%
  st_drop_geometry() %>%
  group_by(id, mode) %>%
  summarise(
    commuters_count = sum(commuters)
  ) %>%
  mutate(
    id = as.character(id)
  ) %>%
  pivot_wider(names_from = "mode", values_from = "commuters_count") %>%
  inner_join(hex_b, by = "id")

st_write(commuters_b, "Mapping_files/commuters_b.gpkg")
st_write(commuters_l, "Mapping_files/commuters_l.gpkg")

### VISUALISATION 2 - Corrplot

corr_l <- commuters_l %>%
  ungroup() %>%
  select(-id) %>%
  st_drop_geometry() %>%
  select(-geom) %>%
  replace(is.na(.), 0)
  
M_l <- cor(corr_l)
corrplot(M_l, method="circle")  

###add p value
# matrix of the p-value of the correlation
p.mat_l <- cor.mtest(corr_l)

png(filename = "img/corrplot_l.png", width = 30, height = 30, units = "cm", res = 600)

corrplot(M_l, method = "number", type="upper", p.mat = p.mat_l, sig.level = 0.001)

dev.off()

###Brisbane

corr_b <- commuters_b %>%
  ungroup() %>%
  select(-id) %>%
  st_drop_geometry() %>%
  select(-geom) %>%
  replace(is.na(.), 0)

M_b <- cor(corr_b)  
p.mat_b <- cor.mtest(corr_b)

png(filename = "img/corrplot_b.png", width = 30, height = 30, units = "cm", res = 600)

corrplot(M_b, method = "number", type="upper", p.mat = p.mat_b, sig.level = 0.001)

dev.off()

### Trip based exploration

#### London

hex_l_tot <- modelling_l %>%
  st_drop_geometry() %>%
  group_by(fromId, toId, distance, mode) %>%
  summarise(
    tot_hex = n()
  )

prep_l <- modelling_l %>%
  st_drop_geometry() %>%
  group_by(commuters, fromId, toId, distance, mode, NoiseCAT) %>%
  summarise(
    hex_count = n(),
    across(Street_length:Bld_area, ~ mean(.x, na.rm = T))
  ) %>%
  inner_join(hex_l_tot, by = c("fromId" = "fromId", "toId" = "toId", "distance" = "distance", "mode" = "mode")) %>%
  mutate(
    noise_dist = (distance/tot_hex)*hex_count #this gets the number of meters travelled for each noise class in a trip
  ) %>%
  pivot_wider(names_from = "NoiseCAT", values_from = "noise_dist") %>%
  rename(
    noise_cat_0 = `0`,
    noise_cat_1 = `1`,
    noise_cat_2 = `2`,
    noise_cat_3 = `3`,
    noise_cat_4 = `4`,
    noise_cat_5 = `5`
  ) %>%
  group_by(commuters, fromId, toId, distance, mode, tot_hex) %>%
  summarise(
    across(Street_length:noise_cat_5, ~ mean(.x, na.rm = T))
  ) %>%
  replace(is.na(.),0) %>%
  mutate(
    noise_cat_0 = noise_cat_0/distance,
    noise_cat_1 = noise_cat_1/distance,
    noise_cat_2 = noise_cat_2/distance,
    noise_cat_3 = noise_cat_3/distance,
    noise_cat_4 = noise_cat_4/distance,
    noise_cat_5 = noise_cat_5/distance
  )

  
saveRDS(prep_l, "Modelling_input/pre-processed-l.RDS")

#### Brisbane

hex_b_tot <- modelling_b %>%
  st_drop_geometry() %>%
  rename(
    distance = total_distance,
    fromId = from_id,
    toId = to_id
  ) %>%
  group_by(fromId, toId, distance, mode) %>%
  summarise(
    tot_hex = n()
  )

prep_b <- modelling_b %>%
  st_drop_geometry() %>%
  rename(
    distance = total_distance,
    fromId = from_id,
    toId = to_id
  ) %>%
  group_by(commuters, fromId, toId, distance, mode, NoiseCAT) %>%
  summarise(
    hex_count = n(),
    across(Street_length:Bld_area, ~ mean(.x, na.rm = T))
  ) %>%
  inner_join(hex_b_tot, by = c("fromId" = "fromId", "toId" = "toId", "distance" = "distance", "mode" = "mode")) %>%
  mutate(
    noise_dist = (distance/tot_hex)*hex_count #this gets the number of meters travelled for each noise class in a trip
  ) %>%
  pivot_wider(names_from = "NoiseCAT", values_from = "noise_dist") %>%
  rename(
    noise_cat_0 = `0`,
    noise_cat_1 = `1`,
    noise_cat_2 = `2`,
    noise_cat_3 = `3`,
    noise_cat_4 = `4`
  ) %>%
  group_by(commuters, fromId, toId, distance, mode, tot_hex) %>%
  summarise(
   across(Street_length:noise_cat_2, ~ mean(.x, na.rm = T))
  ) %>%
  replace(is.na(.),0) %>%
  mutate(
    noise_cat_0 = noise_cat_0/distance,
    noise_cat_1 = noise_cat_1/distance,
    noise_cat_2 = noise_cat_2/distance,
    noise_cat_3 = noise_cat_3/distance,
    noise_cat_4 = noise_cat_4/distance
    )


saveRDS(prep_b, "Modelling_input/pre-processed-b.RDS")

#####

trip_b <- readRDS("Modelling_input/pre-processed-b.RDS")
trip_l <- readRDS("Modelling_input/pre-processed-l.RDS")

trip_b <- trip_b %>%
  pivot_wider(names_from = "mode", values_from = "commuters") %>%
  replace(is.na(.),0) %>%
  rename(
    Distance = distance,
    `Street length` = Street_length,
    `Bld area` = Bld_area,
    Education = EDU,
    Recreation = REC,
    Health = MED,
    Public = PUB,
    Retail = RET,
    Other = OTH,
    `All POI` = ALLPOI,
    `noise < 58dB` = noise_cat_0,
    `58dB <= noise < 63dB` = noise_cat_1,
    `63dB <= noise < 68dB` = noise_cat_2,
    `68dB <= noise < 73dB` = noise_cat_3,
    `noise >= 73dB` = noise_cat_4,
    Walk = WALK,
    Bicycle = BICYCLE,
    Car = CAR
  ) 
    
M_trip_b <- cor(trip_b[c(3, 5:21)])
p.mat_trip_b <- cor.mtest(trip_b[c(3, 5:21)])$p

png(filename = "img/corrplot_trips_b.png", width = 35, height = 35, units = "cm", res = 600)

corrplot(M_trip_b, method = "number", type="upper", p.mat = p.mat_trip_b, sig.level = 0.001)

dev.off()

trip_l <- trip_l %>%
  group_by(across(c(-commuters))) %>%
  summarise(
    commuters = sum(commuters)
  ) %>%
  pivot_wider(names_from = "mode", values_from = "commuters") %>%
  replace(is.na(.),0) %>%
  rename(
    Distance = distance,
    `Street length` = Street_length,
    `Bld area` = Bld_area,
    Education = EDU,
    Recreation = REC,
    Health = MED,
    Public = PUB,
    Retail = RET,
    Other = OTH,
    `All POI` = ALLPOI,
    `noise < 55dB` = noise_cat_0,
    `55dB <= noise < 60dB` = noise_cat_1,
    `60dB <= noise < 65dB` = noise_cat_2,
    `65dB <= noise < 70dB` = noise_cat_3,
    `70dB <= noise < 75dB` = noise_cat_4,
    `noise >= 75dB` = noise_cat_5,
    Walk = WALK,
    Bicycle = BICYCLE,
    Car = CAR
  ) 


M_trip_l <- cor(trip_l[c(3,5:22)])
p.mat_trip_l <- cor.mtest(trip_l[c(3,5:22)])$p

png(filename = "img/corrplot_trips_l.png", width = 35, height = 35, units = "cm", res = 600)

corrplot(M_trip_l, method = "number", type="upper", p.mat = p.mat_trip_l, sig.level = 0.001)

dev.off()


