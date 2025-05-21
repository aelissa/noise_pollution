options(java.parameters = '-Xmx150G')

library(sf)
library(r5r)
library(tidyverse)
library(readr)

####Routing Brisbane#####

B_OD<-read_csv("Commuting/Brisbane/input/OD_mode_Queensland.csv")
B_O<-st_read("Commuting/Brisbane/input/GB_SA2_2016.gpkg") %>% st_transform(4326)
B_D<-st_read("Commuting/Brisbane/input/DZN_2016.gpkg") %>% st_transform(4326)

B_OD<-B_OD %>%
  mutate(
    CAR = `Car, as driver`+ `Car, as passenger`
  ) %>%
  rename(
    SA2_name = `SA2 (UR)`,
    DZN = `DZN (POW)`,
    WALK = `Walked only`,
    BICYCLE = Bicycle
  ) %>%
  select(
    SA2_name, DZN, WALK, BICYCLE, CAR
  ) %>%
  filter(SA2_name %in% B_O$SA2_NAME_2016 & DZN %in% B_D$DZN_CODE_2016) %>% #take only trips in greater brisbane
  pivot_longer(cols = WALK:CAR, names_to = "mode", values_to = "n") %>%
  filter(n!=0)

origins <- B_O[c("SA2_NAME_2016")] %>%
  st_centroid() 
origins <- origins %>%
  mutate(
    lon = st_coordinates(origins)[,1],
    lat = st_coordinates(origins)[,2]
  ) %>%
  st_drop_geometry() 
origins <- B_OD %>%
  inner_join(origins, by = c("SA2_name" = "SA2_NAME_2016")) %>%
  rename(
    id = SA2_name
  )
  
destinations <- B_D[c("DZN_CODE_2016")] %>%
  st_centroid() 
destinations <- destinations %>%
  mutate(
    lon = st_coordinates(destinations)[,1],
    lat = st_coordinates(destinations)[,2]
  ) %>%
  st_drop_geometry() 
  
destinations <- B_OD %>%
  inner_join(destinations, by = c("DZN" = "DZN_CODE_2016")) %>%
  rename(
    id = DZN
  ) 

modes<-unique(B_OD$mode)

for (m in modes){
  
  o<-origins[origins$mode==m,]
  d<-destinations[destinations$mode==m,]

  ttm<-detailed_itineraries(r5r_core,
                          origins = o,
                          destinations = d,
                          mode=m,
                          drop_geometry = FALSE,
                          verbose=FALSE,
                          progress=TRUE)
  write_rds(ttm,paste0("Commuting/Brisbane/output/",m,".rds"))

}  

####Routing Greater London#####

msoa_centroids<- st_read("input/msoa_centroids.gpkg")
car <- read_csv("input/car.csv")
foot <- read_csv("input/foot.csv")
bicycle <- read_csv("input/bycicle.csv")

car <- car %>%
  tidyr::pivot_longer(cols = !msoa, names_to = "destination", values_to = "commuters") %>%
  filter(commuters!=0) %>%
  mutate(
    mode="CAR"
  )

foot <- foot %>%
  tidyr::pivot_longer(cols = !msoa, names_to = "destination", values_to = "commuters") %>%
  filter(commuters!=0) %>%
  mutate(
    mode="WALK"
  )

bicycle <- bicycle %>%
  tidyr::pivot_longer(cols = !msoa, names_to = "destination", values_to = "commuters") %>%
  filter(commuters!=0) %>%
  mutate(
    mode="BICYCLE"
  ) 

travel_to_work<-rbind(car,foot,bicycle)
remove(car,foot,bicycle)

####join geom

origins<-travel_to_work %>%
  inner_join(msoa_centroids[c("MSOA11CD","LONG","LAT")], by=c("msoa"="MSOA11CD")) %>%
  dplyr::rename(
                  id=msoa,
                  lon=LONG,
                  lat=LAT
                )

destinations<-travel_to_work %>%
  inner_join(msoa_centroids[c("MSOA11CD","LONG","LAT")], by=c("destination"="MSOA11CD")) %>%
  dplyr::rename(
    id=destination,
    lon=LONG,
    lat=LAT
  )

####run routing with geometry path

path <- "../L_router/"
r5r_core <- setup_r5(data_path = path, verbose = FALSE)

modes<-unique(travel_to_work$mode)


for (m in modes){
  
  o<-origins[origins$mode==m,]
  d<-destinations[destinations$mode==m,]
  
  ttm<-detailed_itineraries(r5r_core,
                            origins = o,
                            destinations = d,
                            mode=m,
                            drop_geometry = FALSE,
                            verbose=FALSE,
                            progress=TRUE)
  
  write_rds(ttm,paste0("output/route",m,".rds"))
}

