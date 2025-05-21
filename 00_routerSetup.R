options(java.parameters = '-Xmx220G')

library(elevatr)
library(sf)
library(r5r)
library(raster)

###Brisbane Router
brisbane_bnd<-st_read("Auxiliary/brisbane_bnd.gpkg")

##Crop and filter osm data with osmosis. 
brisbane_bb<-st_bbox(brisbane_bnd)

#On terminal run:
#osmium extract --strategy complete_ways --bbox 152.07340,-28.36387,153.54671,-26.45233 australia-latest.osm.pbf -o brisbane.osm.pbf
#rm australia-latest.osm.pbf 

elevation <- get_elev_raster(brisbane_bnd,z = 9)
writeRaster(elevation,'Routing/B_router/elevation.tif',options=c('TFW=YES'))

path <- "/mnt/datastore/ActiveTravel_Noise/Routing/B_router/"
r5r_core <- setup_r5(data_path = path, verbose = FALSE)

###London Router
london_bnd<-st_read("Auxiliary/london_bnd.gpkg")

##Crop and filter osm data with osmosis. 
london_bb<-st_bbox(london_bnd)

#On terminal run:
#osmium extract --strategy complete_ways --bbox london_bb UK-latest.osm.pbf -o london.osm.pbf
#rm uk-latest.osm.pbf 

elevation <- get_elev_raster(london_bnd,z = 9)
writeRaster(elevation,'Routing/L_router/elevation.tif',options=c('TFW=YES'))

path <- "/mnt/datastore/ActiveTravel_Noise/Routing/L_router/"
r5r_core <- setup_r5(data_path = path, verbose = FALSE)
