# convert .gpkg to .shp

library(sf)
library(here)

# check versions of sf packages
sf::sf_extSoftVersion()

# read .gpkg file with geometry
gpkg <- sf::st_read("~/Downloads/modis_event_polygons_cus.gpkg")  #FIRED data

# transform geometry coordinates from "MODIS sinu" to geog coords EPSG:4326 (lat, lon)
gpkg <- sf::st_transform(gpkg, 4326)

# write to .shp
sf::st_write(obj = gpkg, 
             dsn = here::here("data/data_output/FIRED.shp"))

# read the shapefile to make sure it was written correctly
test <- sf::st_read(here::here("data/data_output/FIRED.shp"))
