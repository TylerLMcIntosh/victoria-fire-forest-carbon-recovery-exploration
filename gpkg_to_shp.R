# convert .gpkg to .shp

library(sf)
library(here)

# read .gpkg file with geometry
gpkg <- sf::st_read("~/Downloads/modis_event_polygons_cus.gpkg")  #FIRED data

# write to .shp
sf::st_write(sf::st_write(obj = gpkg, dsn = here::here("data/data_output/FIRED.shp")))


# read the shapefile to make sure it was written correctly
test <- sf::st_read("data/data_output/FIRED.shp")
