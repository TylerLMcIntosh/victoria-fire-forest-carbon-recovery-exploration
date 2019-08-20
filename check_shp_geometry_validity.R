# This script checks the validity of spatial geometries, fixes any invalid 
# entries, and writes a shapefile containing valid spatial geometries. 
#
# Need to upload BLM Surface Management Agency (SMA) polygons in shapefile
# format to the Google Earth Engine (GEE) project.
#
# Download the National SMA data here in .gdb format: 
# https://catalog.data.gov/dataset/blm-national-surface-management-agency-area-polygons-national-geospatial-data-asset-ngda 
#
# Download the Colorado SMA data here in .shp format: 
# https://www.blm.gov/site-page/services-geospatial-gis-data-colorado 
#
# ^ Uploading the .shp to GEE yields an "Asset Ingestion" error due to
# feature "null". 
#
# Tidying feature geometries: https://www.r-spatial.org/r/2017/03/19/invalid.html 

library(sf)
library(tibble) 
library(lwgeom)
library(here)


# define input filename 
geom_in_filename <- here::here("data/data_raw/BLM_National_Surface_Management_Agency/sma_wm.gdb") # CONUS BLM SMA polygons
#geom_in_filename <-  here::here("data/data_raw/BLM_CO_SMA_SHP/BLM_CO_SMA_20190520.shp") # Colorado BLM SMA polygons

# download the data directly from the interwebs 
#download.file(url = "https://gis.blm.gov/EGISDownload/LayerPackages/BLM_National_Surface_Management_Agency.zip"
#              , destfile = "blm_surface_management_agency_polygons_conus.zip")
#unzip("blm_surface_management_agency_polygons_conus.zip")
#geom_in_filename <- "BLM_National_Surface_Management_Agency/sma_wm.gdb/"

# define output filename where valid geometries will be written to .shp
#geom_out_filename <- here::here("data/data_output/blm_co_sma_valid.shp") 
geom_out_filename <- here::here("data/data_output/blm_conus_sma_valid.shp")

# read the input feature class geometries
geom_in <- sf::st_read(geom_in_filename)

# check the layer names within the gdb
#geom_layers <- sf::st_layers(dsn = geom_in_filename
#                         , do_count = TRUE)


# read in the first feature class of the gdb
#geom_in_layer1 <- sf::st_read(dsn = geom_in_filename
#                       ,layer = "Surface_Management_Agency")

# drop Z and/or M dimensions from feature geometries 
geom_drop_zm <- sf::st_zm(geom_in)

# take a look at the attributes and number of observations
tibble::glimpse(geom_drop_zm)

# check the validity of the geometries and query the reason
geom_is_valid <- sf::st_is_valid(geom_drop_zm, reason = TRUE)

# print all reasons of invalidity 
print(geom_is_valid[geom_is_valid != "Valid Geometry"])
# 343 invalid reasons: "Ring Self-intersection" in the CO BLM SMA data set.
# 570 invalid reasons: "Ring Self-intersection", "Self-intersection",
# or NA in the CONUS data set shp exported from ArcGIS. 

# make invalid polygons valid, or remove them? 
geom_valid <- lwgeom::st_make_valid(geom_drop_zm)

# write the valid geometries to .shp
sf::st_write(obj = geom_valid, dsn = geom_out_filename)

# zip shp files into single file 

# 
