# Script to check the validity of polygon geometries and prepare them
# as valid shapefiles.
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

library(sf)
library(tibble) 
library(lwgeom)

# set working directory
setwd("~/github/fire-forest-carbon-recovery/")

# read the SMA polygons for Colorado 
sma_co <- sf::st_read("data/BLM_CO_SMA_SHP/BLM_CO_SMA_20190520.shp")

# take a look at the attributes and number of observations
tibble::glimpse(sma_co)

# check the validity of the geometries and query the reason
sma_co_is_valid <- sf::st_is_valid(sma_co, reason = TRUE)

# print all reasons of invalidity 
print(sma_co_is_valid[sma_co_is_valid != "Valid Geometry"])
# 343 invalid reasons: "Ring Self-intersection" 

# make invalid polygons valid, or remove them? 
sma_co_valid <- lwgeom::st_make_valid(sma_co)
