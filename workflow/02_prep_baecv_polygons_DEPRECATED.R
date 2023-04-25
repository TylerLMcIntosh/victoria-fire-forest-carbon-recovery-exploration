# Prep BAECV polygons

#Tyler McIntosh
#3/9/23

#Takes in BAECV polygons and adds information to them for site selection

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

######Libraries
library(terra)
library(sf)
library(here)
library(tidyverse)
library(mapview)

#####Clean workspace
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)

#Set CRS
projectCRS <- 'EPSG:26913' #Southern rockies NAD83 13N


######## Load data
baecv <- sf::st_read("data/derived/baecv_poly_1999_to_2002.shp") %>% st_transform(projectCRS)
gapCover <- terra::rast("data/GapConusCover2011_SouthernRockies_EPSG_32613_GEE.tif")
gapCodes <- read.csv("data/gap_conus_2011_codes.csv")
gapCodesLookup <- read.csv("data/gap_conus_lookup_codes.csv")
usfs <- sf::st_read("data/SMA_USFS/SMA_USFS_National.shp") %>% st_transform(projectCRS)
roads <- sf::st_read("data/CO_tl_2022_08_prisecroads/tl_2022_08_prisecroads.shp") %>% st_transform(projectCRS)

###### Calculate dats of interest

#filter to only those polygons that overlap w/ USFS
baecvUSFS <- baecv %>% st_filter(usfs) 
mapview(usfs, col.regions = "green") + 
  mapview(baecv, col.regions = "yellow") + 
  mapview(baecvUSFS, col.regions = "red")


#Calculate area within USFS land for a polygon
usfs.area <- function(polygon) {
  intersection <- polygon %>% st_intersection(usfs) #intersect
  if(nrow(intersection) == 0) {
    #    polygon$area_meters <- 0
    return(0)
  } else {
    intersection <- intersection %>% dplyr::mutate(area_meters = round(st_area(intersection), digits=2)) #calculate area
    #    polygon$usfs_area <- intersection$area_meters
    return(intersection$area_meters)
  }
}

#Get list of polygon areas in USFS land and bind back to dataset
usfsAreas <- list()
for(i in seq_along(baecvUSFS[[1]])) {
  usfsAreas[[i]] <- usfs.area(baecvUSFS[i,])
} 
usfs_area_mtrs <- unlist(usfsAreas)
baecvUSFS <- baecvUSFS %>% cbind(usfs_area_mtrs)
baecvUSFS <- baecvUSFS %>% mutate(usfs_perc = round(100 * (usfs_area_mtrs / ar_mtrs)), 2)

mapview(baecvUSFS, zcol = "usfs_perc")
mapview(baecvUSFS, zcol = "burnYer")





