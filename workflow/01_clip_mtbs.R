#Clip MTBS to area of interest

#Takes in MTBS & 

#Tyler McIntosh
#3/9/23

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

#####Load data
niwotAOP <- st_read("data/NEON_AOP/NIWOT_AOP_Flightbox.shp")
mtbsPerims <- st_read("data/mtbs_perimeter_data/mtbs_perims_DD.shp")

#########Buffer, clip
#Buffer area around AOP
aoi <- niwotAOP %>% sf::st_buffer(200000) #200km buffer

#change crs & intersect
aoiMTBS <- mtbsPerims %>% 
  st_transform(projectCRS) %>%
  st_intersection(aoi)

mapview(aoiMTBS)

#Export
st_write(aoiMTBS, "data/derived/MTBS_Perims_200km_of_Niwot.shp")
