# Derive fire boundaries from BAECV data for a given aoi

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

#Untar datasets to data if not already done
#untar("../../data_raw/BAECV/BAECV_1999_v1.1_20170908.tar.gz", exdir = "data/baecv1999v1.1")
#untar("../../data_raw/BAECV/BAECV_2000_v1.1_20170908.tar.gz", exdir = "data/baecv2000v1.1")
#untar("../../data_raw/BAECV/BAECV_2001_v1.1_20170908.tar.gz", exdir = "data/baecv2001v1.1")
#untar("../../data_raw/BAECV/BAECV_2002_v1.1_20170908.tar.gz", exdir = "data/baecv2002v1.1")

# Read in BAECV
burnClass1999 <- terra::rast("data/baecv1999v1.1/BAECV_bc_1999_v1.1_20170908.tif")
burnClass2000 <- terra::rast("data/baecv2000v1.1/BAECV_bc_2000_v1.1_20170908.tif")
burnClass2001 <- terra::rast("data/baecv2001v1.1/BAECV_bc_2001_v1.1_20170908.tif")
burnClass2002 <- terra::rast("data/baecv2002v1.1/BAECV_bc_2002_v1.1_20170908.tif")
plot(burnClass2000)

#set temp crs
tempcrs <- terra::crs(burnClass2000)


#Buffer area around AOP
aoi <- niwotAOP %>% sf::st_buffer(200000) #200km buffer
plot(aoi)

#Function to clip and mask
clipmask.raster.to.vector <- function(raster, vector, setCRS) {
  if (crs(vector) != setCRS) {
    print("Projecting vector")
    vector <- st_transform(vector, setCRS) 
    print("Vector projected")
  } else {
    print("Vector already in chosen CRS")
  }
  if (crs(raster) != setCRS) {
    print("Projecting raster")
    raster <- project(raster, setCRS)
    print("Raster projected")
  } else {
    print("Raster already in chosen CRS")
  }
  if(ext(raster) != ext(vector)) {
    print("Cropping raster to vector")
    raster <- crop(raster, ext(vector))
    print("Cropped")
  } else {
    print("Extents already match")
  }
  print("Masking raster to vector")
  rMask <- mask(raster, vector)
  print("Masked")
  print("Done")
  return(rMask)
}

#Run function on data using CRS of the large raster
baecv1999clip <- clipmask.raster.to.vector(burnClass1999, aoi, tempcrs)
baecv2000clip <- clipmask.raster.to.vector(burnClass2000, aoi, tempcrs)
baecv2001clip <- clipmask.raster.to.vector(burnClass2001, aoi, tempcrs)
baecv2002clip <- clipmask.raster.to.vector(burnClass2002, aoi, tempcrs)

#Function to vectorize, clean, add year
vectorize.clean <- function(raster, year) {
  
  #Vectorize before reprojecting to save compute time
  rasterPoly <- terra::as.polygons(raster, trunc=TRUE, dissolve=TRUE, values=TRUE, extent=FALSE)
  
  unique(rasterPoly$Band_1) #We have values of 0 and 1 in our polygon set
  #Convert back to sf & filter polygons
  rasterPoly <- rasterPoly %>% 
    sf::st_as_sf() %>% 
    filter(Band_1 == 1)
  
  #Add burn year and cast to polygon instead of multi-polygon
  rasterPoly <- rasterPoly %>%
    mutate(burnYear = year) %>%
    st_cast("POLYGON")
  
  #Add unique polygon identifier and fix attribute names
  nms <- paste("baecv", as.character(year), as.character(seq(1:nrow(rasterPoly))), sep="_")
  rasterPoly <- rasterPoly %>%
    cbind(nms) %>%
    rename(fireID = nms, burned = Band_1)
  
  return(rasterPoly)
}

#Run on all 3 years
poly1999 <- vectorize.clean(baecv1999clip, 1999)
poly2000 <- vectorize.clean(baecv2000clip, 2000)
poly2001 <- vectorize.clean(baecv2001clip, 2001)
poly2002 <- vectorize.clean(baecv2002clip, 2002)
mapview(poly1999)

#Combine shapefiles & label as baecv
baecvPoly <- rbind(poly1999, poly2000, poly2001, poly2002) %>%
  mutate(dataset = "BAECV")
mapview(baecvPoly)

#Reproject to project CRS
baecvPoly <- st_transform(baecvPoly, projectCRS) 

#Calculate areas
baecvPoly <- baecvPoly %>% dplyr::mutate(area_meters = st_area(baecvPoly),
                                             # 1 m2 = 0.0.000247105 acres
                                             area_acres = area_meters * 0.000247105,
                                             # 1m2 = 0.0001 hectares
                                             area_hectares = area_meters * 0.0001)
#Round to avoid creating too large of data
baecvPoly <- baecvPoly %>% dplyr::mutate(area_meters = round(area_meters, digits = 2),
                                         area_acres = round(area_acres, digits=2),
                                         area_hectares = round(area_hectares, digits=2))

#Write to shapefile
st_write(baecvPoly, "data/derived/baecv_poly_1999_to_2002.shp", append = FALSE)
