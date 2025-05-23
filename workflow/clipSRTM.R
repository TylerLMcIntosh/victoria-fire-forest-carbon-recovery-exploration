#Clip SRTM to size that Map Pilot 2 will accept


#Check the required libraries and download if needed
list.of.packages <- c("here","tidyverse", "terra", "sf")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(sf)
library(here)
library(tidyverse)
library(terra)


#Function to clip a raster to a vector, ensuring in same projection
#Returns raster in original projection, but clipped and masked to vector
careful.clip <- function(raster, vector) {
  if (sf::st_crs(vector) != terra::crs(raster)) { #if raster and vector aren't in same projection, change vector to match
    print("Projecting vector")
    vector <- sf::st_transform(vector, terra::crs(raster)) 
  } else {
    print("Vector already in raster CRS")
  }
  print("Clipping")
  r <- terra::crop(raster,
                   vector,
                   mask = TRUE) #crop & mask
  return(r)
}


#Function to clip a raster to a set of polygons, ensuring they are in the same projection
#Polygons should have a 'name' field
#Returns the set of rasters as a list 
careful.clip.set <- function(raster, vectors) {
  out <- list()
  nms <- c()
  for(i in 1:nrow(vectors)) {
    vec <- vectors[i,]
    clipped <- careful.clip(raster, vec)
    out[[vec$name]] <- clipped
  }
  return(out)
}


#Function to export the rasters from careful.clip.set
#Input should be the list returned from careful.clip.set (i.e. a named list of rasters)
#Outdir should be the directory for outputs
#SetNm is the name of the raster set to output (e.g. "SRTM")
export.careful.clip.set <- function(input, outDir, setNm) {
  for (i in 1:length(input)) {
    outFlNm <- paste(setNm, "_", names(input)[i], ".tif", sep="")
    terra::writeRaster(rast(input[i]),
                       here::here(outDir, outFlNm),
                       overwrite=TRUE)
  }
}


#Load data
polyAreas <- sf::st_read(here::here("data/srtm_subsections.shp"))
srtm <- terra::rast(here::here("data/merged_colorado_SRTM_nayani.tif"))

#Use functions
careful.clip.set(srtm, polyAreas) %>%
  export.careful.clip.set(outDir = here::here('data'),
                          setNm = "SRTM")

