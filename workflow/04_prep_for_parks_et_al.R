#Take in a dataset of fire perimeter polygons that has an attribute for 'year'
#split it into annual multi-polygon shapefiles to use w/ Parks et al CBI script


#Check the required libraries and download if needed
list.of.packages <- c("sf","here","mapview","tidyverse", "zip")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(sf)
library(here)
library(mapview)
library(tidyverse)
library(zip)

dats <- st_read(here("data", "derived", "filtered", "ponderosa_usfs_private_400_allow_reburns.gpkg"))
mapview(dats)

datsBuff <- dats %>% st_buffer(500) #buffer by 500m to ensure that we are capturing the full extent of the fire and have unburned areas in LBA data fires

sep_year <- datsBuff %>%
  group_by(year) %>%
  summarize(geom = st_union(geom))
mapview(sep_year)



# // From Parks et al 2018 on fire seasons: Within GEE, mean pre- and post-fire NBR values (Equation (1)) 
# //across a pre-specified date range (termed a ‘mean composite’) were calculated per pixel across the stack 
# //of valid pixels (e.g., cloud- and snow-free pixels). For fires that occurred in Arizona, New Mexico, and 
# //Utah, the date range is April through June; for all other fires, the date range is June through September (Figure 1). 
# 
# //From Mike Koontz on fire seasons: Parks et al. 2018 has a description of which states have which image seasons. 
# //Will have to fill in Nevada (same as CA or AZ/NM?) & Colorado (same as WY)

prepped_for_parks_etal <- sep_year %>% mutate(NAME = paste("Landsat burned area fires from ", year, sep = ""),
                                              Fire_ID = paste(year, "fires", sep = "_"),
                                              Fire_Year = as.integer(year),
                                              Start_Day = as.integer(format(as.Date("2023/06/1"), "%j")), #June
                                              End_Day = as.integer(format(as.Date("2023/09/30"), "%j"))) #Through September


#Function to write a shapefile to a new, file-specific directory and add a zipped version
#    shp = the sf file to write to shapefile
#    location = path of directory to create the new file-specific subdirectory in
#    filename = name of file WITHOUT .shp
#    zipOnly = TRUE / FALSE, should the original (unzipped) files be removed?

# Example use:
# st_write_shp(shp = prepped_for_parks_etal,
#              location = here("data/derived"),
#              filename = "career_lba_for_parks_v1",
#              zipOnly = TRUE)
st_write_shp <- function(shp, location, filename, zipOnly) {
  
  #Check for required packages and install if not installed, then load
  list.of.packages <- c("zip","sf","here")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(zip)
  library(sf)
  library(here)
  
  
  #Create subdirectory
  outDir <- here::here(location, filename)
  if (!dir.exists(outDir)){
    dir.create(outDir)
  }
  
  
  
  #Write file
  sf::st_write(shp,
               here::here(outDir, paste(filename, ".shp", sep = "")),
               append = FALSE) #overwrite
  
  #Get all shapefile components
  allShpNms <- list.files(here::here(outDir),
                          pattern = paste(filename, "*", sep = ""),
                          full.names = TRUE)
  
  #Zip together
  zip::zip(zipfile = here(outDir, paste(filename, ".zip", sep="")),
           files = allShpNms,
           mode = "cherry-pick")
  
  
  #Remove raw files if desired
  if(zipOnly == TRUE) {
    file.copy(here(outDir, paste(filename, ".zip", sep="")), here(location, paste(filename, ".zip", sep="")))
    unlink(here(outDir), recursive = TRUE)          
  }
  
}

st_write_shp(shp = prepped_for_parks_etal,
             location = here("data/derived"),
             filename = "career_pond_all_400_include_reburns_for_parks.shp",
             zipOnly = FALSE)

