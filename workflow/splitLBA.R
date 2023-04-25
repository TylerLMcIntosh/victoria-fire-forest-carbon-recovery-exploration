#Take data from 01_prep_process_LBA.R and
#split it into annual multi-polygon shapefiles to use w/ Parks et al CBI script

library(sf)
library(here)
library(mapview)
library(tidyverse)
library(zip)

dats <- st_read(here("data", "derived", "lba_data_final_all_dats_filtered.gpkg"))
mapview(dats)

sep_year <- dats %>%
  group_by(year) %>%
  summarize(geom = st_union(geom))
mapview(sep_year)

#Julian date calendar: https://people.biology.ucsd.edu/patrick/julian_cal.html
prepped_for_parks_etal <- sep_year %>% mutate(NAME = paste("Landsat burned area fires from ", year, sep = ""),
                                              Fire_ID = paste(year, "fires", sep = "_"),
                                              Fire_Year = as.integer(year),
                                              Start_Day = 152,
                                              End_Day = 258)

st_write(prepped_for_parks_etal, here("data", "derived", "career_lba_for_parks.shp"))


#Function to write a shapefile to a new, file-specific directory nad add a zipped version
          #    shp = the sf file to write to shapefile
          #    location = path of directory to create the new file-specific subdirectory in
          #    filename = name of file WITHOUT .shp

# Example use:
          # st_write_shp(shp = prepped_for_parks_etal,
          #              location = here("data/derived"),
          #              filename = "career_lba_for_parks_v1")
st_write_shp <- function(shp, location, filename) {
  
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
}

st_write_shp(shp = prepped_for_parks_etal,
             location = here("data/derived"),
             filename = "career_lba_for_parks_v1")

