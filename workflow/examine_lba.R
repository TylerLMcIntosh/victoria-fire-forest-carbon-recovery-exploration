### ### ### ### ### ### ### ### ### ### ### ###

#explore lba dataset
#Tyler L. McIntosh
#CU Boulder CIRES Earth Lab
#4/11/23

# Explanation of script

#Explain data inputs IN detail, and where analyses were done (if any). Include links where possible.
#List prior workflow script (if any)

#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ### ### ### ###

# SETUP ----
## Libraries ----

#Check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse", "beepr", "mapview")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load
library(tidyverse) #Tidyverse!
library(beepr) # beep() will make a noise. options(error = beep) will beep on any errors
library(mapview)
library(terra)
library(sf)
library(here)

## Clean workspace ----
rm(list=ls()) #Ensure empty workspace if running from the beginning
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
options(error = beep)

## Set relative directories structure ----

# Set output directory & create if doesn't already exist
outDir <- here("data", "derived")
if (!dir.exists(outDir)){
  dir.create(outDir)
}

# Also set data directories
sharedData <- here("data", "shared")
rawData <- here("data", "raw_GEE")

## Load data ----

#Get file names of severity data
firesRaw <- st_read("data/derived/lba_data_final_all_dats_filtered.gpkg")
nlcdForestmask <- terra::rast(here('data', 'external', 'modal_forest_type_nlcd_srockies.tif'))

### ### ### ### ### ### ### ### ### ### ### ###

# SCRIPTED ANALYSIS ----

fires <- firesRaw %>%
  mutate(gapConusForestTot = lodgepole + mixedConifer + ponderosa + spruceFir) %>%
  mutate(gapConusForestPerc = (gapConusForestTot / area_mtrs) * 100)



#Add NLCD modal forest mask (from linked disturbance project) ----
#41: deciduous
#42: evergreen
#43: mixed forest

#NLCD forest codes
forestCodes <- cbind(c(0, 41, 42, 43), c("nlcd_NonForest", "nlcd_Deciduous", "nlcd_Evergreen", "nlcd_MixedForest")) %>%
  `colnames<-`(c("forestCode", "forestType")) %>%
  as.data.frame()
forestCodes$forestCode <- as.integer(forestCodes$forestCode)

nlcdForestDats <- terra::extract(nlcdForestmask, fires, bind = FALSE) %>%
  group_by(ID, modal) %>%
  count() %>%
  ungroup() %>%
  mutate(n = n *900) %>% #turn to meters
  `colnames<-`(c("ID", "forestCode", "mtrs_sq")) %>%
  left_join(forestCodes) %>%
  spread(forestType, mtrs_sq) %>%
  replace(is.na(.), 0) %>%
  select(-forestCode) %>%
  group_by(ID) %>%
  summarise(across(starts_with("nlcd"), sum),
            .groups = 'drop') %>%
  mutate(nlcd_ForestAll = rowSums(across(nlcd_Deciduous:nlcd_MixedForest)))


#join the data to polygons
fires <- fires %>%
  rowid_to_column("rowID") %>%
  left_join(nlcdForestDats, by = c("rowID" = "ID")) %>%
  select(-rowID) %>%
  replace(is.na(.), 0)



#Add BPS classes ----
landfireDir <- here::here('data', 'landfire/')
bpsFile <- list.files(here::here(landfireDir, "biophysical-settings"),
                      pattern = ".tif$",
                      recursive = TRUE,
                      full.names = TRUE)
bpsCsv <- list.files(here::here(landfireDir, "biophysical-settings"),
                     pattern = ".csv$",
                     recursive = TRUE,
                     full.names = TRUE)

bps <- terra::rast(bpsFile)
bpsD <- read.csv(bpsCsv)


#Create new vegetation groups with certain veg types of interest highlighted
#output of case_when is based on the FIRST statement that is true; i.e. order of statements is priority order
newBpsCodes <- bpsD %>%
  mutate(NEWGROUPVEG = case_when(grepl("aspen", BPS_NAME, ignore.case = TRUE) & grepl("conifer", BPS_NAME, ignore.case = TRUE) ~ "AspenConifer",
                                 grepl("aspen", BPS_NAME, ignore.case = TRUE) ~ "Aspen",
                                 grepl("ponderosa", BPS_NAME, ignore.case = TRUE) ~ "Ponderosa",
                                 grepl("douglas-fir", BPS_NAME, ignore.case = TRUE) ~ "DouglasFir",
                                 grepl("mixed conifer", BPS_NAME, ignore.case = TRUE) ~ "MixedConifer", #NON ponderosa or doug fir mixed conifer, since these classes will be caught first
                                 grepl("pinyon|juniper", BPS_NAME, ignore.case = TRUE) ~ "PinyonOrJuniper",
                                 grepl("lodgepole", BPS_NAME, ignore.case = TRUE) ~ "Lodgepole"
  )) %>%
  mutate(NEWGROUPVEG = case_when(is.na(NEWGROUPVEG) ~ GROUPVEG,
                                 TRUE ~ NEWGROUPVEG)) %>%
  select(BPS_CODE, NEWGROUPVEG) %>%
  mutate(BPS_CODE = as.integer(BPS_CODE)) %>%
  distinct()


#extract
bpsDats <- terra::extract(bps, st_transform(fires, crs(bps)), bind = FALSE) %>%
  group_by(ID, BPS_CODE) %>%
  count() %>%
  ungroup() %>%
  mutate(n = n *900) %>% #turn to meters
  `colnames<-`(c("ID", "BPS_CODE", "mtrs_sq")) %>%
  mutate(BPS_CODE = as.integer(as.character(BPS_CODE))) %>%
  left_join(newBpsCodes, by=join_by(BPS_CODE)) %>%
  select(-BPS_CODE) %>%
  group_by(ID, NEWGROUPVEG) %>%
  summarise(mtrs_sq = sum(mtrs_sq)) %>%
  ungroup() %>%
  spread(NEWGROUPVEG, mtrs_sq) %>%
  replace(is.na(.), 0) %>%
  `colnames<-`(c("ID", paste("bps", colnames(.)[-1], sep="_")))


#join the data to polygons
fires <- fires %>%
  rowid_to_column("rowID") %>%
  left_join(bpsDats, by = c("rowID" = "ID")) %>%
  select(-rowID) %>%
  replace(is.na(.), 0) %>%
  mutate(bps_forest = rowSums(across(c(bps_Aspen, bps_AspenConifer, bps_Ponderosa, bps_MixedConifer, bps_PinyonOrJuniper, bps_Lodgepole)))) %>%
  mutate(bps_forest_perc = 100 * (bps_forest / area_mtrs)) %>%
  mutate(bps_forest_perc = case_when(bps_forest_perc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                     TRUE ~ bps_forest_perc)) %>%
  mutate(nlcd_forest_perc = 100 * (nlcd_ForestAll / area_mtrs)) %>%
  mutate(nlcd_forest_perc = case_when(nlcd_forest_perc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                     TRUE ~ nlcd_forest_perc)) %>%
  mutate(gapConusForestPerc = case_when(gapConusForestPerc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                      TRUE ~ gapConusForestPerc))


mapview(fires)

t <- fires %>% select(year, area_mtrs, bps_forest_perc, nlcd_forest_perc, gapConusForestPerc)
tt <- fires %>% select(ponderosa, bps_Ponderosa, lodgepole, bps_Lodgepole)

forestFires <- fires %>% filter(bps_forest_perc > 70 | nlcd_forest_perc > 70 | gapConusForestPerc > 70)
mapview(forestFires)

pondForestFires <- forestFires %>% mutate(bps_pond_perc = 100* (bps_Ponderosa / area_mtrs)) %>%
  filter(bps_pond_perc > 60)

mapview(pondForestFires)


st_write(fires, "data/derived/lba_forestType_test.gpkg", append = FALSE)
st_write(forestFires, "data/derived/lba_forestFires_test.gpkg", append = FALSE)
