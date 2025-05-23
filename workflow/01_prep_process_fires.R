### ### ### ### ### ### ### ### ###
# Process & prep Landsat Burned Area product & MTBS for use in CAREER project
# Clips to AOI
# In case of LBA, merges events in the same year within fireD distance of one another and smooths if performSmooth == TRUE
# Exports both files

#Tyler McIntosh
#Last updated: 4/25/23


#This script uses the following naming conventions wherever possible:
# lowerCamelCase for variables
# period.separated for functions
# underscore_separated for files

### ### ### ### ### ### ### ### ###

# Setup ----

## Libraries ----
#check the required libraries and download if needed
list.of.packages <- c("terra","sf","here","tidyverse","mapview","smoothr", "doParallel", "tictoc")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

#Load libraries
library(terra) #New raster package
library(sf) #New vector package
library(here)
library(tidyverse) #Tidyverse!!!
library(mapview) #Quick mapping
#library(smoothr) #Package for smoothing and tidying spatial features: https://cran.r-project.org/web/packages/smoothr/vignettes/smoothr.html
library(doParallel) #Package for multi-threaded computation in R
  #Basics to get started: https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
  #or: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
  #NOTE: Only use parallel processing (eg foreach %dopar% call) for operations that take a long time!
  #parallelized operations on Windows (i.e. w/o forking) must have packages & functions fed to %dopar% using 
  #.export for functions and .packages for packages
library(tictoc) #benchmarking
#library(beepr) #to make beeps
#library(concaveman) #Package for creation of concave/convex hulls - did NOT end up using

## Clean workspace & set env variables ----
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
#options(error = beep)

#Set CRS
projectCRS <- 'EPSG:26913' #Southern rockies NAD83 13N

#Set distance threshold for merging fire events
fireD <- 500 #500m - this is the same merge-threshold used by the Welty dataset: https://www.sciencebase.gov/catalog/item/61aa537dd34eb622f699df81

#Set fill_holes area threshold
fillHolesThresh <- units::set_units(100000, m^2) # = 10ha or ~25 acres



### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###

# Start of Scripted ----

#Set area around AOI to consider
aoiBuffer <- 400000 #400km buffer

#Set out file name
appendNm <- paste((aoiBuffer / 1000), "km_buff", sep = "")

# Set output directory & create if doesn't already exist
outDir <- here("data", "derived", paste("prepped_fire_dats", appendNm, sep = "_"))
if (!dir.exists(outDir)){
  dir.create(outDir)
}

#Set up parallel computing
corz <- parallel::detectCores()-4 #set cores for doParallel to X=2 less than system cores (1 for OS, 1 for any other operations)
registerDoParallel(corz) #Register parallel processing

## Load data ----

#Load AOI
aoiToBuffer <- st_read("data/NEON_AOP/NIWOT_AOP_Flightbox.shp") %>%
  st_transform(projectCRS)

#Buffer area around AOI
aoi <- aoiToBuffer %>% sf::st_buffer(aoiBuffer)
plot(aoi)

#Untar datasets to data if not already done
# tarList <- list.files("../../data_raw/fire/LandsatBurnedArea/", pattern = "*.tar.gz", full.names=TRUE)
# tarNames <- list.files("../../data_raw/fire/LandsatBurnedArea/", pattern = "*.tar.gz") %>% lapply(. %>% substr(0,28))
# mapply(function(str1, str2)
#   untar(str1, exdir = paste("/data/", str2, sep="")),
#   tarList, tarNames)


# Read in Landsat Burned Area product
lbaFiles <- list.files("data",
                       pattern = "*BF_labeled.shp",
                       recursive = TRUE,
                       full.names=TRUE)
lbaPolyList <- lbaFiles %>% lapply(st_read)


#Read in MTBS perimeters
mtbsPerims <- sf::st_read(here('data', 'mtbs_perimeter_data', 'mtbs_perims_DD.shp')) %>%
  st_transform(projectCRS)

#Read in Welty combined perimeters
weltyPerims <- sf::st_read(here('data', 'welty_combined_wildland_fire_dataset', 'welty_combined_wildland_fire_perimeters.shp')) %>%
  st_transform(projectCRS)

### ### ### ### ### ### ### ### ###

# Operate on LBA vectors ----

## Filter to AOI buffer ----

#Intersect and transform smaller section of dataset
#(now using parallel compute)
tempCRS <- crs(lbaPolyList[[1]])
aoi <- aoi %>% st_transform(tempCRS)

# #single-threaded
# tic("normal")
# lbaPolyList <- lbaPolyList %>%
#   lapply(function(shp) { #intersect
#     print("Filter start")
#     st_filter(shp, aoi)}) %>% 
#   lapply(function(shp) st_transform(shp, projectCRS)) #transform
# toc() #~18 seconds

#multi-threaded
tic("parallel %dopar%")
lbaPolyList <- foreach(i = 1:length(lbaPolyList),
                .packages = 'sf') %dopar% {
  set <- lbaPolyList[[i]]
  out <- set %>%
    sf::st_filter(aoi) %>%
    sf::st_transform(projectCRS)
  return(out)
}
toc() #<9 seconds w/ 200km buffer


## Perform event merging on LBA ----
#Now we need to take each shapefile and combine those fires
#that are within 500m of each other into multi-polygons

#Function to iteratively combine polygons into multipolygons by a set geographic distance between them
merge_within_distance <- function (polys, distance) {
  
  #Add identifiers
  ids <- seq(1:nrow(polys))
  polys <- polys %>% cbind(ids)
  
  yr <- polys[1,]$year #get year
  
  print(paste("Starting", yr, sep = " "))
  
  #for each polygon, create the set
  for (i in 1:nrow(polys)) {
    p <- polys[i,] #get working polygon
    id <- p$ids #get ID of the working polygon
    associated <- p %>% st_is_within_distance(polys, dist = distance) #get the polygons within a distance of working poly
    associated <- associated[[1]] #get just the numbers
    #Change IDs of associated polygons to be the same as working polygon
    polys <- polys %>% mutate(workingID = case_when(ids %in% associated ~ id)) %>%
      mutate(ids = coalesce(workingID, ids))
  }
  
  #Turn into multipolygons by new IDs and then create final ids
  polys <- polys %>%
    group_by(ids) %>%
    summarize(geometry = st_union(geometry))
  
  #Add final ids & year
  event_id <- seq(1:nrow(polys))
  polys <- polys %>% cbind(event_id) %>%
    mutate(year = yr)
  return(polys)
}

#Apply function to all datasets (now using parallel compute)
# tic("normal")
# lbaPolyListEventMerged <- lbaPolyList %>%
#   lapply(function(polys) merge_within_distance(polys, fireD))
# toc() # ~1053 seconds

tic("test: dopar")
lbaPolyListEventMerged <- foreach(i = 1:length(lbaPolyList),
                .export = 'merge_within_distance',
                .packages = c('sf', 'dplyr')) %dopar% {
  return(merge_within_distance(lbaPolyList[[i]], fireD))
}
toc() # ~480 seconds for 200km buffer, 7417 seconds for 400km buffer

#Bind together
lbaPolyAll <- bind_rows(lbaPolyListEventMerged)


### ### ### ### ### ### ### ### ###

## Perform basic smoothing on merged LBA vectors ----

#Since LBA data is landsat-direct derived (i.e. no human interpretation),
#it is likely to primarily contain "high" or "medium" severity burned areas
#(See Hawbaker et al. 2020 for graphics comparing to MTBS)
#To create polygons a bit more comparable to MTBS for area estimates, smoothing is done with smoothr package
#Operations:
   #fill_holes will fill holes in a polygon that are less than a certain threshold in size
   #smooth(... method = "ksmooth", smoothness = x) will smooth polygons according to a Gaussian kernel regression, degree of smoothing controlled by x

# tic("normal")
# lbaPolyAllSmooth1 <- lbaPolyAll %>%
#   smoothr::fill_holes(threshold = fillHolesThresh) %>%
#   smoothr::smooth(method = "ksmooth", smoothness = smoothDeg)
# toc() #3075.22 seconds


  print("Filling holes")
  
  tic("parallel")
  lbaPolyAllSmooth <- foreach(i = 1:nrow(lbaPolyAll),
                              .packages = 'smoothr',
                              .combine = 'rbind') %dopar% {
                                poly <- lbaPolyAll[i,] %>%
                                  smoothr::fill_holes(threshold = fillHolesThresh) #%>%
                                  #smoothr::smooth(method = "ksmooth", smoothness = smoothDeg)
                                return(poly)
                              }
  toc() #724.91 seconds
  




## Calculate LBA area and percentage information ----

#Calculate area
lbaPolyFinal <- lbaPolyAllSmooth %>% dplyr::mutate(area_mtrs = as.numeric(st_area(.)), #as.numeric to avoid creating units object
                                         # 1m2 = 0.0.000247105 acres
                                         area_acres = area_mtrs * 0.000247105,
                                         # 1m2 = 0.0001 hectares
                                         area_hctrs = area_mtrs * 0.0001) %>%
    dplyr::mutate(area_mtrs = round(area_mtrs, digits = 2), #Round to avoid too many digits
                                         area_acres = round(area_acres, digits=2),
                                         area_hctrs = round(area_hctrs, digits=2)) %>%
    dplyr::mutate(event_name = NA) %>%
    dplyr::mutate(dataset = "LBA") %>%
    dplyr::select(-ids)



## Export LBA ----
st_write(lbaPolyFinal,
         here(outDir, paste("lba_prepped_", appendNm, ".gpkg", sep = "")),
         append=FALSE)

### ### ### ### ### ### ### ### ###

# Operate on MTBS perimeters

## Filter to AOI buffer ----

aoi <- aoi %>% st_transform(projectCRS)
filtMTBS <- mtbsPerims %>% sf::st_filter(aoi)

finalMTBS <- filtMTBS %>%
  filter(Incid_Type == "Wildfire") %>%
  mutate(year = year(Ig_Date)) %>%
  dplyr::mutate(area_mtrs = as.numeric(st_area(.)), #as.numeric to avoid creating units object
                                                  # 1m2 = 0.0.000247105 acres
                                                  area_acres = area_mtrs * 0.000247105,
                                                  # 1m2 = 0.0001 hectares
                                                  area_hctrs = area_mtrs * 0.0001) %>% 
 dplyr::mutate(area_mtrs = round(area_mtrs, digits = 2),
                  area_acres = round(area_acres, digits=2),
                  area_hctrs = round(area_hctrs, digits=2)) %>%
 dplyr::mutate(event_id = Event_ID) %>%
 dplyr::mutate(event_name = Incid_Name) %>%
 dplyr::mutate(dataset = "MTBS") %>%
 dplyr::select(year:dataset)

## Export MTBS ----
st_write(finalMTBS,
         here(outDir, paste("mtbs_prepped_", appendNm, ".gpkg", sep = "")),
         append=FALSE)


# Operate on welty combined wildland fire dataset

#Filter to AOI
filtWelty <- weltyPerims %>% sf::st_filter(aoi)

#Create fields
finalWelty <- filtWelty %>%
  dplyr::filter(Assigned_F == "Wildfire") %>%
  dplyr::mutate(year = Fire_Year) %>%
  dplyr::mutate(area_mtrs = as.numeric(st_area(.)), #as.numeric to avoid creating units object
                # 1m2 = 0.0.000247105 acres
                area_acres = area_mtrs * 0.000247105,
                # 1m2 = 0.0001 hectares
                area_hctrs = area_mtrs * 0.0001) %>% 
  dplyr::mutate(area_mtrs = round(area_mtrs, digits = 2),
                area_acres = round(area_acres, digits=2),
                area_hctrs = round(area_hctrs, digits=2)) %>%
  dplyr::mutate(event_id = USGS_Assig) %>%
  dplyr::mutate(event_name = Listed_F_1) %>%
  dplyr::mutate(dataset = "WELTY") %>%
  dplyr::select(year:dataset)

## Export Welty ----
st_write(finalWelty,
         here(outDir, paste("welty_prepped_", appendNm, ".gpkg", sep = "")),
         append=FALSE)

#close parallel cluster
stopImplicitCluster()
