### ### ### ### ### ### ### ### ###
# Process & prep landsat burned area product for use in CAREER project

#Tyler McIntosh
#3/14/23


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
library(smoothr) #Package for smoothing and tidying spatial features: https://cran.r-project.org/web/packages/smoothr/vignettes/smoothr.html
library(doParallel) #Package for multi-threaded computation in R
  #Basics to get started: https://cran.r-project.org/web/packages/doParallel/vignettes/gettingstartedParallel.pdf
  #or: https://nceas.github.io/oss-lessons/parallel-computing-in-r/parallel-computing-in-r.html
  #NOTE: Only use parallel processing (eg foreach %dopar% call) for operations that take a long time!
  #parallelized operations on Windows (i.e. w/o forking) must have packages & functions fed to %dopar% using 
  #.export for functions and .packages for packages
library(tictoc) #benchmarking
library(beepr) #to make beeps
#library(concaveman) #Package for creation of concave/convex hulls - did NOT end up using

## Clean workspace & set env variables ----
rm(list=ls()) #Ensure empty workspace
here() #Check here location
#OR
#setwd() #Set working directory directly
options(digits = 2) #Set standard decimal print output
options(scipen = 999) #Turn scientific notation on and off (0 = on, 999 = off)
options(error = beep)

#Set CRS
projectCRS <- 'EPSG:26913' #Southern rockies NAD83 13N

#Set distance threshold for merging fire events
fireD <- 500 #500m

#Set fill_holes area threshold
fillHolesThresh <- units::set_units(100000, m^2) # = 10ha or ~25 acres

#Set smoothness degree
smoothDeg <- 1

#Set distance from road to consider
distFromRoad <- 1000 #1km

#Set area around AOI to consider
aoiBuffer <- 400000 #400km buffer

#Set reburn potential percent (the maximum possible potential reburn percentage of area, combining LBA & MTBS)
rpp <- 15

#Set up parallel computing
corz <- parallel::detectCores()-4 #set cores for doParallel to X=2 less than system cores (1 for OS, 1 for any other operations)
registerDoParallel(corz) #Register parallel processing

# Set output directory & create if doesn't already exist
outDir <- here("data", "derived")
if (!dir.exists(outDir)){
  dir.create(outDir)
}
thisRun <- here(outDir, paste("lba_dats", format(Sys.time(), "%Y-%m-%d-%H-%M-%S-%Z"), sep = "_"))
dir.create(thisRun)

## Load data ----

aoiToBuffer <- st_read("data/NEON_AOP/NIWOT_AOP_Flightbox.shp") %>% st_transform(projectCRS)

#Untar datasets to data if not already done
# tarList <- list.files("../../data_raw/fire/LandsatBurnedArea/", pattern = "*.tar.gz", full.names=TRUE)
# tarNames <- list.files("../../data_raw/fire/LandsatBurnedArea/", pattern = "*.tar.gz") %>% lapply(. %>% substr(0,28))
# mapply(function(str1, str2)
#   untar(str1, exdir = paste("/data/", str2, sep="")),
#   tarList, tarNames)


# Read in Landsat Burned Area product
lbaFiles <- list.files("data", pattern = "*BF_labeled.shp", recursive = TRUE, full.names=TRUE)
lbaPolyList <- lbaFiles %>% lapply(st_read)

# Load other relevant datasets for getting stats
gapCover <- terra::rast("data/GapConusCover2011_SouthernRockies_EPSG_32613_GEE.tif")
gapCodes <- read.csv("data/gap_conus_2011_codes.csv")
gapCodesLookup <- read.csv("data/gap_conus_lookup_codes.csv")
usfs <- sf::st_read("data/SMA_USFS/SMA_USFS_National.shp") %>% st_transform(projectCRS)
roads <- sf::st_read("data/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE.shp") %>% st_transform(projectCRS)
mtbs <- sf::st_read("data/derived/MTBS_Perims_200km_of_Niwot.shp") %>% st_transform(projectCRS)
wilderness <- sf::st_read("data/wilderness/wilderness.shp") %>% st_transform(projectCRS)
nlcdForestmask <- terra::rast(here('data', 'external', 'modal_forest_type_nlcd_srockies.tif'))


### ### ### ### ### ### ### ### ###

# Clip, transform, and merge LBA vectors ----


#Buffer area around AOP
aoi <- aoiToBuffer %>% sf::st_buffer(aoiBuffer)
plot(aoi)


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
test <- foreach(i = 1:length(lbaPolyList),
                .packages = 'sf') %dopar% {
  set <- lbaPolyList[[i]]
  out <- set %>%
    sf::st_filter(aoi) %>%
    sf::st_transform(projectCRS)
  return(out)
}
toc() #<9 seconds


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
  finalId <- seq(1:nrow(polys))
  polys <- polys %>% cbind(finalId) %>%
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
toc() # ~480 seconds

#Bind together & export to save
lbaPolyAll <- bind_rows(lbaPolyListEventMerged)
st_write(lbaPolyAll, here(thisRun, "lbs_aoi_merged.gpkg"), append=FALSE)



### ### ### ### ### ### ### ### ###

# Perform basic smoothing on merged LBA vectors ----

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



#DO NOT PERFORM SMOOTHING, NOT WORTHWHILE
# tic("parallel")
# lbaPolyAllSmooth <- foreach(i = 1:nrow(lbaPolyAll),
#                             .packages = 'smoothr',
#                             .combine = 'rbind') %dopar% {
#                               poly <- lbaPolyAll[i,] %>%
#                                 smoothr::fill_holes(threshold = fillHolesThresh) %>%
#                                 smoothr::smooth(method = "ksmooth", smoothness = smoothDeg)
#                               return(poly)
#                             }
# toc() #724.91 seconds
# 
# #Export to save
# st_write(lbaPolyAllSmooth, "data/derived/lbs_fixed_v2smoothed.shp")
# lbaPolyAllSmooth <- st_read("data/derived/lbs_fixed_v2smoothed.shp")
# 
# print("Smoothed & saved")


### ### ### ### ### ### ### ### ###

# Calculate relevant area and percentage information ----

#Calculate area
#Calculate areas
lbaPolyAll <- lbaPolyAll %>% dplyr::mutate(area_mtrs = as.numeric(st_area(lbaPolyAll)), #as.numeric to avoid creating units object
                                         # 1 m2 = 0.0.000247105 acres
                                         area_acres = area_mtrs * 0.000247105,
                                         # 1m2 = 0.0001 hectares
                                         area_hctrs = area_mtrs * 0.0001)
#Round to avoid too many digits
lbaPolyAll <- lbaPolyAll %>% dplyr::mutate(area_mtrs = round(area_mtrs, digits = 2),
                                         area_acres = round(area_acres, digits=2),
                                         area_hctrs = round(area_hctrs, digits=2))



## Key Functions ----
    #Function to calculate area within a different polygon set for a single polygon
    calc.area.single.poly <- function(polygon, otherSet) {
      intersection <- polygon %>% st_intersection(otherSet) #intersect polygon with other set of polygons

      if(nrow(intersection) > 1) { #if intersection resulted in more than one polygon, merge them into one
        intersection <- intersection %>%
          group_by(year) %>% #can group by year since year will be the same for all intersections
          summarize(geometry = st_union(geometry))
      }
      if(nrow(intersection) == 0) { #if intersection is empty, area is zero
        return(0)
      } else {  #calculate area
        intersection <- intersection %>% dplyr::mutate(area_mtrs = round(st_area(intersection), digits=2))
        return(intersection$area_mtrs)
      }
    }
    
    #Function to calculate the area of each polygon in set1
    #that intersects with another set of polygons (set2)
    #Function assumes that polygons are in the same projection and that the units of that projection are meters
    #(will calculate associated % of area if there is a column named "area_mtrs")
    #name = string, e.g. "mtbs"
    calc.intersect.area.and.perc <- function(set1, set2, name) {
      
      areas <- list()
      #For each polygon in set1, intersect it with set 2 and get area of intersected polygon
      for(i in seq_along(set1[[1]])) {
        areas[[i]] <- calc.area.single.poly(polygon = set1[i,], otherSet = set2)
      }
      

      added_area <- unlist(areas)
      
      set1 <- set1 %>% cbind(added_area)
      
      if ("area_mtrs" %in% colnames(set1)) { #If area_mtrs column is provided, calculate %
        set1 <- set1 %>%
          mutate(added_perc = round(100 * (added_area / area_mtrs), digits = 2)) %>%
          rename(!!paste(name, "perc", sep ="_") := added_perc)
      }
      
      set1 <- set1 %>% mutate() %>% #There is sometimes an agr error when trying to rename sf objects.
                                    #According to github issues, passing through empty mutate fixes. Seems to work.
        rename(!!paste(name, "ar_mtrs", sep = "_") := added_area)
      
      print(paste("Finished intersection area calcs with", name, sep = " "))
      
      return(set1)
    }
    
    #Function to calculate the area of each polygon in set1
    #that overlaps with OTHER polygons in the SAME set
    #Function assumes that polygons are in the same projection and that the units of that projection are meters
    #(will calculate associated % of area if there is a column named "area_mtrs")
    #name = string, e.g. "mtbs"
    calc.self.intersect.area.and.perc <- function(set1, name) {
      allIndex <- seq_along(set1[[1]])
      areas <- list()
      #For each polygon in set1, intersect it with the other polygons in set1 and get area of intersected polygon
      for(i in allIndex) {
        
        me <- set1[i,]
        keep <- allIndex[!allIndex %in% c(i)]
        others <- set1[keep,]
        areas[[i]] <- calc.area.single.poly(me, otherSet = others)
      }
      
      added_area <- unlist(areas)
      set1 <- set1 %>% cbind(added_area)
      
      if ("area_mtrs" %in% colnames(set1)) { #If area_mtrs column is provided, calculate %
        set1 <- set1 %>% 
          mutate(added_perc = round(100 * (added_area / area_mtrs), digits = 2)) %>%
          rename(!!paste(name, "perc", sep ="_") := added_perc)
      }
      
      set1 <- set1 %>% mutate() %>% #There is sometimes an agr error when trying to rename sf objects.
                                    #According to github issues, passing through empty mutate fixes. Seems to work.
        rename(!!paste(name, "ar_mtrs", sep = "_") := added_area)
      
      print(paste("Finished intersection area calcs with", name, sep = " "))
      
      return(set1)
    }




## Get intersection areas ----
lbaPolyAllDats <- lbaPolyAll %>%
  calc.intersect.area.and.perc(set2 = usfs, name = "usfs") %>% #get USFS overlap dats (should be > 0 for all since have done spatial filter)
  calc.intersect.area.and.perc(set2 = mtbs, name = "mtbs") %>% #get MTBS overlap dats (for reburn avoidance)
  calc.intersect.area.and.perc(set2 = wilderness, name = "wilderness") %>% #get Wilderness overlap
  calc.self.intersect.area.and.perc(name = "lba") #get LBA overlap dats (for reburn avoidance)



# Get GAP 2011 forest type areas ----
#extract and manipulate landcover data from GAP conus 2011 data
landcoverDats <- terra::extract(gapCover, lbaPolyAllDats, bind = FALSE) %>%
  left_join(gapCodesLookup,
            by = c("landcover" = "GAP_CONUS_2011_Code")) %>%
  filter(landcover!=0 & !is.na(landcover)) %>% 
  group_by(ID, Category) %>%
  count() %>%
  filter(Category == "lodgepole" | Category == "ponderosa" | Category == "mixedConifer" | Category == "spruceFir") %>%
  spread(Category, n) %>%
  replace(is.na(.), 0) %>%
  mutate(lodgepole = lodgepole * 900, #change to m^2
         ponderosa = ponderosa * 900,
         mixedConifer = mixedConifer * 900,
         spruceFir = spruceFir * 900)


#join the data to polygons
lbaPolyAllData <- lbaPolyAllDats %>%
  rowid_to_column("rowID") %>%
  left_join(landcoverDats, by = c("rowID" = "ID")) %>%
  select(-rowID, -ids) %>%
  replace(is.na(.), 0)


#CHANGE OF NAMING SCHEME (combined multiple scripts 4/25/23)
firesRaw <- lbaPolyAllData

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


st_write(fires, here(thisRun, "lba_aoi_all_data_added.gpkg"), append = FALSE)


# Perform filtering ----

forestFires <- fires %>% filter(bps_forest_perc > 70 | nlcd_forest_perc > 70 | gapConusForestPerc > 70)
mapview(forestFires)

pondForestFires <- forestFires %>% mutate(bps_pond_perc = 100* (bps_Ponderosa / area_mtrs)) %>%
  filter(bps_pond_perc > 60)

mapview(pondForestFires)


st_write(fires, "data/derived/lba_forestType_test.gpkg", append = FALSE)
st_write(forestFires, "data/derived/lba_forestFires_test.gpkg", append = FALSE)



# #remove large fires
# lbaPolyAll <- lbaPolyAll %>% dplyr::filter(area_acres < 1000)
# 
# print("Removed fires > 1000 acres")

## Filter out reburns ----
lbaPolyAllUSFSDats <- lbaPolyAllUSFSDats %>%
  mutate(reburnPotentialPerc = mtbs_perc + lba_perc) %>%
  filter(reburnPotentialPerc <= rpp)



## Filter to USFS ----
# tic("usfs")
# lbaPolyAllUSFS <- lbaPolyAll %>% st_filter(usfs) 
# print("Filtered to USFS")
# toc() #1057.63 seconds

tic('usfs parallel')
lbaPolyAllUSFS <- foreach(i = 1:nrow(lbaPolyAll),
                          .packages = 'sf',
                          .combine = 'rbind') %dopar% {
                            return(st_filter(lbaPolyAll[i,], usfs))
                          }
toc() #571.93 seconds


# Clip to near roads ----
roadBuffer <- roads %>% st_buffer(distFromRoad)
#lbaPolyAllDataFiltered <- lbaPolyAllData %>% st_filter(roadBuffer) 

lbaPolyAllDataFiltered <- foreach(i = 1:nrow(lbaPolyAllData),
                                  .packages = 'sf',
                                  .combine = 'rbind') %dopar% {
                                    return(st_filter(lbaPolyAllData[i,], roadBuffer))
                                  }



#close parallel cluster
stopImplicitCluster()
