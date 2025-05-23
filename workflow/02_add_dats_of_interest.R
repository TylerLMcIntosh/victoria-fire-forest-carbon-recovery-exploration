### ### ### ### ### ### ### ### ###
# Add data of interest to fire perimeter polygons
# E.g. land cover, reburn, USFS, wilderness, proximity to roads, elevation

#Takes in data from 01_prep_process_fires.R

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


#Set distance from road to consider
distFromRoad <- 1000 #1km


dataBuffSize <- 400

appendNm <- paste(dataBuffSize, "km_buff_nofilter", sep="")
#appendNm <- paste(dataBuffSize, "km_buff", sep="")


#Data directory, outputs from 01_prep_process_LBA.R
fireDatFolder <- here::here("data", "derived", paste("prepped_fire_dats_", paste(dataBuffSize, "km_buff", sep=""), sep=""))

# Set output directory & create if doesn't already exist
outDir <- here("data", "derived", paste("final_fire_dats", appendNm, sep = "_"))
if (!dir.exists(outDir)){
  dir.create(outDir)
}

#Set up parallel computing
corz <- parallel::detectCores()-4 #set cores for doParallel to X=2 less than system cores (1 for OS, 1 for any other operations)
registerDoParallel(corz) #Register parallel processing


#Load fire perimeters datasets
mtbs <- sf::st_read(here::here(fireDatFolder, "mtbs_prepped_400km_buff.gpkg")) %>%
  st_transform(projectCRS)
lba <- sf::st_read(here::here(fireDatFolder, "lba_prepped_400km_buff.gpkg")) %>%
  st_transform(projectCRS)
welty <- sf::st_read(here::here(fireDatFolder, "welty_prepped_400km_buff.gpkg")) %>%
  st_transform(projectCRS)


#Load landfire BPS data
landfireDir <- here::here('data', 'landfire/')
bpsFile <- list.files(here::here(landfireDir, "biophysical-settings"),
                      pattern = ".tif$",
                      recursive = TRUE,
                      full.names = TRUE)
bpsCsv <- list.files(here::here(landfireDir, "biophysical-settings"),
                     pattern = ".csv$",
                     recursive = TRUE,
                     full.names = TRUE)

bpsD <- read.csv(bpsCsv)
bps <- terra::rast(bpsFile)



# Load other raster datasets for getting stats, do NOT project
gapCover <- terra::rast("data/GapConusCover2011_SouthernRockies_EPSG_32613_GEE.tif")
gapCodes <- read.csv("data/gap_conus_2011_codes.csv")
gapCodesLookup <- read.csv("data/gap_conus_lookup_codes.csv")
nlcdForestmask <- terra::rast(here('data', 'external', 'modal_forest_type_nlcd_srockies.tif'))
elevation <- terra::rast(here('data', 'raw_GEE', "USGS_SRTM_30m_SouthernRockies_EPSG_32613_GEE.tif"))


#Load vector datasets & transform
usfs <- sf::st_read("data/SMA_USFS/SMA_USFS_National.shp") %>%
  st_transform(projectCRS)
roads <- sf::st_read("data/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE/Roads_Tiger_Census_SouthernRockies_EPSG_26913_GEE.shp") %>%
  st_transform(projectCRS)
wilderness <- sf::st_read("data/wilderness/wilderness.shp") %>%
  st_transform(projectCRS)


######### RUN THIS ############

# DO NOT FILTER TO USFS ----
#Perform this filter afterwards bsed on the intersected areas
# # Filter to subset ----
# ## Subset to USFS fires ----
# # tic("usfs")
# # lbaPolyAllUSFS <- lbaPolyAll %>% st_filter(usfs) 
# # print("Filtered to USFS")
# # toc() #1057.63 seconds
# 
# #For LBA
# tic('usfs parallel')
# lbaFilt <- foreach(i = 1:nrow(lba),
#                           .packages = 'sf',
#                           .combine = 'rbind') %dopar% {
#                             return(st_filter(lba[i,], usfs))
#                           }
# toc() #571.93 seconds
# 
# #For MTBS
# mtbsFilt <- foreach(i = 1:nrow(mtbs),
#                    .packages = 'sf',
#                    .combine = 'rbind') %dopar% {
#                      return(st_filter(mtbs[i,], usfs))
#                    }
# 
# #For Welty
# weltyFilt <- foreach(i = 1:nrow(welty),
#                    .packages = 'sf',
#                    .combine = 'rbind') %dopar% {
#                      return(st_filter(welty[i,], usfs))
#                    }

# DO NOT FILTER TO NEAR ROADS ----
#Perform this filter afterwards based on distance from road
## Subset to near roads ----
# roadBuffer <- roads %>% st_buffer(distFromRoad)
# 
# #For LBA
# lbaFilt <- foreach(i = 1:nrow(lbaFilt),
#                                   .packages = 'sf',
#                                   .combine = 'rbind') %dopar% {
#                                     return(st_filter(lbaFilt[i,], roadBuffer))
#                                   }
# 
# #For MTBS
# mtbsFilt <- foreach(i = 1:nrow(mtbsFilt),
#                    .packages = 'sf',
#                    .combine = 'rbind') %dopar% {
#                      return(st_filter(mtbsFilt[i,], roadBuffer))
#                    }
# 
# #For Welty
# weltyFilt <- foreach(i = 1:nrow(weltyFilt),
#                    .packages = 'sf',
#                    .combine = 'rbind') %dopar% {
#                      return(st_filter(weltyFilt[i,], roadBuffer))
#                    }

# Add overlap information ----
## Key Functions ----
#Function to calculate area within a different polygon set for a single polygon
calc.area.single.poly <- function(polygon, otherSet) {
  intersection <- polygon %>% st_intersection(otherSet) #intersect polygon with other set of polygons
  
  if(nrow(intersection) > 1) { #if intersection resulted in more than one polygon, merge them into one
    intersection <- intersection %>%
      group_by(year) %>% #can group by year since year will be the same for all intersections
      summarize(geom = st_union(geom))
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
lbaDats <- lba %>%
  calc.intersect.area.and.perc(set2 = usfs, name = "usfs") %>% #get USFS overlap dats (should be > 0 for all since have done spatial filter)
  calc.intersect.area.and.perc(set2 = mtbs, name = "mtbs") %>% #get MTBS overlap dats (for reburn avoidance & identifying co-located fires)
  calc.intersect.area.and.perc(set2 = welty, name = "welty") %>% #get welty overlap dats (for reburn avoidance & identifying co-located fires)
  calc.intersect.area.and.perc(set2 = wilderness, name = "wilderness") %>% #get Wilderness overlap
  calc.self.intersect.area.and.perc(name = "lba") #get LBA overlap dats (for reburn avoidance)

mtbsDats <- mtbs %>%
  calc.intersect.area.and.perc(set2 = usfs, name = "usfs") %>% #get USFS overlap dats (should be > 0 for all since have done spatial filter)
  calc.intersect.area.and.perc(set2 = lba, name = "lba") %>% #get MTBS overlap dats (for reburn avoidance & identifying co-located fires)
  calc.intersect.area.and.perc(set2 = welty, name = "welty") %>% #get welty overlap dats (for reburn avoidance & identifying co-located fires)
  calc.intersect.area.and.perc(set2 = wilderness, name = "wilderness") %>% #get Wilderness overlap
  calc.self.intersect.area.and.perc(name = "mtbs") #get LBA overlap dats (for reburn avoidance)

weltyDats <- welty %>%
  calc.intersect.area.and.perc(set2 = usfs, name = "usfs") %>% #get USFS overlap dats (should be > 0 for all since have done spatial filter)
  calc.intersect.area.and.perc(set2 = mtbs, name = "mtbs") %>% #get MTBS overlap dats (for reburn avoidance & identifying co-located fires)
  calc.intersect.area.and.perc(set2 = lba, name = "lba") %>% #get welty overlap dats (for reburn avoidance & identifying co-located fires)
  calc.intersect.area.and.perc(set2 = wilderness, name = "wilderness") %>% #get Wilderness overlap
  calc.self.intersect.area.and.perc(name = "welty") #get LBA overlap dats (for reburn avoidance)


# Get distance to road ----

#Union roads
roadsUnioned <- roads %>% summarise(geometry = sf::st_union(geometry))

#Function to get distance from dats1 to dats2
#and return dats1 with a new column called 'colNm'
get.distance.between <- function(dats1, dats2, colNm) {
  distances <- dats1 %>% sf::st_distance(dats2) %>%
    as.numeric() %>% #returns units object by default
    as.data.frame() %>%
    `names<-`({{colNm}})
  return(cbind(dats1, distances))
}

#Run road function
lbaDats <- get.distance.between(lbaDats, roadsUnioned, 'dist_road_m')
mtbsDats <- get.distance.between(mtbsDats, roadsUnioned, 'dist_road_m')
weltyDats <- get.distance.between(weltyDats, roadsUnioned, 'dist_road_m')


#write out after performing spatial intersections & road distance
sf::st_write(weltyDats, here::here(outDir, paste("welty_all_intersected", appendNm, ".gpkg", sep = "")), append = FALSE)
sf::st_write(mtbsDats, here::here(outDir, paste("mtbs_all_intersected", appendNm, ".gpkg", sep = "")), append = FALSE)
sf::st_write(lbaDats, here::here(outDir, paste("lba_all_intersected", appendNm, ".gpkg", sep = "")), append = FALSE)

#If starting script midway, reload here
# weltyDats <- sf::st_read(here::here(outDir, paste("welty_all_intersected", appendNm, ".gpkg", sep = "")))
# mtbsDats <- sf::st_read(here::here(outDir, paste("mtbs_all_intersected", appendNm, ".gpkg", sep = "")))
# lbaDats <- sf::st_read(here::here(outDir, paste("lba_all_intersected", appendNm, ".gpkg", sep = "")))


# Add landcover data ----

## Get GAP 2011 forest type areas ----

add.gap.conus.2011 <- function(polys) {
  landcoverDats <- terra::extract(gapCover, polys, bind = FALSE) %>%
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
  polys <- polys %>%
    rowid_to_column("rowID") %>%
    left_join(landcoverDats, by = c("rowID" = "ID")) %>%
    select(-rowID) %>% #     select(-rowID, -ids) %>%
    replace(is.na(.), 0)
  
  polys <- polys %>%
    mutate(gapConusForestTot = lodgepole + mixedConifer + ponderosa + spruceFir) %>%
    mutate(gapConusForestPerc = (gapConusForestTot / area_mtrs) * 100) %>%
    mutate(gapConusForestPerc = case_when(gapConusForestPerc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                          TRUE ~ gapConusForestPerc)) %>%
    mutate(gapConusPondPerc = (ponderosa / area_mtrs) * 100) %>%
    mutate(gapConusPondPerc = case_when(gapConusPondPerc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                                     TRUE ~ gapConusPondPerc))
  
  
  ### ### ### ### ### ###
  #Also do classification scheme that Victoria used
  
  #GEE code from Victoria:
  # var lodgepole = gaplf.eq(149).or(gaplf.eq(150)).rename("lodgepole");
  # var ponderosa = gaplf.eq(139).or(gaplf.eq(141)).or(gaplf.eq(157)).or(gaplf.eq(158)).rename("ponderosa");
  # var spruceFir = gaplf.eq(151).or(gaplf.eq(152)).rename("spruceFir");
  # var dstrb_brnd = gaplf.eq(570).or(gaplf.eq(573)).rename("disturbed_burned");
  # var dstrb_unsp = gaplf.eq(565).rename("disturbed_unspecific");
  # var dstrb_logd = gaplf.eq(566).rename("disturbed_logged");
  # var regen_harv = gaplf.eq(567).or(gaplf.eq(568)).or(gaplf.eq(569)).rename("regenerating_harvested");
  # var regen_dist = gaplf.eq(574).or(gaplf.eq(575)).or(gaplf.eq(576)).rename("regenerating_disturbed");
  # // lodgepoole, ponderosa pine, and spruceFir combined
  # var forest_cover = gaplf.eq(149).or(gaplf.eq(150)).or(gaplf.eq(139))
  # .or(gaplf.eq(141)).or(gaplf.eq(157)).or(gaplf.eq(158)).or(gaplf.eq(151)).or(gaplf.eq(152)
  
  oldCodes <- c(149, 150, 139, 141, 157, 158, 151, 152)
  Category <- c('lodgepole', 'lodgepole', 'ponderosa', 'ponderosa', 'ponderosa', 'ponderosa', 'spruceFir', 'spruceFir')
  vGAPc <- cbind(oldCodes, Category) %>%
    as.data.frame() %>%
    mutate(oldCodes = as.integer(oldCodes))
  
  v_landcoverDats <- terra::extract(gapCover, polys, bind = FALSE) %>%
    left_join(vGAPc,
              by = c("landcover" = "oldCodes")) %>%
    filter(landcover!=0 & !is.na(landcover)) %>% 
    group_by(ID, Category) %>%
    count() %>%
    filter(Category == "lodgepole" | Category == "ponderosa" | Category == "spruceFir") %>%
    spread(Category, n) %>%
    replace(is.na(.), 0) %>%
    mutate(v_lodgepole = lodgepole * 900, #change to m^2
           v_ponderosa = ponderosa * 900,
           v_spruceFir = spruceFir * 900) %>%
    select(-lodgepole, -ponderosa, -spruceFir)
  
  #join the data to polygons
  polys <- polys %>%
    rowid_to_column("rowID") %>%
    left_join(v_landcoverDats, by = c("rowID" = "ID")) %>%
    select(-rowID) %>% #     select(-rowID, -ids) %>%
    replace(is.na(.), 0)
  
  polys <- polys %>%
    mutate(v_gapConusForestTot = v_lodgepole + v_ponderosa + v_spruceFir) %>%
    mutate(v_gapConusForestPerc = (v_gapConusForestTot / area_mtrs) * 100) %>%
    mutate(v_gapConusForestPerc = case_when(v_gapConusForestPerc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                          TRUE ~ v_gapConusForestPerc)) %>%
    mutate(v_gapConusPondPerc = (v_ponderosa / area_mtrs) * 100) %>%
    mutate(v_gapConusPondPerc = case_when(v_gapConusPondPerc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                        TRUE ~ v_gapConusPondPerc))
  

  return(polys)
}


weltyDats <- weltyDats %>% add.gap.conus.2011()
mtbsDats <- mtbsDats %>% add.gap.conus.2011()
lbaDats <- lbaDats %>% add.gap.conus.2011()





## Add NLCD modal forest data ----

add.nlcd.modal.forest <- function(polys) {
  
  #Add NLCD modal forest mask (from linked disturbance project) ----
  #41: deciduous
  #42: evergreen
  #43: mixed forest
  
  #NLCD forest codes
  forestCodes <- cbind(c(0, 41, 42, 43), c("nlcd_NonForest", "nlcd_Deciduous", "nlcd_Evergreen", "nlcd_MixedForest")) %>%
    `colnames<-`(c("forestCode", "forestType")) %>%
    as.data.frame()
  forestCodes$forestCode <- as.integer(forestCodes$forestCode)
  
  nlcdForestDats <- terra::extract(nlcdForestmask, polys, bind = FALSE) %>%
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
  polys <- polys %>%
    rowid_to_column("rowID") %>%
    left_join(nlcdForestDats, by = c("rowID" = "ID")) %>%
    select(-rowID) %>%
    replace(is.na(.), 0) %>%
    mutate(nlcd_forest_perc = 100 * (nlcd_ForestAll / area_mtrs)) %>%
    mutate(nlcd_forest_perc = case_when(nlcd_forest_perc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                        TRUE ~ nlcd_forest_perc))
  
  return(polys)
}


weltyDats <- weltyDats %>% add.nlcd.modal.forest()
mtbsDats <- mtbsDats %>% add.nlcd.modal.forest()
lbaDats <- lbaDats %>% add.nlcd.modal.forest()



#Add BPS classes ----

add.bps.classes <- function(polys) {
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
  bpsDats <- terra::extract(bps, st_transform(polys, crs(bps)), bind = FALSE) %>%
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
  polys <- polys %>%
    rowid_to_column("rowID") %>%
    left_join(bpsDats, by = c("rowID" = "ID")) %>%
    select(-rowID) %>%
    replace(is.na(.), 0) %>%
    mutate(bps_forest = rowSums(across(c(bps_Aspen, bps_AspenConifer, bps_Ponderosa, bps_MixedConifer, bps_PinyonOrJuniper, bps_Lodgepole)))) %>%
    mutate(bps_forest_perc = 100 * (bps_forest / area_mtrs)) %>%
    mutate(bps_forest_perc = case_when(bps_forest_perc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                       TRUE ~ bps_forest_perc))  %>%
    mutate(bps_pond_mix_perc = 100* ((bps_Ponderosa + bps_MixedConifer) / area_mtrs))  %>%
    mutate(bps_pond_mix_perc = case_when(bps_pond_mix_perc > 100 ~ 100, #some end up at over 100% as a result of partial pixels, fix
                                         TRUE ~ bps_pond_mix_perc))
  
  
  
  return(polys)
}

weltyDats <- weltyDats %>% add.bps.classes()
mtbsDats <- mtbsDats %>% add.bps.classes()
lbaDats <- lbaDats %>% add.bps.classes()



# Add elevation dats ----

weltyDats <- terra::extract(elevation, weltyDats, bind = TRUE, fun = mean) %>% st_as_sf()
mtbsDats <- terra::extract(elevation, mtbsDats, bind = TRUE, fun = mean) %>% st_as_sf()
lbaDats <- terra::extract(elevation, lbaDats, bind = TRUE, fun = mean) %>% st_as_sf()


# Write datasets pre-filtering

sf::st_write(weltyDats, here::here(outDir, paste("welty_dats_all", appendNm, ".gpkg", sep = "")), append = FALSE)
sf::st_write(mtbsDats, here::here(outDir, paste("mtbs_dats_all", appendNm, ".gpkg", sep = "")), append = FALSE)
sf::st_write(lbaDats, here::here(outDir, paste("lba_dats_all", appendNm, ".gpkg", sep = "")), append = FALSE)

#re-load data if starting midway through pipeline
# weltyDats <- sf::st_read(here::here(outDir, paste("welty_dats_all", appendNm, ".gpkg", sep = "")))
# mtbsDats <- sf::st_read(here::here(outDir, paste("mtbs_dats_all", appendNm, ".gpkg", sep = "")))
# lbaDats <- sf::st_read(here::here(outDir, paste("lba_dats_all", appendNm, ".gpkg", sep = "")))





# 
# # Perform filtering ----
# 
# ## Filter out reburns w/ self, non-forest fires, non-ponderosa/mixedConifer forests
# 
# weltyDatsFilt <- weltyDats %>%
#   filter(welty_perc <= rpp) %>% #filter out self-set-reburns
#   filter(bps_forest_perc > fpm | nlcd_forest_perc > fpm | gapConusForestPerc > fpm) #filter to forest fires
# 
# # weltyDatsPond <- weltyDatsFilt %>%
# #   filter(bps_pond_mix_perc > 60)
# 
# mtbsDatsFilt <- mtbsDats %>%
#   filter(mtbs_perc <= rpp) %>% #filter out self-set-reburns
#   filter(bps_forest_perc > fpm | nlcd_forest_perc > fpm | gapConusForestPerc > fpm) #filter to forest fires
# 
# # mtbsDatsPond <- mtbsDatsFilt %>%
# #   filter(bps_pond_mix_perc > 60)
# 
# #for LBA, also filter out overlaps from MTBS
# #(avoid large-scale reburns and anything large enough to be caught by MTBS)
# 
# lbaDatsFilt <- lbaDats %>%
#   filter(lba_perc + mtbs_perc <= rpp) %>% #filter out self-set-reburns AND MTBS reburn / MTBS overlap
#   filter(bps_forest_perc > fpm | nlcd_forest_perc > fpm | gapConusForestPerc > fpm) #filter to forest fires
# 
# # lbaDatsPond <- lbaDatsFilt %>%
# #   filter(bps_pond_mix_perc > 60)
# 
# # Write out datasets ----
# 
# sf::st_write(weltyDatsFilt, here::here(outDir, paste("welty_dats_forest_noreburn", appendNm, ".gpkg", sep = "")), append = FALSE)
# #sf::st_write(weltyDatsPond, here::here(outDir, paste("welty_dats_pond_mixed_noreburn", appendNm, ".gpkg", sep = "")), append = FALSE)
# 
# sf::st_write(mtbsDatsFilt, here::here(outDir, paste("mtbs_dats_forest_noreburn", appendNm, ".gpkg", sep = "")), append = FALSE)
# #sf::st_write(mtbsDatsPond, here::here(outDir, paste("mtbs_dats_pond_mixed_noreburn", appendNm, ".gpkg", sep = "")), append = FALSE)
# 
# sf::st_write(lbaDatsFilt, here::here(outDir, paste("lba_dats_forest_noreburn", appendNm, ".gpkg", sep = "")), append = FALSE)
# #sf::st_write(lbaDatsPond, here::here(outDir, paste("lba_dats_pond_mixed_noreburn", appendNm, ".gpkg", sep = "")), append = FALSE)
# 


#close parallel cluster
stopImplicitCluster()
