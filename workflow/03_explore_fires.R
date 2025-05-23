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
list.of.packages <- c("terra","sf","here","tidyverse","mapview","smoothr", "doParallel", "tictoc", "ggrepel", "zip")
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
library(ggrepel) #for easy plot labeling
library(zip) #for zipping together shapefiles

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



### ### ### ### ### ### ### ### ###
# Load data ----

datDir <- here::here('data', 'derived', 'final_fire_dats_400km_buff_nofilter')
# weltyPM <- sf::st_read(here::here(datDir, 'welty_dats_pond_mixed_noreburn400km_buff.gpkg'))
# mtbsPM <- sf::st_read(here::here(datDir, 'mtbs_dats_pond_mixed_noreburn400km_buff.gpkg'))
# lbaPM <- sf::st_read(here::here(datDir, 'lba_dats_pond_mixed_noreburn400km_buff.gpkg'))



#These datasets have NOT been filtered for reburns, roads, USFS, wilderness, etc!
welty <- sf::st_read(here::here(datDir, 'welty_dats_all400km_buff_nofilter.gpkg'))
mtbs <- sf::st_read(here::here(datDir, 'mtbs_dats_all400km_buff_nofilter.gpkg'))
lba <- sf::st_read(here::here(datDir, 'lba_dats_all400km_buff_nofilter.gpkg')) |>
  dplyr::mutate(event_name = paste0("LBA", event_id))



#fired dataset for confirming LBA fires - note that this does NOT include 1999 fires!
fired <- sf::st_read(here::here('data', 'fired_uscan_to2021121_events.gpkg')) %>% sf::st_transform(projectCRS)


#Since lba is documenting smaller fires, & only actual burn pixel, does not capture fires over ice/snow/etc
#drop these columns from all datasets
dropCols <- base::setdiff(names(welty), names(lba))

drop.cols <- function(dats, dropCols) {
  for (nm in dropCols) {
    if (nm %in% names(dats)) {
      dats <- dats %>% select(-{{nm}})
    }
  }
  return(dats)
}

welty <- welty %>% drop.cols(dropCols)
mtbs <- mtbs %>% drop.cols(dropCols)
lba <- lba %>% drop.cols(dropCols)


### ### ### ### ### ### ### ### ###
# Clip back down to 200km to test out ----
#Load AOI
aoiToBuffer <- st_read("data/NEON_AOP/NIWOT_AOP_Flightbox.shp") %>%
  st_transform(projectCRS)

aoi <- aoiToBuffer %>% sf::st_buffer(400000)

#filter fired data to make smaller
fired_400 <- fired %>%
  sf::st_filter(aoi) %>%
  filter(ig_year >= 1999 & ig_year <= 2003)


### ### ### ### ### ### ### ### ###
# Filter down dats ----

filter.to.usfs.notwild <- function(dat, usfs, wild) {
  return(dat %>% filter(usfs_perc >= usfs & 
                          wilderness_perc <= wild))
}
filter.to.1999.2003 <- function(dat) {
  return(dat %>% filter(year >= 1999 & year <= 2003))
}
filter.to.pond <- function(dat, pond) {
  return(dat %>% filter(bps_pond_mix_perc > pond | 
                          v_gapConusPondPerc > pond | 
                          gapConusPondPerc > pond))
}
filter.to.forest <- function(dat, forest) {
  return(dat %>% filter(bps_forest_perc > forest | 
                          nlcd_forest_perc > forest | 
                          gapConusForestPerc > forest |
                          v_gapConusForestPerc > forest))
}

plot.fire.year.size <- function(dat, title, file, labs) {
  p <- ggplot(data = dat, aes(x = year, y = area_hctrs)) +
    geom_point(aes(color = dataset)) +
    scale_y_continuous(trans = 'log2') +

    labs(title = title, subtitle = paste(nrow(dat), "total viable fires", sep =" ")) +
    xlim(1980, 2020)
  if(labs == TRUE) {
  p <- p + geom_label_repel(aes(label = event_name),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50')
  }
  print(p)
  ggsave(here::here('figures', paste(file, ".png", sep = "")), units = "px", width = 3000, height = 2000)
} 
plot.fire.elev.size <- function(dat, title, file, labs) {
  p <- ggplot(data = dat, aes(x = area_hctrs, y = elevation)) +
    geom_point(aes(color = dataset)) +
    scale_x_continuous(trans = 'log2') +
    geom_label_repel(aes(label = event_name),
                     box.padding   = 0.35, 
                     point.padding = 0.5,
                     segment.color = 'grey50') +
    labs(title = title, subtitle = paste(nrow(dat), "total viable fires", sep =" ")) +
    ylim(1585, 3350) #boulder to treeline
  if(labs == TRUE) {
  p <- p + geom_label_repel(aes(label = event_name),
                            box.padding   = 0.35, 
                            point.padding = 0.5,
                            segment.color = 'grey50')
  }
  print(p)
  ggsave(here::here('figures', paste(file, ".png", sep = "")), units = "px", width = 3000, height = 2000)
} 

filter.fires <- function(usfs, wild, pond, rpp, forest, radius, elev) {
  mtbsClean <- mtbs %>%
    filter.to.usfs.notwild(usfs, wild) %>%
    #filter.to.1999.2003() %>%
    filter.to.pond(pond) %>%
    filter.to.forest(forest) %>%
    filter(mtbs_perc <= rpp) #remove reburns
  
  weltyClean <- welty %>%
    filter.to.usfs.notwild(usfs, wild) %>%
    #filter.to.1999.2003() %>%
    filter.to.pond(pond) %>%
    filter.to.forest(forest) %>%
    filter(welty_perc <= rpp) #remove reburns
  
  lbaClean <- lba %>%
    filter.to.usfs.notwild(usfs, wild) %>%
    #filter.to.1999.2003() %>%
    filter.to.pond(pond) %>%
    filter.to.forest(wild) %>%
    filter(lba_perc + mtbs_perc <= rpp) #remove reburns
  
  
  ## Confirm LBA fires w/ FIRED ----

  lbaCleanConfirmed <- lbaClean %>%
    sf::st_filter(fired_400, .predicate = st_intersects)
  
  # Combine to single dataset ----
  #remove welty fires already in mtbs & lba fires already in welty, then join
  allClean_400 <- rbind(weltyClean %>% filter(mtbs_perc<75),
                        mtbsClean,
                        lbaCleanConfirmed %>% filter(welty_perc<75))
  
  #Buffer area around AOI
  aoiBuffer <- 1000*radius
  aoi <- aoiToBuffer %>% sf::st_buffer(aoiBuffer)
  
  outs <- allClean_400%>%
    sf::st_filter(aoi)
  
  #Filter to only include fires in the Southern Rockies ecoregion!
  outs <- outs %>%
    filter(!is.na(elevation) & elevation != 0)
  
  #Filter to elevation range accepted
  #The median elevation of all pond fires in the SR is 2434
  minE <- 2434 - elev
  maxE <- 2434 + elev
  outs <- outs %>%
    filter(elevation >= minE & elevation <= maxE)
  
  return(outs)
}

pond_usfs_200 <- filter.fires(usfs = 50, 
                              wild = 10, 
                              pond = 40, 
                              rpp = 15, 
                              forest = 40, 
                              radius = 200,
                              elev = 500) #16
pond_usfs_private_200 <- filter.fires(usfs = 0, 
                              wild = 10, 
                              pond = 40, 
                              rpp = 15, 
                              forest = 40, 
                              radius = 200,
                              elev = 500) #21
pond_usfs_400 <- filter.fires(usfs = 50, 
                              wild = 10, 
                              pond = 40, 
                              rpp = 15, 
                              forest = 40, 
                              radius = 400,
                              elev = 500) #23
pond_usfs_private_400 <- filter.fires(usfs = 0, 
                                      wild = 10, 
                                      pond = 40, 
                                      rpp = 100, 
                                      forest = 40, 
                                      radius = 400,
                                      elev = 500) #40
strict_pond_usfs_200 <- filter.fires(usfs = 50, 
                              wild = 10, 
                              pond = 60, 
                              rpp = 15, 
                              forest = 40, 
                              radius = 200,
                              elev = 500)
strict_pond_usfs_private_200 <- filter.fires(usfs = 0, 
                                      wild = 10, 
                                      pond = 60, 
                                      rpp = 15, 
                                      forest = 40, 
                                      radius = 200,
                                      elev = 500) #40
strict_pond_usfs_private_400 <- filter.fires(usfs = 0, 
                                             wild = 10, 
                                             pond = 60, 
                                             rpp = 15, 
                                             forest = 40, 
                                             radius = 400,
                                             elev = 500) 


#### USING THIS FOR NOW

plot.fire.year.size.options <- function(dat, title, file, labs, xlim) {
  nVisited <- nrow(dat |> dplyr::filter(visited))
  nNew <- nrow(dat |> dplyr::filter(!visited))
  nTot <- nrow(dat)
  nNewUSFS <- nrow(dat |> dplyr::filter(!visited & usfs_perc >= 50))
  p <- ggplot(data = dat, aes(x = year, y = area_hctrs)) +
    geom_vline(xintercept = 2000, linetype = "dashed", color = "black") +
    geom_vline(xintercept = 2004, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
    geom_hline(yintercept = 1000, linetype = "dashed", color = "black") +
    geom_point(aes(alpha = usfs_perc, color = visited, shape = dataset)) +
    scale_y_continuous(trans = 'log2') +
    labs(title = title, subtitle = paste(nTot, "total viable fires\n", nNew, "total new viable fires - ", nNewUSFS, "with 50%+ USFS coverage\n", "(", nVisited, " visited so far)", sep =" ")) +
    xlim(xlim)
  if(labs == TRUE) {
    p <- p + geom_label_repel(aes(label = event_name),
                              box.padding   = 0.35, 
                              point.padding = 0.5,
                              segment.color = 'grey50')
  }
  print(p)
  ggsave(here::here('figures', paste(file, ".png", sep = "")), units = "px", width = 3000, height = 2000)
} 

#########
visited <- c("High Meadows", "Bobcat", "Eldorado", "Snaking", "Schoonover", "Hayman", "Overland", "LBA766", "LBA694", "Prevent", "Big Elk")
             #"Mason", "Buffalo Creek", "North Divide") #new fires to visit, large but outside timeframe

viable_fires <- filter.fires(usfs = 0, 
                             wild = 10, 
                             pond = 40, 
                             rpp = 15, 
                             forest = 40, 
                             radius = 800,
                             elev = 500) |>
  dplyr::mutate(visited = purrr::map_lgl(event_name, ~ any(stringr::str_detect(stringr::str_to_lower(.x), str_to_lower(visited)))))

sf::st_write(viable_fires , here::here('data', 'derived', 'viableFiresAll.gpkg'), append = FALSE)



viable_new_fires <- viable_fires |> dplyr::filter(!visited)
visited_fires <- viable_fires |> dplyr::filter(visited)

plot.fire.year.size.options(dat = viable_fires,
                            title = "40% Ponderosa Fires w/in 800km & w/in 500m of elevation median, 1980-2020",
                            file = 'pond_options_all',
                            labs = FALSE,
                            xlim = c(1980, 2020))


plot.fire.year.size.options(dat = viable_fires |> dplyr::filter(year >= 2000 & year <= 2004),
                            title = "40% Ponderosa Fires w/in 800km & w/in 500m of elevation median, 2000-2004",
                            file = 'pond_options_2000-2004',
                            labs = FALSE,
                            xlim = c(2000, 2004))

plot.fire.year.size.options(dat = viable_fires |> dplyr::filter(year >= 1995 & year <= 2006),
                            title = "40% Ponderosa Fires w/in 800km & w/in 500m of elevation median, 1995-2006",
                            file = 'pond_options_1995-2006',
                            labs = FALSE,
                            xlim = c(1995, 2006))


mapview(viable_fires |> dplyr::filter(year >= 2000 & year <= 2004), col.regions = "gray") +
  mapview(viable_new_fires |> dplyr::filter(year >= 2000 & year <= 2004), zcol = 'usfs_perc')
mapview(welty, col.regions = "gray") + mapview(viable_fires, col.regions = "green")

sf::st_write(viable_new_fires , here::here('data', 'derived', 'viableFires2024.gpkg'))
sf::st_write(viable_new_fires |> dplyr::filter(year >= 2000 & year <= 2004), here::here('data', 'derived', 'viableFires2024_2000-2004.gpkg'))

################################################################

plot.fire.elev.size(pond_usfs_200,
                    "USFS 40% Ponderosa Fires w/in 200km & w/in 500m of elevation median",
                    "pond_usfs_200") #16 - one of these is in WY. have filed to visit all of them except WY one
ggsave(here('figs', 'pond_usfs_200_plot_fire_elev_size.png'))

plot.fire.elev.size(pond_usfs_private_200,
                    "All 40% Ponderosa Fires w/in 200km & w/in 500m of elevation median",
                    "pond_all_200")
plot.fire.elev.size(pond_usfs_400,
                    "USFS 40% Ponderosa Fires w/in 400km & w/in 500m of elevation median",
                    "pond_usfs_400")
plot.fire.elev.size(pond_usfs_private_400,
                    "All 40% Ponderosa Fires w/in 400km & w/in 500m of elevation median",
                    "pond_all_400")



plot.fire.elev.size(dat = strict_pond_usfs_200,
                    title = "USFS 60% Ponderosa Fires w/in 200km & w/in 500m of elevation median, all years",
                    file = "strict_pond_usfs_200_all_years")
plot.fire.year.size(dat = strict_pond_usfs_200,
                    title = "USFS 60% Ponderosa Fires w/in 200km & w/in 500m of elevation median, all years",
                    file = 'strict_pond_usfs_200_all_years_fire_year')
plot.fire.elev.size(dat = pond_usfs_200,
                    title = "USFS 40% Ponderosa Fires w/in 200km & w/in 500m of elevation median, all years",
                    file = "pond_usfs_200_all_years",
                    labs = TRUE)
plot.fire.year.size(dat = pond_usfs_200,
                    title = "USFS 40% Ponderosa Fires w/in 200km & w/in 500m of elevation median, all years",
                    file = 'pond_usfs_200_all_years_fire_year',
                    labs = TRUE)
plot.fire.elev.size(dat = pond_usfs_400,
                    title = "USFS 40% Ponderosa Fires w/in 400km & w/in 500m of elevation median, all years",
                    file = "pond_usfs_400_all_years",
                    labs = TRUE)
plot.fire.year.size(dat = pond_usfs_400,
                    title = "USFS 40% Ponderosa Fires w/in 400km & w/in 500m of elevation median, all years",
                    file = 'pond_usfs_400_all_years_fire_year',
                    labs = TRUE)


plot.fire.elev.size(strict_pond_usfs_private_200,
                    "All 60% Ponderosa Fires w/in 200km & w/in 500m of elevation median",
                    "strict_pond_all_200")

plot.fire.elev.size(pond_usfs_private_400,
                    "All 40% Ponderosa Fires w/in 400km & w/in 500m of elevation median",
                    "pond_all_400")


plot.fire.year.size(pond_usfs_200,
                    title = "pond_usfs_200_plot_fire_year_size",
                    file = 'pond_usfs_200_plot_fire_year_size')

mapview(pond_usfs_200, lwd = 5)

# Set output directory & create if doesn't already exist
outDir <- here("data", "derived", "filtered")
if (!dir.exists(outDir)){
  dir.create(outDir)
}


sf::st_write(pond_usfs_200, here("data", "derived", "filtered", "ponderosa_usfs_200.gpkg"))
sf::st_write(pond_usfs_private_200, here("data", "derived", "filtered", "ponderosa_usfs_private_200.gpkg"))
sf::st_write(pond_usfs_400, here("data", "derived", "filtered", "ponderosa_usfs_400.gpkg"))
sf::st_write(pond_usfs_private_400, here("data", "derived", "filtered", "ponderosa_usfs_private_400_allow_reburns.gpkg"))





t <- strict_pond_usfs_200 %>% select(event_name, dataset, bps_pond_mix_perc)





#sf::st_write(allClean_200, here::here(outDir, "all_1999_2003_usfsPortion_nonWild.gpkg"))


#Function to export by year for GEE
export.by.year <- function(dats, nm) {
  for (i in 1999:2003) {
    split <- dats %>% filter(year == i)
    sf::st_write(split, here::here(outDir, paste(nm, "_", as.character(i), ".shp", sep = "")), append = FALSE)
    #GET NAMES LIST AND ZIP
    
    files <- list.files(path = outDir, pattern = paste(nm, "_", as.character(i), "*", sep = ""), full.names = TRUE)
    
    #Zip together
    zip::zip(zipfile = here(outDir, paste(nm, "_", as.character(i), ".zip", sep="")),
             files = c(files),
             mode = "cherry-pick")
  }
}

#HAVE TO UNION FIRST!!


export.by.year(pond_usfs_private_400, "pond_usfs_private_400")

