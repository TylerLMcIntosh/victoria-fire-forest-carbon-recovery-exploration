# A script to visualize selected fires for the CAREER project

library(here)
library(sf)
library(tidyverse)

rm(list=ls()) #Ensure empty workspace

epsg <- "EPSG:5070" #Albers equal area

# FUNCTIONS ----

# A function to easily access EPA ecoregion data via VSI
# PARAMETERS
# level :: the EPA ecoregion level as an integer
# workLocal :: whether or not to work locally or operate only from the cloud. (TRUE/FALSE)
#              if TRUE, the EPA data will be downloaded to filePath, and, in the future, read from there.
#              this can speed up access if you are doing many repeat reads
# filePath :: the filePath of the data if you want to work locally (e.g. here::here('data/epa_l3.gpkg'))
access.epa.ecoregions <- function(level, workLocal, filePath = NA) {
  if(workLocal) {
    
    if(file.exists(filePath)) {
      epa <- sf::st_read(filePath)
      return(epa)
    }
    
  }
  
  if(level == 3) {
    epa <- glue::glue(
      "/vsizip/vsicurl/", #magic remote connection
      "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip", #copied link to download location
      "/us_eco_l3.shp") |> #path inside zip file
      sf::st_read()
  }
  
  if(level == 4) {
    epa <- glue::glue(
      "/vsizip/vsicurl/", #magic remote connection
      "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l4.zip", #copied link to download location
      "/us_eco_l4_no_st.shp") |> #path inside zip file
      sf::st_read()
  }
  
  if(workLocal) {
    sf::st_write(epa, filePath)
  }
  
  return(epa)
  
}


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


#Function to write a shapefile to a new, file-specific directory and add a zipped version
#    shp = the sf file to write to shapefile
#    location = path of directory to create the new file-specific subdirectory in
#    filename = name of file WITHOUT .shp
#    zipOnly = TRUE / FALSE, should the original (unzipped) files be removed?
#    overwrite = TRUE / FALSE, should files be overwritten?

# Example use:
# st_write_shp(shp = prepped_for_parks_etal,
#              location = here("data/derived"),
#              filename = "career_lba_for_parks_v1",
#              zipOnly = TRUE,
#              overwrite = TRUE)
st_write_shp <- function(shp, location, filename, zipOnly, overwrite) {
  
  #Check for required packages and install if not installed, then load
  list.of.packages <- c("zip","sf","here")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(zip)
  library(sf)
  library(here)
  
  
  
  #Create subdirectory & manage overwriting
  zipOnlyFile <- here::here(location, glue::glue("{filename}.zip"))
  outDir <- here::here(location, filename)
  
  if(!zipOnly & dir.exists(outDir) & overwrite) {
    unlink(outDir, recursive = TRUE)
  } else if (!zipOnly & dir.exists(outDir) & !overwrite) {
    stop("Directory already exists")
  }
  
  if(zipOnly & file.exists(zipOnlyFile) & overwrite) {
    unlink(zipOnlyFile)
  } else if (zipOnly & file.exists(zipOnlyFile) & !overwrite) {
    stop("Zip file already exists")
  }
  
  
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
  zipfile <- here::here(outDir, paste(filename, ".zip", sep=""))
  zip::zip(zipfile = zipfile,
           files = allShpNms,
           mode = "cherry-pick")
  
  
  #Remove raw files if desired
  if(zipOnly == TRUE) {
    file.copy(zipfile, zipOnlyFile)
    unlink(here(outDir), recursive = TRUE)          
  }
  
}


# OPERATE ----



priorityVisit <- c("44388", #"On The Border", - small fire near large complex
                   "35281", # Hells Canyon - 1994, small fire in northern range
                   "CO3937210525219960518", #"Buffalo Creek", - 1996, large fire near large complex
                   "49729", #Drake - 2005, small fire in northern range
                   "36486", #Elk Creek - 1995, small fire in northern range
                   "47135"# "Pallisade", small fire in northern range
                   # "45627", #unknown
                   # "47756", #campbell
                   # "45602", # locke mountain
                   # "42588", #"Cooper mountain"
                   # "47086", #phantom canyon
                   # "39564", #muddy creek - 1998
                   # "50636", #Tyndall gulch - 2006
                   # "CO3816010503620050706", #Mason - 2005
                   # "CO3874110535020080627" # nash ranch - 2008
)

flagged <- c("CO4066610545920020320", #unnamed - re-burned by recent wildfire
             #"CO3934910490620010926", #MVD PSFRXASST 4 - large, low-sev fire
             "47700", #unknown - re-burned by RX
             "45663", #Deep canyon - not near any others
             "39246" #North divide - doesn't seem to have actually happened... or if did, very low sev)
)

epaL4 <- access.epa.ecoregions(level = 4, workLocal = FALSE, filePath = NA) |>
  sf::st_transform(epsg)

crystalMidElevForests <- epaL4 |>
  dplyr::filter(US_L4CODE == "21c")

frontRange <- sf::st_read(here::here('data', 'front_range_shp.gpkg')) |>
  sf::st_transform(epsg)

viableFires <- sf::st_read(here::here('data', 'derived', 'viableFiresAll.gpkg')) |>
  sf::st_transform(epsg) |>
  sf::st_filter(frontRange) |>
  calc.intersect.area.and.perc(set2 = crystalMidElevForests, name = "epa21c") |>
  dplyr::filter(epa21c_perc >= 90) |> #Over 80% in 21c lvl4 ecoregion
  dplyr::filter(year <= 2009 & year >= 1994) |> #15 - 30 years ago
  dplyr::mutate(priorityVisit = purrr::map_lgl(event_id, ~ any(stringr::str_detect(stringr::str_to_lower(.x), str_to_lower(priorityVisit))))) |>
  dplyr::mutate(flagged = purrr::map_lgl(event_id, ~ any(stringr::str_detect(stringr::str_to_lower(.x), str_to_lower(flagged))))) |>
  dplyr::mutate(status = dplyr::case_when(priorityVisit == TRUE ~ "high priority",
                                          visited == TRUE ~ "visited",
                                          flagged == TRUE ~ "flagged",
                                          TRUE ~ "viable")) |>
  dplyr::filter(status != "flagged")


unvisited <- viableFires |> dplyr::filter(!visited)
visited <- viableFires |> dplyr::filter(visited)
priority <- viableFires |> dplyr::filter(priorityVisit)



p <- ggplot(data = viableFires, aes(x = year, y = area_hctrs)) +
  geom_point(aes(color = status)) +
  scale_y_continuous(trans = 'log2') +
  geom_vline(xintercept = 2000, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 2004, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 100, linetype = "dashed", color = "black") +
  geom_hline(yintercept = 1000, linetype = "dashed", color = "black")
p

length(priorityVisit)

mapview(viableFires |> dplyr::select(status), z.col = "status", burst = TRUE)
mapview(viableFires)


sf::st_write(viableFires, here::here('data', 'derived', 'viableFires1994_2009_epa21c.gpkg'), append = FALSE)
st_write_shp(shp = viableFires,
             location = here("data/derived"),
             filename = "viableFires1994_2009_epa21c",
             zipOnly = TRUE,
             overwrite = TRUE)
