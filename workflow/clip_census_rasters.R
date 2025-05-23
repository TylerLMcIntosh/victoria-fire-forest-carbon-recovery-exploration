#clip census rasters

library(terra)
library(sf)
library(mapview)
library(here)

setwd("C:/Users/tyler/Documents/2023_FieldSeason/SouthernRockies/career_tree_censuses")

orthos <- grep(list.files("C:/Users/tyler/Documents/2023_FieldSeason/SouthernRockies/career_tree_censuses/orthos", pattern = ".tif", full.names = TRUE), 
               pattern = "_clipped.tif", invert = TRUE, value = TRUE)
clipping <- sf::st_read("clip_it_censuses.shp")

orthos <- orthos[4]

for(file in orthos) {
  r <- terra::rast(file)
  for(i in 1:nrow(clipping)) {
    clipP <- clipping[i,]
    print(i)
    clp <- tryCatch({r %>% terra::crop(clipP, mask = TRUE)}, error = function(cond){})
    if(!is.null(clp)) {
      print("Cropped & masked")
      newfile <- paste(substr(file, 0, nchar(file)-4), i, "_clipped.tif", sep="")
      terra::writeRaster(clp, newfile, overwrite = TRUE)
      print("Wrote file")
    } else {
      print("Clip not viable")
    }
  }
}


mapview(clipping)

