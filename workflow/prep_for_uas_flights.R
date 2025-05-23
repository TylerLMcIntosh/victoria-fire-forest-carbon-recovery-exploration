

library(elevatr)
library(terra)
library(sf)
library(here)
library(dplyr)


# Set global
epsg <- "EPSG:26913"


# Pull DEM for front range
frontRange <- sf::st_read(here::here('data', 'front_range_shp.gpkg')) |>
  sf::st_transform(epsg)
frontRangeDEM <- elevatr::get_elev_raster(frontRange, z = 12) |> #https://cran.r-project.org/web/packages/elevatr/vignettes/introduction_to_elevatr.html#Key_information_about_version_0990_and_upcoming_versions_of_elevatr
  terra::rast() |> 
  terra::project(terra::crs(epsg)) |>
  terra::writeRaster(here::here('data', 'frontRangeDEM26913.tif'), overwrite = TRUE)


# DJI Functions



# Load transects file




# Filter to desired set




#Export for DJI

