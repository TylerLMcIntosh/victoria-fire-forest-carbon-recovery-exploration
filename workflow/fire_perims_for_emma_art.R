
library(sf)
library(terra)
library(here)
library(tidyverse)

epa_l3 <- glue::glue(
  "/vsizip/vsicurl/", #magic remote connection
  "https://gaftp.epa.gov/EPADataCommons/ORD/Ecoregions/us/us_eco_l3.zip", #copied link to download location
  "/us_eco_l3.shp") |> #path inside zip file
  sf::st_read()

southernRockies <- epa_l3 |>
  dplyr::filter(US_L3NAME == "Southern Rockies") |>
  dplyr::group_by(US_L3NAME) |>
  dplyr::summarise(geometry = sf::st_union(geometry)) |>
  dplyr::ungroup() |>
  sf::st_transform('EPSG:5070')

weltyPerims <- sf::st_read(here('data', 'welty_combined_wildland_fire_dataset', 'welty_combined_wildland_fire_perimeters.shp')) |>
  sf::st_transform('EPSG:5070') |>
  sf::st_filter(southernRockies)


years <- seq(2000, 2020)

outputFireImage <- function(yr) {
  fires <- weltyPerims |>
    filter(Assigned_F == "Wildfire") |>
    filter(Fire_Year == yr)
  
  ggplot() +
    geom_sf(data = southernRockies, aes(), fill = "white", color = "black") +
    geom_sf(data = fires, aes(), fill = "red", color = NA) +
    labs(title = glue::glue("Southern Rockies fires in {yr}")) +
    theme(plot.title = element_text(size = 50))
  
  ggsave(here::here('figs', 'emma_art', glue::glue("sr_fires_{yr}.jpg")),
         dpi = 300,
         units = "px",
         width = 6000,
         height = 12000)
}


years |> purrr::walk(~outputFireImage(.x))


