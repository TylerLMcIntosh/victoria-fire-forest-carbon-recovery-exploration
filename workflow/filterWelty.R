#Read in Welty combined perimeters
weltyPerims <- sf::st_read(here::here('data', 'welty_combined_wildland_fire_dataset', 'welty_combined_wildland_fire_perimeters.shp'))

filtWelty <- weltyPerims |> dplyr::filter(Fire_Year >= 2013 & Fire_Year <= 2015)
sf::st_write(filtWelty,
             here::here('data', 'welty_2013-2015.gpkg'))

filtWelty <- weltyPerims |> dplyr::filter(Fire_Year >= 2005 & Fire_Year <= 2015)
sf::st_write(filtWelty,
             here::here('data', 'welty_2005-2015.gpkg'))