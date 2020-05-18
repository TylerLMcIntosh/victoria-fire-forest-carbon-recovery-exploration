library(sf) 
library(here)
library(dplyr) 
library(kableExtra)

# read shapefile where each row describes one fire 
fire_filename <- here::here("data/fire_stats_test.geojson")
fire_data <- sf::st_read(fire_filename)

# adjust the data type and/or precision of some fields
fire_data$id <- as.character(fire_data$id)
fire_data$Acres <- as.integer(fire_data$Acres)
fire_data$lodgepole <- round(as.numeric(as.character(fire_data$lodgepole)) * 100, digits = 1)
fire_data$ponderosa <- round(as.numeric(as.character(fire_data$ponderosa)) * 100, digits = 1)
fire_data$spruceFir <- round(as.numeric(as.character(fire_data$spruceFir)) * 100, digits = 1)
fire_data$disturbed_burned <- round(as.numeric(as.character(fire_data$disturbed_burned)) * 100, digits = 1)
fire_data$disturbed_unspecific <- round(as.numeric(as.character(fire_data$disturbed_unspecific)) * 100, digits = 1)
fire_data$disturbed_logged <- round(as.numeric(as.character(fire_data$disturbed_logged)) * 100, digits = 1)
fire_data$regenerating_disturbed <- round(as.numeric(as.character(fire_data$regenerating_disturbed)) * 100, digits = 1)
fire_data$regenerating_harvested <- round(as.numeric(as.character(fire_data$regenerating_harvested)) * 100, digits = 1)

# create a table to summarize attributes of interest per fire 
fire_table <- as.data.frame(fire_data) %>% 
  # select columns of interest
  dplyr::select(Fire_Name, Year, Acres, lodgepole, ponderosa, spruceFir, disturbed_burned, disturbed_unspecific, 
                disturbed_logged, regenerating_disturbed, regenerating_harvested) %>%
  # reorder the rows based on multiple variables 
  dplyr::arrange(desc(lodgepole), desc(ponderosa), desc(spruceFir), desc(disturbed_burned)) %>%
  # rename the columns for interpretation
  dplyr::rename("Fire Name" = Fire_Name, 
                "Year" = Year,
                "Acres" = Acres,
                "Lodgepole %" = lodgepole,
                "Ponderosa %" = ponderosa,
                "Spruce Fir %" = spruceFir,
                "Disturbed Burned %" = disturbed_burned,
                "Disturbed Unspecified %" = disturbed_unspecific,
                "Disturbed Logged %" = disturbed_logged,
                "Regenerating Disturbed %" = regenerating_disturbed,
                "Regenerating Harvested %" = regenerating_harvested) 

kableExtra::kable(fire_table) %>%
  kableExtra::kable_styling(bootstrap_options = "striped", 
                            full_width = F, 
                            position = "left")
