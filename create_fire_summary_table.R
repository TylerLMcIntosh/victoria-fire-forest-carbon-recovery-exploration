library(sf) 
library(here)
library(dplyr) 
library(kableExtra)
library(ggplot2)

# read shapefile where each row describes one fire 
fire_filename <- here::here("data/fire_stats_test.geojson")
fire_df <- sf::st_read(fire_filename)

# adjust the data type and/or precision of some fields
fire_df$id <- as.character(fire_df$id)
fire_df$Acres <- as.integer(fire_df$Acres)
fire_df$lodgepole <- round(as.numeric(as.character(fire_df$lodgepole)) * 100, digits = 1)
fire_df$ponderosa <- round(as.numeric(as.character(fire_df$ponderosa)) * 100, digits = 1)
fire_df$spruceFir <- round(as.numeric(as.character(fire_df$spruceFir)) * 100, digits = 1)
fire_df$disturbed_burned <- round(as.numeric(as.character(fire_df$disturbed_burned)) * 100, digits = 1)
fire_df$disturbed_unspecific <- round(as.numeric(as.character(fire_df$disturbed_unspecific)) * 100, digits = 1)
fire_df$disturbed_logged <- round(as.numeric(as.character(fire_df$disturbed_logged)) * 100, digits = 1)
fire_df$regenerating_disturbed <- round(as.numeric(as.character(fire_df$regenerating_disturbed)) * 100, digits = 1)
fire_df$regenerating_harvested <- round(as.numeric(as.character(fire_df$regenerating_harvested)) * 100, digits = 1)

# create a table to summarize attributes of interest per fire 
fire_table <- as.data.frame(fire_df) %>% 
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



# Explore fire characteristics  -------------------------------------------


# Fire Years -----------------------

year_min <- min(fire_df$Year)
year_max <- max(fire_df$Year)
hist_year_title <- paste("Histogram: Fire Years,", year_min, "-", year_max)

ggplot(fire_df, aes(x=Year)) + 
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  labs(title = hist_year_title, y = "Count") + 
  theme_bw()


# Fire Size -----------------------

# don't use sci notation on x axis
options(scipen=10000)

# convert units to hectares
ggplot(fire_df, aes(x=Acres * 0.404686)) + 
  geom_histogram(fill="#69b3a2", color="#e9ecef", alpha=0.9) + 
  labs(title = "Histogram: Fire Size", y = "Count", x = "Size [Ha]") + 
  theme_bw()


