#Visualize fires

#Using this methodology we will sample 36 wildfires (N=12 per size class:
#                                                      10-100 ha; 100-1000 ha; and >1000 ha)

library(tidyverse)
library(here)
library(sf)
library(mapview)

#1999 - 2003
forestFires <- sf::st_read(here("data", "derived", "lba_forestFires_test.gpkg"))

forestFiresUSFS <- forestFires %>% filter(usfs_perc >= 60)

forestFiresUSFS <- forestFiresUSFS %>% mutate(ponderosa_perc = (ponderosa / area_mtrs) * 100,
                                              bps_ponderosa_perc = (bps_Ponderosa / area_mtrs) * 100)


ggplot(forestFiresUSFS) +
  geom_point(aes(x = ponderosa_perc, y = bps_ponderosa_perc, color = year))

forestFiresUSFS$ponderosa

ponderosaFiresUSFS <- forestFiresUSFS %>% filter(ponderosa_perc >= 50 | bps_ponderosa_perc >= 50)

ggplot(ponderosaFiresUSFS) +
  geom_point(aes(x = year, y = area_acres)) + 
  labs(title = "Ponderosa fires")
ggsave(here("figures", "Ponderosa fires_acrs.png"))

ggplot(forestFiresUSFS) +
  geom_point(aes(x = year, y = area_hctrs)) + 
  labs(title = "Forest  fires")

mapview(ponderosaFiresUSFS)
