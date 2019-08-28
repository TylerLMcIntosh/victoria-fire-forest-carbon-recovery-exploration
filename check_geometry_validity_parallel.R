# This script checks the validity of spatial geometries and fixes any invalid 
# entries using parallel processing. 

library(sf)
library(pbapply)
library(lwgeom)
library(parallel)

# define input geometry filename 
geom_in_filename <- here::here("data/data_raw/BLM_National_Surface_Management_Agency/sma_wm.gdb") # CONUS BLM SMA polygons

# define output geometry filename 
geom_out_filename <- here::here("data/data_output/blm_conus_sma_valid.shp")

# read the input feature class geometries.
# this function reads the first layer by default. 
geom_in <- sf::st_read(geom_in_filename)

# drop Z and/or M dimensions from feature geometries 
geom_drop_zm <- sf::st_zm(geom_in)

# for parallelization: spread the computation across multiple cores. 
# first, split up the data frame rows into a list
geom_split <- base::split(geom_drop_zm, seq(nrow(geom_drop_zm)))

# just use the first n rows for testing 
geom_split_subset <- geom_split[1:5]

# make the geometry valid for each row in the data frame.

# calculate the number of cores and set up a cluster 
num_cores <- parallel::detectCores() - 1
cluster <- parallel::makeCluster(num_cores)

geom_valid_list <- pbapply::pblapply(X = geom_split_subset
                                     ,FUN = lwgeom::st_make_valid
                                     ,cl = cluster)

# subset without splitting into a list 
geom_subset <- geom_drop_zm[1:5,]
geom_valid_list <- pbapply::pbapply(X = geom_subset
                                    , MARGIN = 1
                                     ,FUN = lwgeom::st_make_valid
                                     ,cl = cluster)

# stop the cluster 
parallel::stopCluster(cluster)
