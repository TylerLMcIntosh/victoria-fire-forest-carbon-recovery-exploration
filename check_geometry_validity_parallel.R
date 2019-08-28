
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

# just use the first 10 rows for testing 
geom_split_subset <- geom_split[1:10]

# make the geometry valid for each row in the data frame.
cluster <- parallel::makeCluster()
geom_valid_list <- pbapply::pblapply(X = geom_split_subset
                                     ,FUN = lwgeom::st_make_valid)
