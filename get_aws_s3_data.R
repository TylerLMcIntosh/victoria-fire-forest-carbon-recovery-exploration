library(aws.s3)

# use your access ID and key credentials in the console
Sys.setenv("AWS_ACCESS_KEY_ID" = "<YOUR_ACCESS_ID>",
           "AWS_SECRET_ACCESS_KEY" = "<YOUR_KEY>")

# save file from AWS bucket locally
aws.s3::save_object(object = "s3://earthlab-jmcglinchy/blm_shapefiles/sma_geoms.zip"
                    , file = "sma_geoms.zip")

# extract the zipped files 
utils::unzip("sma_geoms.zip")
