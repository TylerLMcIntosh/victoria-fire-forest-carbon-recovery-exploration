#Clear workspace
rm(list = ls())

#Set working directory
setwd("/Users/tyler/Documents/Field Planning/CAREER/")
here::i_am("FiresOfInterest_PullMetrics.r")

# Install packages
library(sf) 
library(here)
library(dplyr) 
library(kableExtra)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(landscapemetrics) #Fragstats alternative
library(stars)
library(mapview)
library(viridis)
library(terra)
library(tmap)


here::here()
setwd(here())
getwd()

#Get file names of severity data
severityFileNames <- list.files(path = here("data", "Co_PriorityFires", "Clean"), pattern ="_dnbr6.tif", full.names = TRUE)

#Substring from right function
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

#Read in all fire severities and ids into list, name elements by id
fireSeverities <- vector(mode = "list", 1)
ids <- c()
for (fileName in severityFileNames) {
  severity <- rast(fileName) %>% classify(cbind(0,NA)) #load via Terra, replace background with NAS
  id <- strtrim(substrRight(fileName, 49), 21)
  fireSeverities <- append(fireSeverities, severity)
  ids <- c(ids, id)
  print(id)
}
fireSeverities <- fireSeverities[-1]
names(fireSeverities) <- ids

#Load names of fires
firenames <- read.csv(here("data", "FireNames.csv"))
t <- ids %>% as.data.frame()
t <- t %>% dplyr::rename('Fire_ID' = colnames(t)[1]) %>% dplyr::mutate(Fire_ID = toupper(Fire_ID)) %>% dplyr::left_join(firenames, by='Fire_ID') %>% dplyr::select(-"X")

#plot all fires to check
jpeg(file = here("figures", "firesPlot.jpeg"), width = 2000, height = 1000)
par(mfrow = c(3,4))
for (i in 1:length(fireSeverities)) {
  plot(fireSeverities[[i]], main = t$Fire_Name[i], cex=2.8)
}
par(mfrow = c(1,1))
dev.off()


#Set up metric output
metricnames <- c('FireID',  'UnburnedPix', 'LowBurnPix', 'ModerateBurnPix', 
                 'HighBurnPix', 'IncreasedGreenPix', 'NonProcessPix', 'ShannonDiv', 'ShannonEven', 'PatchDiversity', 'EdgeDensity')
metrics <- data.frame(matrix(nrow = 0, ncol = length(metricnames)))

#Pull metrics for each fire
for (j in 1:length(fireSeverities)) {
  fire <- fireSeverities[[j]]
  singlefiremetrics <- c()
  singlefiremetrics <- c(singlefiremetrics, names(fireSeverities)[j])
  for (i in 1:6) {
    pix <- length(cells(fire, i)$Histogram)
    singlefiremetrics <- c(singlefiremetrics, pix)
  }
  
  #Landscape metrics
  fire %>% check_landscape()
  shdi <- lsm_l_shdi(fire)$value
  shei <- lsm_l_shei(fire)$value
  pd <- lsm_l_pd(fire)$value
  ed <- lsm_l_ed(fire)$value
  singlefiremetrics <- c(singlefiremetrics, shdi, shei, pd, ed)
  #Bind all metrics for fire
  metrics <- metrics %>% rbind(singlefiremetrics)
}
colnames(metrics) <- metricnames


