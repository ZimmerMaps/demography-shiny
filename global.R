library(dplyr)

allzones <- readRDS("data/zmb_data.rds")
allzones$latitude <- jitter(allzones$latitude)
allzones$longitude <- jitter(allzones$longitude)
row.names(allzones) <- allzones$zones

cleantable <- allzones