# Purpose: create a raster layer of yellow pine/mixed-conifer for easy subsetting of the Short (2017) dataset

library(tidyverse)
library(raster)
library(sf)
library(fasterize)
library(rgdal)

sn <- st_read("data/data_output/jepson_sierra-nevada-ecoregion/jepson_sierra-nevada-ecoregion.shp") %>% 
  st_transform(3310)
# template raster in epsg3310
r <- raster(sn, res = 30)

nsn <- 
  st_read("data/data_raw/features/FRID-sierra-nevada/FRID_NorthSierra17_1.gdb") %>% 
  dplyr::filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))
ssn <- 
  st_read("data/data_raw/features/FRID-sierra-nevada/FRID_SouthSierra17_1.gdb") %>% 
  dplyr::filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))

ypmc_vector <- rbind(nsn, ssn)

ypmc_raster <- fasterize(sf = ypmc_vector, raster = r, field = NULL, background = 0)

writeRaster(ypmc_raster, "data/data_output/frid-ypmc.tif", overwrite = TRUE)

# plot(ypmc_raster)
# plot(sn$geometry, add = TRUE)
