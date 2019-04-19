# Purpose: set context for Sierra YPMC fires using Short (2017) data

library(tidyverse)
library(here)

sn_ypmc_fod <- read_csv(here::here("data/data_output/short-fpafod-sierra-ypmc-nonspatial.csv"))

fire_size_ecdf <- ecdf(sn_ypmc_fod$fire_size)

# Percent of fire events that are smaller than a single Landsat pixel (30m x 30m or about 0.222 acres)
fire_size_ecdf(0.222) # 0.6143517
# Percent of fire events that are smaller in size than the 4 hectare cutoff for remote-sensing-resistance dataset
fire_size_ecdf(10) # 0.9488143
# Percent of fire events that are smaller in size than the 80 hectare cutoff for the Region 5 Geospatial dataset
fire_size_ecdf(200) # 0.9793656
# Percent of fire events that are smaller in size than the 1000 acre cutoff for the MTBS dataset
fire_size_ecdf(1000) # 0.9887281


total_area_burned <- sum(sn_ypmc_fod$fire_size)
# 2185458 million hectares

total_ypmc_area_burned <- sum(sn_ypmc_fod$fire_size * sn_ypmc_fod$prop_ypmc)
# 1180487 million hectares