# Purpose: model the combined effects of suppression, regional climate, elevation, vegetation, fire size, stand replacing decay coefficient

# Load libraries
library(tidyverse)
library(randomForest)
library(sf)

# Read metadata file
md <- read_csv("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv")
fires_by_severity <- read_csv("data/data_output/polygonized-fires-by-severity-classes.csv")

sev_cat_prop <-
  fires_by_severity %>% 
  dplyr::select(fire_id, sev_cat, sev_cat_prop) %>% 
  tidyr::spread(key = sev_cat, value = sev_cat_prop, fill = 0) %>% 
  dplyr::rename(prop_unchanged = `0`,
                prop_low = `1`,
                prop_mod = `2`,
                prop_high = `3`)

fires <- 
  read_csv("data/data_output/polygonized-fires-by-fire.csv") %>% 
  dplyr::left_join(md, by = "fire_id") %>% 
  dplyr::left_join(sev_cat_prop, by = "fire_id") %>% 
  dplyr::filter(!is.na(objective)) %>% 
  dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))

sdc <- 
  read_csv("data/data_output/high-severity-fires-with-sdc.csv") %>% 
  dplyr::select(fire_id, sdc, burn_duration)

fires_sdc <-
  fires %>% 
  left_join(sdc, by = "fire_id")


# Successful initial attack versus failed initial attack based
# on burn duration

ia <-
  fires_sdc %>% 
  dplyr::filter(burn_duration > 0,
                burn_duration < 365) %>% 
  dplyr::mutate(survived_ia = ifelse(burn_duration > 3, yes = 1, no = 0))

fm1 <- glm(survived_ia ~ scale(fire_area_m2 / 10000) + scale(prefire_ndvi) + scale(nbhd_sd_ndvi_1) + scale(earlyfire_hdw) + scale(elevation) + scale(sdc), data = ia, family = binomial())
summary(fm1)

# some models -------------------------------------------------------------


fm1 <- randomForest(sdc ~ objective + prefire_vpd + elevation + prefire_ndvi + area_ha, data = fires, importance = TRUE, proximity = TRUE)

fm2 <- lm(prefire_vpd ~ objective*area_ha, data = fires)

fm2 <- lm(log(fires$sdc, base = 10) ~ objective + prefire_vpd + elevation + prefire_ndvi + area_ha, data = fires)

