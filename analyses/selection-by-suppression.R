# Purpose: model the combined effects of suppression, regional climate, elevation, vegetation, fire size, stand replacing decay coefficient

# Load libraries
library(tidyverse)
library(sf)
library(lubridate)
library(here)

# Read metadata file
md <- read_csv(here::here("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv"))
md_sf <- st_read(here::here("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON")) %>% st_transform(3310)
sdc <- 
  read_csv(here::here("data/data_output/high-severity-fires-with-sdc.csv")) %>% 
  dplyr::select(fire_id, sdc)

fires_by_fire <- read_csv(here::here("data/data_output/polygonized-fires-by-fire.csv"))
fires_by_severity <- read.csv(here::here("data/data_output/polygonized-fires-by-severity-classes.csv"))

# short_fires <- st_read("data/data_output/sierra-nevada-ypmc-short-fod.gpkg") %>% dplyr::rename(fod_id = FOD_ID)
# ee_short_fires <- 
#   st_read("data/data_output/ee_short-burning-conditions_48-day-window_L4578_bicubic.geojson") %>% 
#   st_drop_geometry() %>% 
#   dplyr::select(fod_id, alarm_date, cont_date, fire_size, prop_ypmc, elev, earlyFfm100, fm100, hdw, vs, vpd, RBR, preFire_ndvi, het_ndvi_1, focal_mean_ndvi_1)

sev_cat_prop <-
  fires_by_severity %>% 
  dplyr::select(fire_id, sev_cat, sev_cat_prop) %>% 
  tidyr::spread(key = sev_cat, value = sev_cat_prop, fill = 0) %>% 
  dplyr::rename(prop_unchanged = `0`,
                prop_low = `1`,
                prop_mod = `2`,
                prop_high = `3`)

fires <-
  md_sf %>% 
  dplyr::mutate(burn_duration = as.numeric(ymd(paste(cont_year, cont_month, cont_day, sep = "-")) - ymd(paste(alarm_year, alarm_month, alarm_day, sep = "-")))) %>% 
  left_join(fires_by_fire, by = "fire_id") %>% 
  dplyr::left_join(sev_cat_prop, by = "fire_id") %>% 
  dplyr::filter(!is.na(objective)) %>% 
  dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))


fires_sdc <-
  fires %>% 
  left_join(sdc, by = "fire_id")

# Successful initial attack versus failed initial attack based
# on burn duration

# Pier fire was definitely not contained in 2 days. Contained on 2017-11-29, not 2017-08-30?
# 0000f67fc2d69fe5c384
fires_sdc$cont_month[fires_sdc$fire_id == "0000f67fc2d69fe5c384"] = 11
fires_sdc$cont_day[fires_sdc$fire_id == "0000f67fc2d69fe5c384"] = 29
fires_sdc$burn_duration[fires_sdc$fire_id == "0000f67fc2d69fe5c384"] = 92

# Cedar fire was definitely not contained the same day it started. Contained on 206-10-01
# 000098483297cad8ed1f
# http://cdfdata.fire.ca.gov/incidents/incidents_details_info?incident_id=1392
fires_sdc$cont_month[fires_sdc$fire_id == "000098483297cad8ed1f"] = 10
fires_sdc$cont_day[fires_sdc$fire_id == "000098483297cad8ed1f"] = 1
fires_sdc$burn_duration[fires_sdc$fire_id == "000098483297cad8ed1f"] = 46

# Round Fire in 2015 was 2015-02-06 to 2015-02-12; not just 2 days
# 0000b2e869b63ddf6a4f
# http://cdfdata.fire.ca.gov/incidents/incidents_details_info?incident_id=1073
fires_sdc$alarm_day[fires_sdc$fire_id == "0000b2e869b63ddf6a4f"] = 6
fires_sdc$cont_day[fires_sdc$fire_id == "0000b2e869b63ddf6a4f"] = 12
fires_sdc$burn_duration[fires_sdc$fire_id == "0000b2e869b63ddf6a4f"] = 6 

# Containment fire day estimated for Chilcoot Fire? Include?
# 0000af58f72ef9d1239b

# wfu <-
#   fires_sdc %>% 
#   dplyr::filter(objective == "wfu")
# 
# quantile(wfu$burn_duration, prob = c(0.01, 0.05, 0.10, 0.25, 0.5), na.rm = TRUE)

ia <-
  fires_sdc %>% 
  dplyr::filter(objective == "suppression") %>% 
  dplyr::filter(!is.na(burn_duration) & (burn_duration >= 0 & burn_duration < 365)) %>% 
  dplyr::mutate(survived_ia = ifelse(burn_duration > 1, yes = 1, no = 0))

ia %>% 
  st_drop_geometry() %>% 
  group_by(survived_ia) %>% 
  summarize(n = n(),
            elev = mean(elevation, na.rm = TRUE),
            size = median(area_ha))


fm1 <- glm(survived_ia ~ scale(prefire_ndvi) + 
             scale(nbhd_sd_ndvi_1) +
             scale(prefire_erc), 
           data = ia, 
           family = binomial())

summary(fm1)

fm2 <- glm(survived_ia ~ scale(prefire_ndvi) + 
             scale(nbhd_sd_ndvi_1) +
             scale(prefire_erc) +
             scale(earlyfire_vs), 
           data = ia, 
           family = binomial())

summary(fm2)

hist(fires_compare$simultaneous_fires)

fires_compare %>% group_by(cause) %>% tally()
fires_compare %>% filter(objective == "suppression" & cause == 18)

fm2 <- glm(survived_ia ~ scale(prefire_ndvi) + 
             scale(nbhd_sd_ndvi_1) +
             scale(prefire_erc) +
             scale(earlyfire_vs) +
             scale(simultaneous_fires), 
           data = fires_compare %>% 
             dplyr::filter(objective == "suppression") %>% 
             dplyr::filter(cause != 18), 
           family = binomial())
summary(fm2)


fm2b <- glm(survived_ia ~ scale(prefire_ndvi) + 
              scale(nbhd_sd_ndvi_1) +
              scale(prefire_erc) +
              scale(earlyfire_vs) +
              scale(simultaneous_fires), 
            data = fires_compare %>% 
              dplyr::filter(objective == "suppression") %>% 
              dplyr::filter(cause != 18), 
            family = binomial())
summary(fm2b)

fm2c <- glm(survived_ia ~ scale(prefire_ndvi) + 
              scale(nbhd_sd_ndvi_1) +
              scale(prefire_erc) +
              scale(earlyfire_vs),
            data = fires_compare %>% 
              dplyr::filter(objective == "suppression") %>% 
              dplyr::filter(cause != 18) %>% 
              dplyr::filter(simultaneous_fires < 21), 
            family = binomial())
summary(fm2c)

fm3 <- glm(escaped_rx ~ scale(prefire_ndvi) + 
             scale(nbhd_sd_ndvi_1) +
             scale(prefire_erc) +
             scale(earlyfire_vs) +
             scale(simultaneous_fires), 
           data = fires_compare %>% 
             dplyr::filter(objective == "wfu" | cause == 18) %>% 
             dplyr::mutate(escaped_rx = ifelse(cause == 18, yes = 1, no = 0)), 
           family = binomial())
summary(fm3)

