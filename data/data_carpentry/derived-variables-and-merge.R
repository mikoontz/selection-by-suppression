# Add all derived variables to the data frame including:
# 1) burn duration (plus corrections for wildly inaccurate ones)
# 2) whether suppression fires survived initial attack
# 3) whether fires were began during the June 22, 2008 lightning storms
# 4) merge with metadata
# 5) merge with stand replacing decay coefficient data
# 6) merge with fire level data
# 7) merge with severity class per fire level data

library(tidyverse)
library(sf)

# Read metadata file
md <- read_csv("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv")
# md_sf <- st_read("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON") %>% st_transform(3310)
sdc <- 
  read_csv("data/data_output/high-severity-fires-with-sdc.csv") %>% 
  dplyr::select(fire_id, sdc)

fires_by_fire <- read_csv("data/data_output/polygonized-fires-by-fire.csv")
fires_by_severity <- read.csv("data/data_output/polygonized-fires-by-severity-classes.csv")

# short_fires <- st_read("data/data_output/sierra-nevada-ypmc-short-fod.gpkg") %>% dplyr::rename(fod_id = FOD_ID)
# ee_short_fires <- 
#   st_read("data/data_output/ee_short-burning-conditions_48-day-window_L4578_bicubic.geojson") %>% 
#   st_drop_geometry() %>% 
#   dplyr::select(fod_id, alarm_date, cont_date, fire_size, prop_ypmc, elev, earlyFfm100, fm100, hdw, vs, vpd, RBR, preFire_ndvi, het_ndvi_1, focal_mean_ndvi_1)

# Make the different severity level information a per-fire variable
sev_cat_prop <-
  fires_by_severity %>% 
  dplyr::select(fire_id, sev_cat, sev_cat_prop) %>% 
  tidyr::spread(key = sev_cat, value = sev_cat_prop, fill = 0) %>% 
  dplyr::rename(prop_unchanged = `0`,
                prop_low = `1`,
                prop_mod = `2`,
                prop_high = `3`)

# Add a burn duration to the per-fire metadata information using the containment date 
fires <-
  md %>% 
  dplyr::mutate(alarm_date = ymd(paste(alarm_year, alarm_month, alarm_day, sep = "-"))) %>% 
  dplyr::mutate(cont_date = ymd(paste(cont_year, cont_month, cont_day, sep = "-"))) %>% 
  dplyr::mutate(burn_duration = as.numeric(cont_date - alarm_date)) %>% 
  left_join(fires_by_fire, by = "fire_id") %>% 
  dplyr::left_join(sev_cat_prop, by = "fire_id") %>% 
  dplyr::filter(!is.na(objective)) %>% 
  dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))

# Join the SDC data with the per-fire metadata + derived variables
fires_sdc <-
  fires %>% 
  left_join(sdc, by = "fire_id")

# Fix a few of the extreme outliers
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

# Determine whether fires survived initial attack (did they burn for longer than 1 day?)
ia <-
  fires_sdc %>% 
  dplyr::filter(objective == "suppression") %>% 
  dplyr::filter(!is.na(burn_duration) & (burn_duration >= 0 & burn_duration < 365)) %>% 
  dplyr::mutate(survived_ia = ifelse(burn_duration > 1, yes = 1, no = 0))

# Which fires ignited June 21-22, 2008?
june2008fires_id <-
  ia %>% 
  dplyr::filter(alarm_year == 2008, alarm_month == 6, alarm_day %in% c(21, 22)) %>% 
  pull(fire_id)

# Merge all these data together
fires_compare <-
  fires_sdc %>% 
  dplyr::filter(!is.na(burn_duration) & (burn_duration >= 0 & burn_duration < 365)) %>% 
  dplyr::mutate(june2008fire = ifelse(fire_id %in% june2008fires_id, yes = "yes", no = "no")) %>% 
  dplyr::left_join(ia %>% dplyr::select(fire_id, survived_ia)) %>% 
  dplyr::mutate(comparison_cat = case_when(june2008fire == "yes" ~ "june2008fire",
                                           objective == "wfu" ~ "wfu",
                                           objective == "suppression" & survived_ia == 1 ~ "survived_ia",
                                           objective == "suppression" & survived_ia == 0 ~ "died_ia"
  ))

# Determine number of concurrently burning fires on the discovery day of each fire

fires_compare$simultaneous_fires <- NA

for (i in seq_along(1:nrow(fires_compare))) {
    alarm_date <- fires_compare$alarm_date[i]
    fires_compare$simultaneous_fires[i] <- length(which((fires_compare$cont_date >= alarm_date) & (fires_compare$alarm_date <= alarm_date)))
}

write_csv(fires_compare, path = "data/data_output/merged-data-and-derived-variables.csv")
