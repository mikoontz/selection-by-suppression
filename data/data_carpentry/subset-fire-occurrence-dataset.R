# Purpose: subset the Fire Occurrence Dataset from Short (2015) to just Sierra Nevada fires

library(sf)
library(raster)
library(fasterize)
library(viridis)
library(velox)

layers <- st_layers("data/data_raw/features/RDS-2013-0009.4_GDB/Data/FPA_FOD_20170508.gdb")
fod <- st_read("data/data_raw/features/RDS-2013-0009.4_GDB/Data/FPA_FOD_20170508.gdb", layer = "Fires")
# nwcg <- st_read("data/data_raw/features/RDS-2013-0009.4_GDB/Data/FPA_FOD_20170508.gdb", layer = "NWCG_UnitIDActive_20170109")

sn <- st_read("data/data_output/jepson_sierra-nevada-ecoregion/jepson_sierra-nevada-ecoregion.shp") %>% 
  st_transform(3310)

ypmc_raster <- raster::raster("data/data_output/frid-ypmc.tif")


# helper functions to convert acres to metric -----------------------------

ac_to_m2 <- function(ac) {return(ac * (66 * 660) * (0.0254 * 12 * 0.0254 * 12))}
ac_to_r <- function(ac) {return(sqrt(ac_to_m2(ac) / pi))}


# read the Sierra Nevada fod ----------------------------------------------


if (!file.exists("data/data_output/sierra-nevada-short-fod.gpkg")) {
  ca_fod <- 
    fod %>% 
    dplyr::filter(STATE == "CA") %>% 
    st_transform(3310)
  
  sn_fod <- ca_fod[sn, ]
  
  st_write(obj = sn_fod, dsn = "data/data_output/sierra-nevada-short-fod.gpkg")
} else {
  sn_fod <- st_read("data/data_output/sierra-nevada-short-fod.gpkg")
}

# buffer each point by reported fire size ---------------------------------
sn_fod_buffer <- st_buffer(sn_fod, dist = (ac_to_r(sn_fod$FIRE_SIZE)))

# create higher resolution version of ypmc image --------------------------
ypmc_hi_res <- raster::disaggregate(x = ypmc_raster, fact = 2)


# convert to velox object -------------------------------------------------
vx <- velox(ypmc_hi_res)

# percent cover of ypmc in each polygon -----------------------------------
pct_ypmc <- vx$extract(sp = sn_fod_buffer, fun = mean)
sn_fod_buffer$prop_ypmc <- pct_ypmc


# convert NA prop_ypmc back to points -------------------------------------
unk_prop_ypmc <- 
  sn_fod_buffer[is.na(sn_fod_buffer$prop_ypmc), ] %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(3310)


# extract the ypmc pixels by points (all small fires) ---------------------
pct_ypmc_for_unk <- vx$extract_points(sp = unk_prop_ypmc)
sn_fod_buffer[is.na(sn_fod_buffer$prop_ypmc), "prop_ypmc"] <- pct_ypmc_for_unk


# turn sf object back to points -------------------------------------------
sn_ypmc_fod <- 
  sn_fod_buffer %>% 
  dplyr::filter(prop_ypmc > 0) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>% 
  st_transform(3310)

sn_ypmc_fod_shp <- 
  sn_fod_buffer %>% 
  dplyr::filter(prop_ypmc > 0) %>% 
  st_drop_geometry() %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE) %>% 
  dplyr::mutate(alarm_date = ymd(DISCOVERY_DATE)) %>% 
  dplyr::mutate(cont_date = ymd(CONT_DATE)) %>% 
  dplyr::rename(fod_id = FOD_ID,
                fpa_id = FPA_ID,
                fire_size = FIRE_SIZE,
                cause_code = STAT_CAUSE_CODE,
                cause_desc = STAT_CAUSE_DESCR) %>% 
  dplyr::select(fod_id, fpa_id, fire_size, alarm_date, cont_date, prop_ypmc)

st_write(obj = sn_ypmc_fod, dsn = "data/data_output/sierra-nevada-ypmc-short-fod.gpkg", delete_dsn = TRUE)
st_write(obj = sn_ypmc_fod_shp, dsn = "data/data_output/sierra-nevada-ypmc-short-fod/sierra-nevada-ypmc-short-fod.shp", delete_dsn = TRUE)


# Some summaries of the Sierra Nevada data --------------------------------
# Still need to do for the ypmc

table(sn_ypmc_fod$FIRE_SIZE_CLASS) / nrow(sn_ypmc_fod)
cumsum(table(sn_ypmc_fod$FIRE_SIZE_CLASS) / nrow(sn_ypmc_fod))

fire_size_ecdf <- ecdf(sn_ypmc_fod$FIRE_SIZE)

# Percent of fire events that are smaller than a single Landsat pixel (30m x 30m or about 0.222 acres)
fire_size_ecdf(0.222) # 0.6143517
# Percent of fire events that are smaller in size than the 4 hectare cutoff for remote-sensing-resistance dataset
fire_size_ecdf(10) # 0.9488143
# Percent of fire events that are smaller in size than the 80 hectare cutoff for the Region 5 Geospatial dataset
fire_size_ecdf(200) # 0.9793656
# Percent of fire events that are smaller in size than the 1000 acre cutoff for the MTBS dataset
fire_size_ecdf(1000) # 0.9887281

total_area_burned <- sum(sn_fod$FIRE_SIZE)

sn_fod_summary <-
  sn_fod %>% 
  dplyr::group_by(FIRE_SIZE_CLASS) %>% 
  dplyr::summarize(area_burned = sum(FIRE_SIZE),
                   prop_area_burned = area_burned / total_area_burned) %>% 
  dplyr::mutate(cumulative_sum_area = cumsum(area_burned),
                cumulative_sum_prop = cumsum(prop_area_burned),
                prop_greater = 1 - cumulative_sum_prop)

sn_fod_summary

plot(x = log(sort(sn_fod$FIRE_SIZE), base = 10), y = log(1 - fire_size_ecdf(sort(sn_fod$FIRE_SIZE)), base = 10), yaxt = "n", ylab = "Probability of a fire being this size or larger", xaxt = "n", xlab = "Fire size in acres")
axis(side = 2, at = log(c(1.0, 0.5, 0.1, 0.05, 0.01, 0.001, 0.0001), base = 10), labels = c("100%", "50%", "10%", "5%", "1.0%", "0.1%", "0.01%"), las = 1)
axis(side = 1, at = -2:5, labels = 10^(-2:5))
abline(v = log(c(0.222, 10, 200, 1000), base = 10), col = 2:5)
abline(h = log(1 - fire_size_ecdf(c(0.222, 10, 200, 1000)), base = 10), col = 2:5)
legend("topright", legend = c("One Landsat pixel", "remote-sensing-resistance dataset cutoff", "USFS Region 5 Geospatial dataset cutoff", "MTBS dataset cutoff"), col = 2:5, lwd = 3, lty = 1)

plot(x = log(sort(sn_fod$FIRE_SIZE[sn_fod$FIRE_SIZE > 0.222]), base = 10), y = log(1 -  ecdf(sn_fod$FIRE_SIZE[sn_fod$FIRE_SIZE > 0.222])(sort(sn_fod$FIRE_SIZE[sn_fod$FIRE_SIZE > 0.222])), base = 10), yaxt = "n", ylab = "Probability of a fire being this size or larger", xaxt = "n", xlab = "Fire size in acres")
axis(side = 2, at = log(c(1.0, 0.5, 0.1, 0.05, 0.01, 0.001, 0.0001), base = 10), labels = c("100%", "50%", "10%", "5%", "1.0%", "0.1%", "0.01%"), las = 1)
axis(side = 1, at = -2:5, labels = 10^(-2:5))
abline(v = log(c(0.222, 10, 200, 1000), base = 10), col = 2:5)
abline(h = log(1 -  ecdf(sn_fod$FIRE_SIZE[sn_fod$FIRE_SIZE > 0.222])(c(0.222, 10, 200, 1000)), base = 10), col = 2:5)
legend("topright", legend = c("One Landsat pixel", "remote-sensing-resistance dataset cutoff", "USFS Region 5 Geospatial dataset cutoff", "MTBS dataset cutoff"), col = 2:5, lwd = 3, lty = 1)