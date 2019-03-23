# Purpose: subset the Fire Occurrence Dataset from Short (2015) to just Sierra Nevada fires

library(sf)

layers <- st_layers("data/data_raw/features/RDS-2013-0009.4_GDB/Data/FPA_FOD_20170508.gdb")
fod <- st_read("data/data_raw/features/RDS-2013-0009.4_GDB/Data/FPA_FOD_20170508.gdb")
sn <- st_read("data/data_output/jepson_sierra-nevada-ecoregion/jepson_sierra-nevada-ecoregion.shp") %>% 
  st_transform(3310)

ca_fod <- 
  fod %>% 
  dplyr::filter(STATE == "CA") %>% 
  st_transform(3310)

sn_fod <- ca_fod[sn, ]

ac_to_m2 <- function(ac) {return(ac * (66 * 660) * (0.0254 * 12 * 0.0254 * 12))}
ac_to_r <- function(ac) {return(sqrt(ac_to_m2(ac) / pi))}

sn_fod_buffer <- st_buffer(sn_fod, dist = (ac_to_r(sn_fod$FIRE_SIZE)))
plot(sn_fod_buffer$Shape)

st_write(obj = sn_fod, dsn = "data/data_output/sierra-nevada-short-fod.gpkg")
# 
# st_write(obj = st_transform(sn_fod, 4326), dsn = "data/data_output/sierra-nevada-short-fod-4326.geoJSON")


table(sn_fod$FIRE_SIZE_CLASS) / nrow(sn_fod)
cumsum(table(sn_fod$FIRE_SIZE_CLASS) / nrow(sn_fod))

fire_size_ecdf <- ecdf(sn_fod$FIRE_SIZE)

# Percent of fire events that are smaller than a single Landsat pixel (30m x 30m or about 0.222 acres)
fire_size_ecdf(0.222) # 0.5437291
# Percent of fire events that are smaller in size than the 4 hectare cutoff for remote-sensing-resistance dataset
fire_size_ecdf(10) # 0.9410107
# Percent of fire events that are smaller in size than the 80 hectare cutoff for the Region 5 Geospatial dataset
fire_size_ecdf(200) # 0.9860923
# Percent of fire events that are smaller in size than the 1000 acre cutoff for the MTBS dataset
fire_size_ecdf(1000) # 0.9942746

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