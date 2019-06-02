# Explore the June 21-22, 2008 fires
# Dry lightning storms across the state of CA on June 21-22 which ignited
# lots and lots of fires
# Fire fighting resources stretched thin, so many of these fires burned longer
# than they might otherwise under full suppression

# Many considered to have done good work. What are the severity properties
# of these fires?

library(tidyverse)
library(sf)
library(lubridate)

# Read data
fires <- read_csv("data/data_output/merged-data-and-derived-variables.csv")
wx_per_fire <- 
  read_csv("data/data_output/wx_per_fire.csv") %>% 
  dplyr::select(-`system:index`, -.geo, -alarm_date, -area_ha, -cont_date, -objective, -n_concur, -fire_name, -cause)

fires <-
  fires %>% 
  dplyr::left_join(wx_per_fire, by = "fire_id") %>% 
  dplyr::mutate(doy = as.numeric(alarm_date - ymd(paste(alarm_year, "01-01"))))


# short <- st_read("data/data_output/ee_short-burning-conditions_48-day-window_L4578_bicubic.geojson")
# 
# short_mod <-
#   short %>% 
#   filter(!is.na(cont_date)) %>% 
#   dplyr::rename(cont_date_milli_since_epoch = cont_date) %>% 
#   dplyr::mutate(cont_year = year(milliseconds(cont_date_milli_since_epoch) + ymd("1970-01-01")),
#                 cont_month = month(milliseconds(cont_date_milli_since_epoch) + ymd("1970-01-01")),
#                 cont_day = day(milliseconds(cont_date_milli_since_epoch) + ymd("1970-01-01"))) %>% 
#   dplyr::mutate(cont_date = ymd(paste(cont_year, cont_month, cont_day, sep = "-"))) %>%
#   dplyr::mutate(alarm_date = ymd(paste(alarm_year, alarm_month, alarm_day, sep = "-"))) %>% 
#   dplyr::mutate(burn_duration = as.numeric((cont_date - alarm_date))) %>% 
#   dplyr::filter(burn_duration <= 364)  %>% 
#   dplyr::mutate(dowy = ifelse(test = alarm_date < ymd(paste(alarm_year, "10", "01", sep = "-")), 
#                               yes = 1 + alarm_date - ymd(paste(alarm_year - 1, "10", "01", sep = "-")),
#                               no = 1 + alarm_date - ymd(paste(alarm_year, "10", "01", sep = "-")))) %>% 
#   dplyr::mutate(doy = alarm_date - ymd(paste(alarm_year, "01-01", sep = "-"))) %>% 
#   dplyr::mutate(survived_ia = ifelse(burn_duration > 1, yes = "yes", no = "no")) %>% 
#   dplyr::mutate(survived_ia_code = ifelse(burn_duration > 1, yes = 1, no = 0))
# 
# short_mod$simultaneous_fires <- NA
# 
# for (i in seq_along(1:nrow(short_mod))) {
#   alarm_date <- short_mod$alarm_date[i]
#   short_mod$simultaneous_fires[i] <- length(which((short_mod$cont_date >= alarm_date) & (short_mod$alarm_date <= alarm_date)))
# }
# 
# plot(table(short_mod$burn_duration))

# fires <- 
#   fires %>% 
#   dplyr::mutate(comparison_cat = ifelse(comparison_cat != "wfu" & simultaneous_fires > 20, yes = "june2008fire", no = comparison_cat)) %>% 
#   dplyr::mutate(comparison_cat = ifelse(comparison_cat == "june2008fire", yes = "busy_fire", no = comparison_cat))

fires %>% 
  dplyr::filter(prop_ypmc > 0.5) %>% 
  group_by(comparison_cat) %>% summarize(n = n(),
                                         elev = mean(elevation, na.rm = TRUE),
                                         dowy = mean(dowy),
                                         simultaneous_fires = mean(simultaneous_fires),
                                         burn_duration = mean(burn_duration, na.rm = TRUE),
                                         prefire_ndvi = mean(prefire_ndvi, na.rm = TRUE),
                                         nbhd_sd_ndvi_1 = mean(nbhd_sd_ndvi_1, na.rm = TRUE),
                                         prefire_erc = mean(prefire_erc, na.rm = TRUE),
                                         erc_p10 = median(erc_p10),
                                         erc_p50 = median(erc_p50),
                                         erc_p90 = median(erc_p90),
                                         vpd_p10 = median(vpd_p10),
                                         vpd_p50 = median(vpd_p50),
                                         vpd_p90 = median(vpd_p90),
                                         rmin_p0 = mean(rmin_p0),
                                         rmin_p50 = mean(rmin_p50),
                                         rmin_p100 = mean(rmin_p100),
                                         rmax_p0 = mean(rmax_p0),
                                         rmax_p50 = mean(rmax_p50),
                                         rmax_p100 = mean(rmax_p100),
                                         vs_p10 = median(vs_p10),
                                         vs_p50 = median(vs_p50),
                                         vs_p90 = median(vs_p90),
                                         fm100_p10 = median(fm100_p10),
                                         fm100_p50 = median(fm100_p50),
                                         fm100_p90 = median(fm100_p90),
                                         hdw_p10 = median(vs_p10 * vpd_p10),
                                         hdw_p50 = median(vs_p50 * vpd_p50),
                                         hdw_p90 = median(vs_p90 * vpd_p90),
                                         prefire_fm100 = mean(prefire_fm100, na.rm = TRUE),
                                         prefire_vpd = mean(prefire_vpd, na.rm = TRUE),
                                         earlyfire_hdw = mean(earlyfire_hdw, na.rm = TRUE),
                                         earlyfire_vs = mean(earlyfire_vs, na.rm = TRUE),
                                         prop_high = mean(prop_high, na.rm = TRUE),
                                         max_patch_area_high = mean(max_patch_m2_high, na.rm = TRUE) / 10000,
                                         med_fire_area_ha = median(fire_area_m2, na.rm = TRUE) / 10000,
                                         mean_fire_area_ha = mean(fire_area_m2, na.rm = TRUE) / 10000,
                                         max_fire_area_ha = max(fire_area_m2, na.rm = TRUE) / 10000,
                                         sdc = mean(sdc, na.rm = TRUE)
                                         
  ) %>% 
  as.data.frame()

june2008 <-
  fires %>% 
  dplyr::filter(comparison_cat == "june2008fire") %>% 
  dplyr::mutate(group = cut(dowy, breaks = c(0, 150, 300, 365)))

ggplot(june2008, aes(dowy, y = 1, color = group)) + geom_point()


june2008 %>% group_by(group) %>% summarize(n = n(),
                                             survived_ia = mean(survived_ia),
                                             elev = mean(elevation, na.rm = TRUE),
                                             dowy = mean(dowy),
                                             simultaneous_fires = mean(simultaneous_fires),
                                             burn_duration = mean(burn_duration, na.rm = TRUE),
                                             prefire_ndvi = mean(prefire_ndvi, na.rm = TRUE),
                                             nbhd_sd_ndvi_1 = mean(nbhd_sd_ndvi_1, na.rm = TRUE),
                                             erc_p10 = mean(erc_p10),
                                             erc_p50 = mean(erc_p50),
                                             erc_p90 = mean(erc_p90),
                                             vpd_p10 = mean(vpd_p10),
                                             vpd_p50 = mean(vpd_p50),
                                             vpd_p90 = mean(vpd_p90),
                                             vs_p10 = mean(vs_p10),
                                             vs_p50 = mean(vs_p50),
                                             vs_p90 = mean(vs_p90),
                                             prefire_erc = mean(prefire_erc, na.rm = TRUE),
                                             prefire_fm100 = mean(prefire_fm100, na.rm = TRUE),
                                             prefire_vpd = mean(prefire_vpd, na.rm = TRUE),
                                             earlyfire_hdw = mean(earlyfire_hdw, na.rm = TRUE),
                                             earlyfire_vs = mean(earlyfire_vs, na.rm = TRUE),
                                             prop_high = mean(prop_high, na.rm = TRUE),
                                             max_patch_area_high = mean(max_patch_m2_high, na.rm = TRUE) / 10000,
                                             med_fire_area_ha = median(fire_area_m2, na.rm = TRUE) / 10000,
                                             mean_fire_area_ha = mean(fire_area_m2, na.rm = TRUE) / 10000,
                                             max_fire_area_ha = max(fire_area_m2, na.rm = TRUE) / 10000,
                                             sdc = mean(sdc, na.rm = TRUE)
                                             
) %>% 
  as.data.frame()

unique(busy_fires$alarm_month)
june2008 <- fires %>% dplyr::filter(fire_id %in% june2008fires_id)


preds <- predict(fm1, newdata = june2008, type = "response")


sort(unique(busy_fires$alarm_date))

busy_fires %>% 
  dplyr::select(dowy, alarm_date) %>% 
  dplyr::arrange(alarm_date) %>% 
  as.data.frame()

nrow(june2008)
sum(june2008$fire_area_m2 / 10000 > 80)
sum(june2008$fire_area_m2 / 10000 > 400)

fires %>% 
  dplyr::filter(prop_ypmc > 0.5) %>% 
  ggplot(aes(dowy)) +
  geom_histogram(bins = 365)

ggplot(fires %>% filter(objective == "suppression"), aes(doy, vpd_p10)) + geom_point() + geom_smooth()
