# Purpose: polygonize the high severity pixels in each fire into a multipolygon

library(tidyverse)
library(sf)
library(raster)
library(stars)
library(APfun) # to use python gdal_polygonize when stars inexplicably doesn't work
library(doParallel)


metadata <- read_csv("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv")
metadata_sf <- 
  st_read("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON", stringsAsFactors = FALSE) %>% 
  st_transform(3310)

r_list <- list.files("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_rasters/")
band_names <- c('rdnbr', 'prefire_nbr', 'postfire_nbr', 'rdndvi', 'rbr', 'prefire_ndvi', 'postfire_ndvi', 'nbhd_sd_ndvi_1', 'nbhd_mean_ndvi_1', 'nbhd_sd_ndvi_2', 'nbhd_mean_ndvi_2', 'nbhd_sd_ndvi_3', 'nbhd_mean_ndvi_3', 'nbhd_sd_ndvi_4', 'nbhd_mean_ndvi_4', 'date', 'ordinal_day', 'alarm_year', 'alarm_month', 'alarm_day', 'longitude', 'latitude', 'ypmc', 'slope', 'aspect', 'topo_roughness_1', 'topo_roughness_2', 'topo_roughness_3', 'topo_roughness_4', 'elevation', 'B1_prefire', 'B2_prefire', 'B3_prefire', 'B4_prefire', 'B5_prefire', 'B6_prefire', 'B7_prefire', 'B1_postfire', 'B2_postfire', 'B3_postfire', 'B4_postfire', 'B5_postfire', 'B6_postfire', 'B7_postfire', 'prefire_erc', 'prefire_fm100', 'prefire_vpd', 'earlyfire_vs', 'earlyfire_hdw')

# low_sev_lower_rbr_threshold <- best_model$low_sev
low_sev_lower_rbr_threshold <- 0.04509658
# mod_sev_lower_rbr_threshold <- best_model$mod_sev
mod_sev_lower_rbr_threshold <- 0.1125589
# hi_sev_lower_rbr_threshold <- best_model$hi_sev
hi_sev_lower_rbr_threshold <- 0.2823349

cores <- detectCores()
cl <- makeCluster(cores[1] - 2) #not to overload your computer
registerDoParallel(cl)

(start <- Sys.time())
polygonized_fires <- foreach(i = seq_along(r_list), .combine = rbind) %dopar% {
  library(tidyverse)
  library(sf)
  library(raster)
  library(stars)
  library(APfun)
  
  current_fire <- r_list[i]
  fire_metadata <- str_split(current_fire, pattern = "_")[[1]]
  fire_ee_id <- fire_metadata[3]
  
  r <- raster::brick(paste0("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_rasters/", current_fire))
  names(r) <- band_names
  
  rbr <- r[["rbr"]]
  sev_cats <- rbr
  sev_cats[rbr[] < low_sev_lower_rbr_threshold] <- 0
  sev_cats[rbr[] >= low_sev_lower_rbr_threshold & rbr[] < mod_sev_lower_rbr_threshold] <- 1
  sev_cats[rbr[] >= mod_sev_lower_rbr_threshold & rbr[] < hi_sev_lower_rbr_threshold] <- 2
  sev_cats[rbr[] >= hi_sev_lower_rbr_threshold] <- 3
  
  rstar <- try(stars::st_as_stars(sev_cats) %>% st_as_sf() %>% dplyr::rename(sev_cat = rbr))
  if (class(rstar)[1] == "try-error") {
    rstar <- try(APpolygonize(sev_cats) %>% st_as_sf() %>% dplyr::rename(sev_cat = DN))
    if (class(rstar)[1] == "try-error") {
      rstar <- raster::rasterToPolygons(sev_cats) %>% st_as_sf() %>% dplyr::rename(sev_cat = rbr)
    }
  }
  
  rstar <-
    rstar %>% 
    dplyr::mutate(patch_area = as.numeric(st_area(.))) %>% 
    group_by(sev_cat) %>% 
    summarize(n_patches = n(),
              # patch_areas_m2 = list(patch_area),
              max_patch_area_m2 = max(patch_area)) %>% 
    dplyr::mutate(fire_id = fire_ee_id) %>% 
    dplyr::mutate(sev_cat_area_m2 = as.numeric(st_area(.))) %>% 
    dplyr::mutate(sev_cat_prop = as.numeric(st_area(.) / sum(st_area(.)))) %>% 
    dplyr::mutate(sev_cat_prop_exclude_unchanged = ifelse(sev_cat == 0, yes = NA, no = sev_cat_area_m2 / sum(sev_cat_area_m2[sev_cat != 0]))) %>% 
    sf::st_cast(to = "MULTIPOLYGON")
  
  if (nrow(rstar) > 0) {
    # amount in each category that burned in YPMC
    # extracting this on its own because we want the sum of these pixels, not the mean
    ypmc <- velox::velox(r[["ypmc"]])
    
    print(class(st_geometry(rstar)))
    
    ypmc_df <- 
      ypmc$extract(sp = st_geometry(rstar), fun = sum) %>% 
      as_tibble() %>% 
      set_names("ypmc_direct_pixel_count")
    
    rstar <-
      rstar %>% 
      bind_cols(ypmc_df)
    
    # layers for which we want the mean value in each severity category
    # take a weighted mean based on these values and the representation
    # of each severity category within the fire to get the "whole fire"
    # value
    # pixel values of vegetation variables for each category
    veg_climate_elev <- velox::velox(r[[c("prefire_ndvi", "nbhd_sd_ndvi_1", "prefire_fm100", "prefire_vpd", "prefire_erc", "earlyfire_vs", "earlyfire_hdw", "elevation")]])
    
    veg_climate_elev_df <-
      veg_climate_elev$extract(rstar, fun = mean) %>% 
      as_tibble() %>% 
      set_names(c("prefire_ndvi", "nbhd_sd_ndvi_1", "prefire_fm100", "prefire_vpd", "prefire_erc", "earlyfire_vs", "earlyfire_hdw", "elevation"))
    
    rstar <-
      rstar %>% 
      bind_cols(veg_climate_elev_df) %>% 
      dplyr::select(fire_id, sev_cat, everything())
    
    # polygonized_fires[[i]] <- rstar
    # print(paste0("...", i, " of ", length(r_list), " fires polygonized. Thanks stars!"))
  } else {
    
    extra_df <-
      matrix(NA, nrow = 0, ncol = 9) %>% 
      as_tibble(.name_repair = "minimal") %>% 
      set_names(c("ypmc_direct_pixel_count", "prefire_ndvi", "nbhd_sd_ndvi_1", "prefire_fm100", "prefire_vpd", "prefire_erc", "earlyfire_vs", "earlyfire_hdw", "elevation")) 
    
    rstar <-
      rstar %>% 
      bind_cols(extra_df) %>% 
      dplyr::select(fire_id, sev_cat, everything())
  }
  
  return(rstar)
}

#stop cluster
stopCluster(cl)

(end <- Sys.time())
print(end - start)

polygonized_fires
st_write(polygonized_fires, "data/data_output/polygonized-fires-by-severity-classes.gpkg", delete_dsn = TRUE)
write_csv(polygonized_fires %>% st_drop_geometry(), "data/data_output/polygonized-fires-by-severity-classes.csv")

pf_summarized_by_fire <-
  polygonized_fires %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(fire_id) %>% 
  dplyr::summarize(fire_area_m2 = sum(sev_cat_area_m2),
                   fire_area_m2_exclude_unchanged = sum(sev_cat_area_m2[sev_cat != 0]),
                   prop_ypmc = (sum(ypmc_direct_pixel_count) * 30 * 30) / fire_area_m2,
                   prefire_ndvi = stats::weighted.mean(x = prefire_ndvi, w = sev_cat_prop),
                   nbhd_sd_ndvi_1 = stats::weighted.mean(x = nbhd_sd_ndvi_1, w = sev_cat_prop),
                   prefire_fm100 = stats::weighted.mean(x = prefire_fm100, w = sev_cat_prop),
                   prefire_vpd = stats::weighted.mean(x = prefire_vpd, w = sev_cat_prop),
                   prefire_erc = stats::weighted.mean(x = prefire_erc, w = sev_cat_prop),
                   earlyfire_vs = stats::weighted.mean(x = earlyfire_vs, w = sev_cat_prop),
                   earlyfire_hdw = stats::weighted.mean(x = earlyfire_hdw, w = sev_cat_prop),
                   elevation = stats::weighted.mean(x = elevation, w = sev_cat_prop))

# pf_summarized_by_fire <- 
#   metadata_sf %>% 
#   left_join(pf_summarized_by_fire) %>% 
#   dplyr::filter(!st_is_empty(.))

glimpse(pf_summarized_by_fire)
write_csv(pf_summarized_by_fire, "data/data_output/polygonized-fires-by-fire.csv")


# all_data <- 
#   metadata %>%
#   dplyr::left_join(polygonized_fires, by = "fire_id") %>% 
#   dplyr::left_join(total_burned_area, by = "fire_id") %>% 
#   dplyr::mutate(objective = ifelse(!is.na(objective), 
#                                    yes = ifelse(objective == 1, 
#                                                 yes = "suppression",
#                                                 no = "wfu"),
#                                    no = objective))
# 
# all_data %>% 
#   dplyr::filter(sev_cat == 2) %>% 
#   dplyr::filter(!is.na(objective)) %>% 
#   # dplyr::filter(fire_area_m2_exclude_unchanged > 400 * 10000) %>%
#   ggplot(aes(x = fire_area_m2_exclude_unchanged / 10000, y = sev_cat_prop_exclude_unchanged, color = as.factor(objective))) +
#   geom_point() +
#   scale_x_log10() +
#   geom_smooth(method = "lm")
# 
# all_data %>% 
#   dplyr::filter(sev_cat == 2) %>% 
#   dplyr::filter(!is.na(objective)) %>% 
#   # dplyr::filter(fire_area_m2 > 400 * 10000) %>% 
#   ggplot(aes(x = fire_area_m2 / 10000, y = sev_cat_prop, color = as.factor(objective))) +
#   geom_point() +
#   scale_x_log10() +
#   geom_smooth(method = "gam") +
#   geom_vline(xintercept = 80) +
#   geom_vline(xintercept = 400)


