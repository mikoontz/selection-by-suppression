# Purpose: calculate the SDC (stand-replacing decay coefficient; Stevens et al. (2017), Collins et al. (2017))

library(tidyverse)
library(sf)
library(purrr)
library(lubridate)

metadata <- read_csv("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv")
# metadata with original FRAP perimeter attached
metadata_sf <- st_read("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.geoJSON",  stringsAsFactors = FALSE)

fires_by_severity <- st_read("data/data_output/polygonized-fires-by-severity-classes.gpkg", stringsAsFactors = FALSE)
fires <- read_csv("data/data_output/polygonized-fires-by-fire.csv")

# Length of time the fire was burning
metadata <- 
  metadata %>% 
  dplyr::mutate(burn_duration = as.numeric(ymd(paste(cont_year, cont_month, cont_day, sep = "-")) - ymd(paste(alarm_year, alarm_month, alarm_day, sep = "-")))) %>% 
  dplyr::mutate(objective = ifelse(is.na(objective),
                                   yes = NA,
                                   no = ifelse(objective == 1,
                                               yes = "suppression",
                                               no = "wfu")))
# Length of time the fire was burning
metadata_sf <- 
  metadata_sf %>% 
  dplyr::mutate(burn_duration = as.numeric(ymd(paste(cont_year, cont_month, cont_day, sep = "-")) - ymd(paste(alarm_year, alarm_month, alarm_day, sep = "-")))) %>% 
  dplyr::mutate(objective = ifelse(is.na(objective),
                                   yes = NA,
                                   no = ifelse(objective == 1,
                                               yes = "suppression",
                                               no = "wfu")))

fires <-
  metadata_sf %>%
  left_join(fires)

fires_by_severity <-
  fires_by_severity %>% 
  dplyr::left_join(metadata, by = "fire_id")

hi_sev_fires <- 
  fires_by_severity %>% 
  dplyr::filter(sev_cat == 3,
                is.na(burn_duration) | burn_duration > 0) %>% 
  dplyr::left_join(fires %>% 
                     st_drop_geometry() %>% 
                     dplyr::select(fire_id, prop_ypmc),
                   by = "fire_id")

# function to approximate the stand replacing decay constant --------------

calculate_sdc <- function(poly) {
  
  n_samples <- max(floor(poly$sev_cat_area_m2 / 50000), 500)
  pts <- st_sample(poly, size = n_samples)
  poly_to_linestring <- st_cast(poly, "MULTILINESTRING")
  
  dist_to_edge <- as.numeric(st_distance(x = pts, y = poly_to_linestring)[, 1])
  sorted_dist_to_edge <- sort(dist_to_edge)
  dist_to_edge_cdf <- ecdf(sorted_dist_to_edge)
  pr_dist_to_edge <- dist_to_edge_cdf(sorted_dist_to_edge)
  
  plot(sorted_dist_to_edge, 1 - pr_dist_to_edge, main = poly$fire_id, xlim = c(0, 100))
  
  fm1 <- nls(formula = (1 - pr_dist_to_edge) ~ 1 / (10 ^ (sdc * sorted_dist_to_edge)), start = list(sdc = 0.1))
  sdc <- coef(fm1)
  
  return(sdc)
  
}

(start <- Sys.time())
hi_sev_fires$sdc <- map_dbl(1:nrow(hi_sev_fires), .f = function(x) {calculate_sdc(hi_sev_fires[x, ])})
(Sys.time() - start)

readr::write_csv(hi_sev_fires %>% st_drop_geometry(), "data/data_output/high-severity-fires-with-sdc.csv")

ggplot(hi_sev_fires, aes(x = area_ha, y = sdc, lty = objective, color = elevation)) +
  geom_point() +
  facet_grid(~ objective) +
  scale_x_log10() +
  theme_bw() +
  scale_color_viridis_c() +
  labs(color = "Prefire 100-hr\nfuel moisture") +
  geom_smooth()
