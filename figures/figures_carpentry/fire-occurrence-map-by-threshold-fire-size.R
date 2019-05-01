# Purpose: create images of number of fires of different threshold sizes

library(tidyverse)
library(sf)
library(tmap)

sn <- st_read("data/data_output/jepson_sierra-nevada-ecoregion/jepson_sierra-nevada-ecoregion.shp") %>% st_transform(3310)

short <- st_read("data/data_output/sierra-nevada-ypmc-short-fod/sierra-nevada-ypmc-short-fod.shp") %>% st_transform(3310)

nrow(short %>% dplyr::filter(fire_size > 400 * 2.5))
nrow(short %>% dplyr::filter(fire_size > 80 * 2.5))
nrow(short %>% dplyr::filter(fire_size > 4 * 2.5))
nrow(short %>% dplyr::filter(fire_size < 0.09 * 2.5))
nrow(short)

# MTBS threshold ----------------------------------------------------------

mtbs <-
  tm_shape(sn) +
  tm_polygons() +
  tm_shape(short %>% dplyr::filter(fire_size > 400 * 2.5)) +
  tm_dots() +
  tm_ylab(text = "Latitude (km)", space = 2) +
  tm_xlab(text = "Longitude (km)", space = 1.5) +
  tm_grid(alpha = 0.05, labels.inside.frame = FALSE, labels.format = list(fun = function(x) as.character(x / 1000))) +
  tm_compass(type = "arrow", position = c(0.1, 0.05))

tmap_save(tm = mtbs, filename = "figures/fod-map-by-fire-size-threshold-mtbs.png", dpi = 600, height = 5.36, width = 3.75, units = "in")

# Region 5 threshold ------------------------------------------------------

r5 <-
  tm_shape(sn) +
  tm_polygons() +
  tm_shape(short %>% dplyr::filter(fire_size > 80 * 2.5)) +
  tm_dots() +
  tm_ylab(text = "Latitude (km)", space = 2) +
  tm_xlab(text = "Longitude (km)", space = 1.5) +
  tm_grid(alpha = 0.05, labels.inside.frame = FALSE, labels.format = list(fun = function(x) as.character(x / 1000))) +
  tm_compass(type = "arrow", position = c(0.1, 0.05))

tmap_save(tm = r5, filename = "figures/fod-map-by-fire-size-threshold-r5.png", dpi = 600, height = 5.36, width = 3.75, units = "in")

# Koontz et al 2019 threshold ---------------------------------------------

koontz <-
  tm_shape(sn) +
  tm_polygons() +
  tm_shape(short %>% dplyr::filter(fire_size > 4 * 2.5)) +
  tm_dots() +
  tm_ylab(text = "Latitude (km)", space = 2) +
  tm_xlab(text = "Longitude (km)", space = 1.5) +
  tm_grid(alpha = 0.05, labels.inside.frame = FALSE, labels.format = list(fun = function(x) as.character(x / 1000))) +
  tm_compass(type = "arrow", position = c(0.1, 0.05))

tmap_save(tm = koontz, filename = "figures/fod-map-by-fire-size-threshold-koontz.png", dpi = 600, height = 5.36, width = 3.75, units = "in")

# Landsat pixel threshold -------------------------------------------------

landsat <-
  tm_shape(sn) +
  tm_polygons() +
  tm_shape(short %>% dplyr::filter(fire_size > 0.09 * 2.5)) +
  tm_dots() +
  tm_ylab(text = "Latitude (km)", space = 2) +
  tm_xlab(text = "Longitude (km)", space = 1.5) +
  tm_grid(alpha = 0.05, labels.inside.frame = FALSE, labels.format = list(fun = function(x) as.character(x / 1000))) +
  tm_compass(type = "arrow", position = c(0.1, 0.05))

tmap_save(tm = landsat, filename = "figures/fod-map-by-fire-size-threshold-landsat.png", dpi = 600, height = 5.36, width = 3.75, units = "in")

# all ---------------------------------------------------------------------

all <-
  tm_shape(sn) +
  tm_polygons() +
  tm_shape(short) +
  tm_dots() +
  tm_ylab(text = "Latitude (km)", space = 2) +
  tm_xlab(text = "Longitude (km)", space = 1.5) +
  tm_grid(alpha = 0.05, labels.inside.frame = FALSE, labels.format = list(fun = function(x) as.character(x / 1000))) +
  tm_compass(type = "arrow", position = c(0.1, 0.05))

tmap_save(tm = all, filename = "figures/fod-map-by-fire-size-threshold-all.png", dpi = 600, height = 5.36, width = 3.75, units = "in")
