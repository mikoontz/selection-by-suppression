library(raster)
library(fasterize)
library(sf)

# Latitudinal extent of the YPMC in this study
ypmc <- raster::raster("data/data_output/frid-ypmc.tif")
e <- raster::extent(ypmc)
(e@ymax - e@ymin) / 1000

# Elevational extent of the YPMC in this study
nsn <- 
  st_read("data/data_raw/features/FRID-sierra-nevada/FRID_NorthSierra17_1.gdb") %>% 
  dplyr::filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))
ssn <- 
  st_read("data/data_raw/features/FRID-sierra-nevada/FRID_SouthSierra17_1.gdb") %>% 
  dplyr::filter(PFR %in% c("Dry mixed conifer", "Moist mixed conifer", "Yellow pine"))

ypmc_vector <- rbind(nsn, ssn)
ypmc_vector <-
  ypmc_vector %>% 
  st_transform(4326)

sn <- sf::st_read("data/data_output/jepson_sierra-nevada-ecoregion/jepson_sierra-nevada-ecoregion.shp")
elev <- raster::getData(name = "alt", country = "USA", download = FALSE)
ca_elev <- 
  elev[[1]] %>% 
  raster::crop(sn)

# template raster in epsg3310
r <- raster(sn, res = res(ca_elev))

ypmc_raster <- fasterize(sf = ypmc_vector, raster = ca_elev, field = NULL, background = 0)

ypmc_elev <- ypmc_raster * ca_elev

ypmc_elev[ypmc_elev[] == 0] <- NA
range(values(ypmc_elev), na.rm = TRUE)

# Interquartile range is 1273 to 1945
summary(values(ypmc_elev))
quantile(values(ypmc_elev), prob = c(0.025, 0.1, 0.9, 0.975), na.rm = TRUE)
diff(range(values(ypmc_elev), na.rm = TRUE))
