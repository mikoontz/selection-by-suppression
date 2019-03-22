# Purpose: model the combined effects of suppression, regional climate, elevation, vegetation, fire size, stand replacing decay coefficient

library(tidyverse)
library(randomForest)
library(lavaan)

fires <- read.csv("data/data_output/high-severity-fires-with-sdc.csv")

fm1 <- randomForest(sdc ~ objective + prefire_vpd + elevation + prefire_ndvi + area_ha, data = fires, importance = TRUE, proximity = TRUE)

fm2 <- lm(prefire_vpd ~ objective*area_ha, data = fires)

fm2 <- lm(log(fires$sdc, base = 10) ~ objective + prefire_vpd + elevation + prefire_ndvi + area_ha, data = fires)

