# Purpose: model the combined effects of suppression, regional climate, elevation, vegetation, fire size, stand replacing decay coefficient

# Load libraries
library(tidyverse)
library(sf)
library(lubridate)
library(here)
library(brms)

fires <- read_csv(here::here("data/data_output/merged-data-and-derived-variables.csv"))

ia <-
  fires %>% 
  dplyr::filter(objective == "suppression") %>% 
  dplyr::filter(prop_ypmc > 0.5)

ia %>% 
  group_by(survived_ia) %>% 
  summarize(n = n(),
            elev = mean(elevation, na.rm = TRUE),
            size = median(area_ha))

fm1 <- glm(survived_ia ~ scale(prefire_ndvi) + 
             scale(nbhd_sd_ndvi_1) +
             scale(prefire_erc) +
             scale(earlyfire_vs), 
           data = ia, 
           family = binomial())

summary(fm1)

fm1b <- glm(survived_ia ~ scale(prefire_ndvi) + 
             scale(nbhd_sd_ndvi_1) +
             scale(prefire_erc) +
             scale(earlyfire_vs) +
             scale(simultaneous_fires), 
           data = ia, 
           family = binomial())

summary(fm1b)

fm1b_brms <- brm(survived_ia ~ scale(prefire_ndvi) + 
                   scale(nbhd_sd_ndvi_1) +
                   scale(prefire_erc) +
                   scale(earlyfire_vs) +
                   scale(simultaneous_fires), 
                 data = ia, 
                 family = bernoulli())

fm1b_brms
pp_check(fm1b_brms)
write_rds(fm1b_brms, path = "analyses/analyses_output/selection-by-suppression-model-fit.rds")

# All human-ignited fires
fm2 <- glm(survived_ia ~ scale(prefire_ndvi) + 
              scale(nbhd_sd_ndvi_1) +
              scale(prefire_erc) +
              scale(earlyfire_vs), 
            data = ia %>% 
              dplyr::filter(!is.na(cause) & cause != 1), 
            family = binomial())
summary(fm2)

fm2b <- glm(survived_ia ~ scale(prefire_ndvi) + 
             scale(nbhd_sd_ndvi_1) +
             scale(prefire_erc) +
             scale(earlyfire_vs) +
           scale(simultaneous_fires), 
           data = ia %>% 
             dplyr::filter(!is.na(cause) & cause != 1), 
           family = binomial())
summary(fm2b)


# All lightning-ignited fires
fm3 <- glm(survived_ia ~ scale(prefire_ndvi) + 
             scale(nbhd_sd_ndvi_1) +
             scale(prefire_erc) +
             scale(earlyfire_vs) +
           scale(simultaneous_fires), 
           data = ia %>% 
             dplyr::filter(cause == 1), 
           family = binomial())
summary(fm3)

# Non-prescribed fires; filter out escaped Rx fires
fm4 <- glm(survived_ia ~ scale(prefire_ndvi) + 
              scale(nbhd_sd_ndvi_1) +
              scale(prefire_erc) +
              scale(earlyfire_vs) +
              scale(simultaneous_fires), 
            data = fires %>% 
              dplyr::filter(objective == "suppression") %>% 
              dplyr::filter(cause != 18), 
            family = binomial())
summary(fm4)

# fm2c <- glm(survived_ia ~ scale(prefire_ndvi) + 
#               scale(nbhd_sd_ndvi_1) +
#               scale(prefire_erc) +
#               scale(earlyfire_vs),
#             data = fires %>% 
#               dplyr::filter(objective == "suppression") %>% 
#               dplyr::filter(cause != 18) %>% 
#               dplyr::filter(simultaneous_fires < 21), 
#             family = binomial())
# summary(fm2c)
# 
# fm3 <- glm(escaped_rx ~ scale(prefire_ndvi) + 
#              scale(nbhd_sd_ndvi_1) +
#              scale(prefire_erc) +
#              scale(earlyfire_vs) +
#              scale(simultaneous_fires), 
#            data = fires %>% 
#              dplyr::filter(objective == "wfu" | cause == 18) %>% 
#              dplyr::mutate(escaped_rx = ifelse(cause == 18, yes = 1, no = 0)), 
#            family = binomial())
# summary(fm3)

