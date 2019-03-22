# Purpose: generate a plot showing the proportion of high severity as a function
# of size for different management objectives

# Load libraries
library(tidyverse)
library(sf)

# Read metadata file
md <- read_csv("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv")
fires_by_severity <- read_csv("data/data_output/polygonized-fires-by-severity-classes.csv")

sev_cat_prop <-
  fires_by_severity %>% 
  dplyr::select(fire_id, sev_cat, sev_cat_prop) %>% 
  tidyr::spread(key = sev_cat, value = sev_cat_prop, fill = 0) %>% 
  dplyr::rename(prop_unchanged = `0`,
                prop_low = `1`,
                prop_mod = `2`,
                prop_high = `3`)

fires <- 
  read_csv("data/data_output/polygonized-fires-by-fire.csv") %>% 
  dplyr::left_join(md, by = "fire_id") %>% 
  dplyr::left_join(sev_cat_prop, by = "fire_id") %>% 
  dplyr::filter(!is.na(objective)) %>% 
  dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))

ggplot(fires, aes(x = fire_area_m2 / 10000, y = prop_high, color = objective)) +
  geom_point() +
  scale_x_log10() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(x = expression(Event ~ size ~ (log[10] ~ hectares)),
       y = "Proportion high severity",
       color = "Management\nobjective") +
  scale_color_viridis_d() +
  theme_bw()

ggsave("figures/prop-high-severity-vs-event-size-by-objective.pdf")

sdc <- 
  read_csv("data/data_output/high-severity-fires-with-sdc.csv") %>% 
  dplyr::select(fire_id, sdc, burn_duration)

fires_sdc <-
  fires %>% 
  left_join(sdc, by = "fire_id")

ggplot(fires_sdc, aes(x = fire_area_m2 / 10000, y = prop_high, color = sdc, lty = objective, shape = objective)) +
  geom_point() +
  scale_x_log10() +
  # facet_grid(~ objective) +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(x = expression(Event ~ size ~ (log[10] ~ hectares)),
       y = "Proportion high severity",
       color = "SDC") +
  scale_color_viridis_c() +
  theme_bw()
