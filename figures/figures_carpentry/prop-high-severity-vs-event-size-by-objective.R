# Purpose: generate a plot showing the proportion of high severity as a function
# of size for different management objectives

# Load libraries
library(tidyverse)
library(sf)

fires <- 
    read_csv("data/data_output/merged-data-and-derived-variables.csv")

ggplot(fires, aes(x = log(fire_area_m2 / 10000, base = 10), y = prop_high, color = objective)) +
  geom_point() +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  labs(x = expression(Event ~ size ~ (log[10] ~ hectares)),
       y = "Proportion high severity",
       color = "Management\nobjective") +
  scale_color_viridis_d() +
  theme_bw()

ggsave("figures/prop-high-severity-vs-event-size-by-objective.png")

# ggplot(fires, aes(x = fire_area_m2 / 10000, y = prop_high, color = sdc, lty = objective, shape = objective)) +
#   geom_point() +
#   scale_x_log10() +
#   # facet_grid(~ objective) +
#   geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
#   labs(x = expression(Event ~ size ~ (log[10] ~ hectares)),
#        y = "Proportion high severity",
#        color = "SDC") +
#   scale_color_viridis_c() +
#   theme_bw()
