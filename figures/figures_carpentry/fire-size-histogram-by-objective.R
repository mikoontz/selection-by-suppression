# Purpose: generate a histogram of the fire size distribution grouped
# by management objective

# Load libraries
library(tidyverse)
library(sf)

# Read metadata file
md <- read_csv("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv")

# Add labels in place of numeric categories for objective
md <-
  md %>% 
  dplyr::mutate(objective = ifelse(objective == 1, yes = "suppression", no = "wfu"))

# Quck summary
md %>% 
  group_by(objective) %>% 
  summarize(n = n(), 
            mean_area = mean(area_ha),
            min_area = min(area_ha), 
            max_area = max(area_ha))

ggplot(md %>% filter(!is.na(objective)), aes(x = area_ha, fill = as.factor(objective))) +
  geom_histogram(bins = 50) +
  scale_x_log10(label = function(x) ifelse(x < 1, 
                                           yes = sprintf("%.1f", x), 
                                           no = sprintf("%.0f", x))) +
  theme_bw() +
  labs(x = "Event size (hectares)", 
       y = "Count",
       fill = "Management\nobjective")


ggsave("figures/fire-size-histogram-by-management-objective.png", width = 6, height = 4)
