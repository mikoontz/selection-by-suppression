# Load libraries
library(tidyverse)
library(sf)
library(cowplot)

fires <- read_csv("data/data_output/merged-data-and-derived-variables.csv")

ia <-
  fires %>% 
  dplyr::filter(objective == "suppression") %>% 
  dplyr::filter(prop_ypmc > 0.5)


selection_on_event_size <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = log(fire_area_m2 / 10000), fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = expression(Fire ~ area ~ (log[10] ~ hectares)),
       fill = "Escaped/`survived`\ninitial attack") +
  theme_bw()

selection_on_prefire_ndvi <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = prefire_ndvi, fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = "Prefire NDVI\n(higher is more extreme)",
       fill = "Escaped/`survived`\ninitial attack") +
  theme_bw()

selection_on_fuel_heterogeneity <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = nbhd_sd_ndvi_1, fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = "Prefire NDVI heterogeneity\n(lower is more extreme)",
       fill = "Escaped/`survived`\ninitial attack") +
  theme_bw()

selection_on_erc <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = prefire_erc, fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = "Prefire Energy Release Component (ERC)\n(higher is more extreme)",
       fill = "Escaped/`survived`\ninitial attack") +
  theme_bw()

selection_on_erc

shared_legend = get_legend(selection_on_event_size)

selection_on_event_size + geom_density(data = fires_sdc %>% filter(objective == "wfu"), aes(x = log(fire_area_m2 / 10000)), inherit.aes = FALSE)
selection_on_prefire_ndvi + geom_density(data = fires_sdc %>% filter(objective == "wfu"), aes(x = prefire_ndvi), inherit.aes = FALSE)
selection_on_fuel_heterogeneity + geom_density(data = fires_sdc %>% filter(objective == "wfu"), aes(x = nbhd_sd_ndvi_1), inherit.aes = FALSE)
selection_on_erc + geom_density(data = fires_sdc %>% filter(objective == "wfu"), aes(x = prefire_erc), inherit.aes = FALSE)

panel_grid <- plot_grid(selection_on_event_size + theme(legend.position = "none"),
                        selection_on_prefire_ndvi + theme(legend.position = "none"),
                        selection_on_fuel_heterogeneity + theme(legend.position = "none"),
                        selection_on_erc + theme(legend.position = "none"),
                        nrow = 2, ncol = 2,
                        labels = LETTERS[1:4])

selection_panel_plot <- plot_grid(panel_grid, shared_legend, rel_widths = c(1, 0.1))
selection_panel_plot

ggsave(plot = selection_panel_plot, filename = "figures/selection-on-burning-conditions.pdf")
