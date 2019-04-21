# Load libraries
library(tidyverse)
library(sf)
library(cowplot)

fires <- read_csv("data/data_output/merged-data-and-derived-variables.csv")

ia <-
  fires %>% 
  dplyr::filter(objective == "suppression") %>% 
  dplyr::filter(prop_ypmc > 0.5)


selection_on_concurrent_fires <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = simultaneous_fires, fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = "Number of fires concurrently burning\n(higher is more extreme)",
       y = "Pr(X)",
       fill = "Survived\ninitial attack") +
  theme_bw()

selection_on_prefire_ndvi <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = prefire_ndvi, fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = "Prefire NDVI\n(higher is more extreme)",
       y = "Pr(X)",
       fill = "Survived\ninitial attack") +
  theme_bw()

selection_on_fuel_heterogeneity <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = nbhd_sd_ndvi_1, fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = "Prefire NDVI heterogeneity\n(lower is more extreme)",
       y = "Pr(X)",
       fill = "Survived\ninitial attack") +
  theme_bw()

selection_on_erc <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = prefire_erc, fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = "Prefire Energy Release Component (ERC)\n(higher is more extreme)",
       y = "Pr(X)",
       fill = "Survived\ninitial attack") +
  theme_bw()

selection_on_erc

shared_legend = get_legend(selection_on_erc)
shared_legend_bw = get_legend(selection_on_erc + scale_fill_grey())

selection_on_concurrent_fires + geom_density(data = fires %>% filter(objective == "wfu"), aes(x = simultaneous_fires), inherit.aes = FALSE)
selection_on_prefire_ndvi + geom_density(data = fires %>% filter(objective == "wfu"), aes(x = prefire_ndvi), inherit.aes = FALSE)
selection_on_fuel_heterogeneity + geom_density(data = fires %>% filter(objective == "wfu"), aes(x = nbhd_sd_ndvi_1), inherit.aes = FALSE)
selection_on_erc + geom_density(data = fires %>% filter(objective == "wfu"), aes(x = prefire_erc), inherit.aes = FALSE)

panel_grid_bw <- plot_grid(selection_on_erc  + scale_fill_grey() + theme(legend.position = "none"),
                           selection_on_fuel_heterogeneity  + scale_fill_grey() + theme(legend.position = "none"),
                           selection_on_concurrent_fires  + scale_fill_grey() + theme(legend.position = "none"),
                           selection_on_prefire_ndvi  + scale_fill_grey() + theme(legend.position = "none"),
                           nrow = 2, ncol = 2,
                           labels = LETTERS[1:4])

selection_panel_plot_bw <- plot_grid(panel_grid_bw, shared_legend_bw, rel_widths = c(1, 0.2))
selection_panel_plot_bw

ggsave(plot = selection_panel_plot_bw, filename = "figures/selection-on-burning-conditions_bw.png")

panel_grid_color <- plot_grid(selection_on_erc + theme(legend.position = "none"),
                              selection_on_fuel_heterogeneity + theme(legend.position = "none"),
                              selection_on_concurrent_fires + theme(legend.position = "none"),
                              selection_on_prefire_ndvi + theme(legend.position = "none"),
                              nrow = 2, ncol = 2,
                              labels = LETTERS[1:4])

selection_panel_plot_color <- plot_grid(panel_grid_color, shared_legend, rel_widths = c(1, 0.2))

ggsave(plot = selection_panel_plot_color, filename = "figures/selection-on-burning-conditions.png")



# color figure; simpler model ---------------------------------------------

# No effect of simultaneous fires
selection_on_vs <-
  ggplot(ia %>% dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no")), aes(x = earlyfire_vs, fill = survived_ia)) +
  geom_density(alpha = 0.5) +
  # geom_histogram(alpha = 0.5, bins = 100) +
  labs(x = "Early fire wind speed\n(higher is more extreme)",
       y = "Pr(X)",
       fill = "Survived\ninitial attack") +
  theme_bw()

panel_grid_color <- plot_grid(selection_on_erc + theme(legend.position = "none"),
                              selection_on_fuel_heterogeneity + theme(legend.position = "none"),
                              selection_on_vs + theme(legend.position = "none"),
                              selection_on_prefire_ndvi + theme(legend.position = "none"),
                              nrow = 2, ncol = 2,
                              labels = LETTERS[1:4])

selection_panel_plot_color <- plot_grid(panel_grid_color, shared_legend, rel_widths = c(1, 0.2))

ggsave(plot = selection_panel_plot_color, filename = "figures/selection-on-burning-conditions.png")

