# Load libraries
library(tidyverse)
library(sf)
library(lubridate)
library(cowplot)

fires <- 
  read_csv("data/data_output/merged-data-and-derived-variables.csv") %>% 
  dplyr::mutate(wfu_survived_ia = case_when(objective == "wfu" ~ "wfu",
                                            objective == "suppression" & survived_ia == 1 ~ "survived IA",
                                            objective == "suppression" & survived_ia == 0 ~ "extinguished during IA")) %>% 
  dplyr::filter(prop_ypmc > 0.5)

duration_distribution <-
  ggplot(fires, aes(x = burn_duration, fill = objective)) +
  geom_density(alpha = 0.5) +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = "Density",
       fill = "Management\nobjective")

ggsave(plot = duration_distribution, filename = "figures/burn-duration-density-plot-by-objective.png")

event_size_v_duration <-
  ggplot(fires, aes(x = burn_duration, y = log(fire_area_m2 / 10000, base = 10), lty = objective, color = objective)) +
  geom_point(alpha = 0.1, cex = 0.9) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = expression(Event ~ size ~ (log[10] ~ hectares)),
       lty = "Management\nobjective",
       color = "Management\nobjective")
event_size_v_duration
ggsave(plot = event_size_v_duration, filename = "figures/burn-duration-vs-event-size-by-objective.png")

sdc_v_duration <-
  ggplot(fires, aes(x = burn_duration, y = sdc, lty = objective, color = objective)) +
  geom_point(alpha = 0.1, cex = 0.9) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = "Stand replacing\ndecay coefficient",
       lty = "Management\nobjective",
       color = "Management\nobjective")

ggsave(plot = sdc_v_duration, filename = "figures/burn-duration-vs-sdc-by-objective.png")

prop_high_v_duration <- 
  ggplot(fires, aes(x = burn_duration, y = prop_high, lty = objective, color = objective)) +
  geom_point(alpha = 0.1, cex = 0.9) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = "Proportion high severity",
       lty = "Management\nobjective",
       color = "Management\nobjective")

ggsave(plot = prop_high_v_duration, filename = "figures/burn-duration-vs-proportion-high-severity-by-objective.png")


duration_distribution_no_legend <- duration_distribution + theme(legend.position = "none")
event_size_v_duration_no_legend <- event_size_v_duration + theme(legend.position = "none")
sdc_v_duration_no_legend <- sdc_v_duration + theme(legend.position = "none") 
prop_high_v_duration_no_legend <- prop_high_v_duration + theme(legend.position = "none")

shared_legend <- get_legend(sdc_v_duration)

# build grid without legends
pgrid <- plot_grid(duration_distribution_no_legend, 
                   event_size_v_duration_no_legend, 
                   prop_high_v_duration_no_legend, 
                   sdc_v_duration_no_legend,
                   ncol = 2,
                   labels = LETTERS[1:4])
# add legend
panel_plot <- plot_grid(pgrid, shared_legend, ncol = 2, rel_widths = c(1, 0.2))

ggsave(plot = panel_plot, filename = "figures/burn-duration-4-panel.png")
