# Load libraries
library(tidyverse)
library(sf)
library(lubridate)
library(cowplot)

# Read metadata file
md <- read_csv("data/data_raw/wildfire-severity_sierra-nevada-ca-usa_ypmc_1984-2017_fire-metadata.csv") %>% 
  dplyr::mutate(alarm_date = ymd(paste(alarm_year, alarm_month, alarm_day, sep = "-")),
                cont_date = ymd(paste(cont_year, cont_month, cont_day, sep = "-")),
                burn_duration = as.numeric(cont_date - alarm_date))
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

sdc <-
  read_csv("data/data_output/high-severity-fires-with-sdc.csv") %>%
  dplyr::select(fire_id, sdc)

fires_sdc <-
  fires %>% 
  dplyr::left_join(sdc, by = "fire_id") %>% 
  dplyr::filter(burn_duration > 0,
                burn_duration < 365)

duration_distribution <-
  ggplot(fires_sdc, aes(x = burn_duration, fill = objective)) +
  geom_histogram(alpha = 0.5, bins = max(fires_sdc$burn_duration)) +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = "Count",
       fill = "Management\nobjective")

ggsave(plot = duration_distribution, filename = "figures/burn-duration-density-plot-by-objective.pdf")

event_size_v_duration <-
  ggplot(fires_sdc, aes(x = burn_duration, y = fire_area_m2 / 10000, color = objective)) +
  geom_point(alpha = 0.1) +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = expression(Event ~ size ~ (log[10] ~ hectares)),
       color = "Management\nobjective")

ggsave(plot = event_size_v_duration, filename = "figures/burn-duration-vs-event-size-by-objective.pdf")

sdc_v_duration <-
  ggplot(fires_sdc, aes(x = burn_duration, y = sdc, color = objective)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = "Stand replacing\ndecay coefficient",
       color = "Management\nobjective")

ggsave(plot = sdc_v_duration, filename = "figures/burn-duration-vs-sdc-by-objective.pdf")

prop_high_v_duration <- 
  ggplot(fires_sdc, aes(x = burn_duration, y = prop_high, color = objective)) +
  geom_point(alpha = 0.1) +
  geom_smooth() +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = "Proportion high severity",
       color = "Management\nobjective")

ggsave(plot = prop_high_v_duration, filename = "figures/burn-duration-vs-proportion-high-severity-by-objective.pdf")


duration_distribution_no_legend <- duration_distribution + theme(legend.position = "none")
event_size_v_duration_no_legend <- event_size_v_duration + theme(legend.position = "none")
sdc_v_duration_no_legend <- sdc_v_duration + theme(legend.position = "none") 
prop_high_v_duration_no_legend <- prop_high_v_duration + theme(legend.position = "none")

shared_legend <- get_legend(duration_distribution)

# build grid without legends
pgrid <- plot_grid(duration_distribution_no_legend, 
                   event_size_v_duration_no_legend, 
                   prop_high_v_duration_no_legend, 
                   sdc_v_duration_no_legend,
                   ncol = 2,
                   labels = LETTERS[1:4])
# add legend
panel_plot <- plot_grid(pgrid, shared_legend, ncol = 2, rel_widths = c(1, .1))

ggsave(plot = panel_plot, filename = "figures/burn-duration-4-panel.pdf")
