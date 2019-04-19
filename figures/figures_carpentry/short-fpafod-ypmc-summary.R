# Purpose: Distribution of fire event size (from Short 2017)

library(tidyverse)
library(lubridate)
library(cowplot)
library(tmap)
library(here)

epoch <- ymd("1970-01-01")

sn <- st_read("data/data_output/jepson_sierra-nevada-ecoregion/jepson_sierra-nevada-ecoregion.shp")

sn_ypmc_fod <- 
  read_csv(here::here("data/data_output/short-fpafod-sierra-ypmc-nonspatial.csv"))

short_fire_size <-
  ggplot(sn_ypmc_fod %>% dplyr::filter(prop_ypmc > 0.5), aes(log(fire_size, base = 10))) +
  geom_histogram(binwidth = 0.25) +
  theme_bw() +
  labs(x = expression(Fire ~ size ~ (log[10] ~ hectares)),
       y = "Count")

short_burn_duration <-
  ggplot(sn_ypmc_fod %>% dplyr::filter(prop_ypmc > 0.5 & burn_duration < 365), aes(burn_duration)) +
  geom_histogram(binwidth = 1) +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = "Count")

short_burn_duration

short_burn_duration_inset <-
  ggplot(sn_ypmc_fod %>% dplyr::filter(prop_ypmc > 0.5), aes(burn_duration)) +
  geom_histogram(binwidth = 1) +
  theme_bw() +
  labs(x = "Burn duration (days)",
       y = "Count") +
  xlim(c(-1, 20))

short_burn_duration_incl_inset <-
  ggdraw() +
  draw_plot(short_burn_duration, 0, 0, 1, 1) +
  draw_plot(short_burn_duration_inset, 0.35, 0.35, 0.6, 0.6) +
  draw_plot_label(c("A", "B"), c(0, 0.35), c(1, 0.95), size = 15)

ggsave(plot = short_fire_size, filename = "figures/short_fire_size_histogram.png")
ggsave(plot = short_burn_duration_incl_inset, filename = "figures/short_burn_duration_histogram_inset.png")

short_spatial <- st_read("data/data_output/ee_short-burning-conditions_48-day-window_L4578_bicubic.geojson")

june2008 <-
  short_spatial %>% 
  dplyr::filter(alarm_year == 2008,
                alarm_month == 06,
                alarm_day %in% c(21, 22))

june2008 %>% 
  summarize(erc = mean(erc))


tm_shape(sn) +
  tm_polygons() +
  tm_shape(june2008) +
  tm_dots()

ggplot(june2008, aes(fire_size)) + geom_histogram()
