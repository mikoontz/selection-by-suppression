# Purpose: Show bias of how fire size thresholds affect the proportion of fires that survive the first 24 hours of initial attack.
# Main point: Large fires tend to be those that survived initial attack and, as we show in this paper, burn under more extreme conditions

library(tidyverse)

fires <- read_csv("data/data_output/merged-data-and-derived-variables.csv")
short <- read_csv("data/data_output/short-fpafod-sierra-ypmc-nonspatial.csv")

ia <-
  fires %>% 
  dplyr::filter(objective == "suppression") %>% 
  dplyr::filter(prop_ypmc > 0.5)

short_ia <-
  short %>% 
  dplyr::filter(prop_ypmc > 0.5) %>% 
  dplyr::mutate(survived_ia = ifelse(burn_duration > 1, yes = 1, no = 0)) %>% 
  dplyr::rename(area_ha = fire_size)

prop_suppressed_by_size <-
  ia %>% 
  group_by(survived_ia) %>% 
  summarize(`4` = length(which(area_ha >= 4)),
            `10` = length(which(area_ha >= 10)),
            `20` = length(which(area_ha >= 20)),
            `40` = length(which(area_ha >= 40)),
            `80` = length(which(area_ha >= 80)),
            `100` = length(which(area_ha >= 100)),
            `150` = length(which(area_ha >= 150)),
            `200` = length(which(area_ha >= 200)),
            `300` = length(which(area_ha >= 300)),
            `400` = length(which(area_ha >= 400))) %>% 
  gather(key = threshold, value = count, -survived_ia) %>% 
  dplyr::mutate(threshold = as.numeric(threshold)) %>% 
  dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no"))


prop_suppressed_by_size <-
  prop_suppressed_by_size %>% 
  dplyr::left_join(prop_suppressed_by_size %>% group_by(threshold) %>% summarize(total_count = sum(count)), by = "threshold") %>% 
  dplyr::mutate(prop = count / total_count)


prop_suppressed_by_size_gg <-
  ggplot(prop_suppressed_by_size %>% dplyr::filter(survived_ia == "yes"), aes(x = threshold, y = prop)) + 
  geom_point() +
  geom_smooth()

prop_suppressed_by_size_gg <-
  ggplot(prop_suppressed_by_size, aes(x = threshold, y = prop, fill = survived_ia)) + 
  geom_col() +
  scale_fill_grey() +
  labs(color = "Survived initial attack?",
       x = "Threshold fire size (hectares)",
       y = "Proportion ")

prop_suppressed_by_size_gg


short_prop_suppressed_by_size <-
  short_ia %>% 
  dplyr::filter(!is.na(survived_ia)) %>% 
  group_by(survived_ia) %>% 
  summarize(`0.01` = length(which(area_ha >= 0.01)),
            `0.1` = length(which(area_ha >= 0.1)),
            `1` = length(which(area_ha >= 1)),
            `2` = length(which(area_ha >= 2)),
            `4` = length(which(area_ha >= 4)),
            `10` = length(which(area_ha >= 10)),
            `20` = length(which(area_ha >= 20)),
            `40` = length(which(area_ha >= 40)),
            `80` = length(which(area_ha >= 80)),
            `100` = length(which(area_ha >= 100)),
            `150` = length(which(area_ha >= 150)),
            `200` = length(which(area_ha >= 200)),
            `300` = length(which(area_ha >= 300)),
            `400` = length(which(area_ha >= 400))) %>% 
  gather(key = threshold, value = count, -survived_ia) %>% 
  dplyr::mutate(threshold = as.numeric(threshold)) %>% 
  dplyr::mutate(survived_ia = ifelse(survived_ia == 1, yes = "yes", no = "no"))


short_prop_suppressed_by_size <-
  short_prop_suppressed_by_size %>% 
  dplyr::left_join(short_prop_suppressed_by_size %>% group_by(threshold) %>% summarize(total_count = sum(count)), by = "threshold") %>% 
  dplyr::mutate(prop = count / total_count)

short_prop_suppressed_by_size_gg <-
  ggplot(short_prop_suppressed_by_size, aes(x = threshold, y = prop, fill = survived_ia)) + 
  geom_col() +
  scale_fill_grey() +
  labs(color = "Survived initial attack?",
       x = "Threshold fire size (hectares)",
       y = "Proportion ")

short_prop_suppressed_by_size_gg <-
  ggplot(short_prop_suppressed_by_size %>% dplyr::filter(survived_ia == "yes"), aes(x = threshold, y = prop * 100)) + 
  geom_point() +
  ylim(c(0, 100)) +
  labs(color = "Survived initial attack?",
       x = "Threshold fire size (hectares)",
       y = "Percent surviving initial attack") +
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs")) +
  theme_bw()

short_prop_suppressed_by_size_gg

ggsave(plot = short_prop_suppressed_by_size_gg, filename = "figures/prop-surviving-ia-by-size_short.png", height = 3, width = 6, units = "in")
