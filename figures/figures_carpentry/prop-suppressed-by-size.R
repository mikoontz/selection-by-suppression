# Purpose: Show bias of how fire size thresholds affect the proportion of fires that survive the first 24 hours of initial attack.
# Main point: Large fires tend to be those that survived initial attack and, as we show in this paper, burn under more extreme conditions

library(tidyverse)

fires <- read_csv("data/data_output/merged-data-and-derived-variables.csv")

ia <-
  fires %>% 
  dplyr::filter(objective == "suppression") %>% 
  dplyr::filter(prop_ypmc > 0.5)

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


ggplot(prop_suppressed_by_size, aes(threshold, prop, color = as.factor(survived_ia))) + 
  geom_point() + 
  geom_smooth() +
  labs(color = "Survived first 24 hours?",
       x = "Threshold fire size (hectares)",
       y = "Proportion ") +
  scale_x_log10() +
  theme_bw()
