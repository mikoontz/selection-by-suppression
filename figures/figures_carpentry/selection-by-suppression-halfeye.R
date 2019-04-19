# Purpose: visualize model output

library(tidyverse)
library(tidybayes)

fm1b_brms <- read_rds("analyses/analyses_output/selection-by-suppression-model-fit.rds")

samps <- 
  fm1b_brms %>% 
  posterior_samples() %>% 
  dplyr::select(-lp__) %>% 
  dplyr::rename(Intercept = b_Intercept,
                `Prefire NDVI` = b_scaleprefire_ndvi,
                `Forest structure variability` = b_scalenbhd_sd_ndvi_1,
                `Prefire ERC` = b_scaleprefire_erc,
                `Early fire windspeed` = b_scaleearlyfire_vs,
                `Concurrent fires` = b_scalesimultaneous_fires)

long_samps <-
  samps %>% 
  tidyr::gather(key = "variable", value = "samps") %>% 
  dplyr::mutate(variable = factor(variable, levels = rev(c("Intercept", "Prefire NDVI", "Forest structure variability", "Prefire ERC", "Early fire windspeed", "Concurrent fires"))))

effect_sizes_halfeye <-
  ggplot(long_samps, aes(x = samps, y = variable)) +
  geom_halfeyeh() +
  theme_bw() +
  geom_vline(xintercept = 0, lty = 2) +
  labs(x = "Effect size\nLog odds change in Pr(suriving 48-hour initial attack)\nfor a 1 standard deviation increase in covariate", y = "Coefficient")

effect_sizes_halfeye

ggsave(effect_sizes_halfeye, filename = "figures/selection-by-suppression-halfeye.png")
