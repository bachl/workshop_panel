## ---- packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggstance, broom, broom.mixed, haven, plm, lmtest, lme4, lmerTest, performance)
theme_set(theme_bw()) # ggplot theme

tibble(package = c("R", sort(pacman::p_loaded()))) %>% 
  mutate(version = map_chr(package, ~as.character(pacman::p_version(package = .x)))) %>% 
  knitr::kable()
