# Übung 1
# Pakete
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggstance, broom, broom.mixed, haven, plm, lmtest, lme4, lmerTest, performance)
theme_set(theme_bw()) # ggplot theme
# Daten
d = read_rds("R/data/data.rds")

# Falsches POLS
lm(verhint3 ~ ein3, data = d) %>%
  summary()

# Einfaches LSDV Modell
lm(verhint3 ~ ein3 + factor(IDsosci), data = d) %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print(n = 17)

# LSDV Modell + Perioden
lm(verhint3 ~ ein3 + factor(wave) + factor(IDsosci), data = d) %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print(n = 17)
