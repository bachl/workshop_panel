# Übung 3
# Pakete
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggstance, broom, broom.mixed, haven, plm, lmtest, lme4, lmerTest, performance)
theme_set(theme_bw()) # ggplot theme
# Daten
d = read_rds("R/data/data.rds")

# ICC nur Personen
lmer(verhint3 ~ 1 + (1 | IDsosci), data = d) %>% 
  icc()

# ICC Personen und Wellen
lmer(verhint3 ~ 1 + (1 | IDsosci) + (1 | wave), data = d) %>% 
  icc(by_group = TRUE)
