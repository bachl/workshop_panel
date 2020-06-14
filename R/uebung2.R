# Übung 2
# Pakete
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggstance, broom, broom.mixed, haven, plm, lmtest, lme4, lmerTest, performance)
theme_set(theme_bw()) # ggplot theme
# Daten
d = read_rds("R/data/data.rds")

# Modell mit plm
plm(verhint3 ~ ein3 + factor(wave), data = d, model = "within", index = "IDsosci") %>%
  summary()

# + desnormp3
plm(verhint3 ~ ein3 + desnormp3 + factor(wave), data = d, model = "within", index = "IDsosci") %>% 
  summary()

# *C_sex
plm(verhint3 ~ (ein3 + desnormp3) * C_sex + factor(wave), data = d, model = "within", index = "IDsosci") %>% 
  summary()
