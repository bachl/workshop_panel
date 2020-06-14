# Übung 5
# Pakete
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggstance, broom, broom.mixed, haven, plm, lmtest, lme4, lmerTest, performance)
theme_set(theme_bw()) # ggplot theme
# Daten
d = read_rds("R/data/data.rds")

# Null-Modell mit random intercept als Referenz
ue5_m0_ri = lmer(verhint3 ~ 1 + factor(wave) + (1 | IDsosci), data = d)

# Random intercept mit ein3
ue5_m1_ri = lmer(verhint3 ~ ein3 + factor(wave) + (1 | IDsosci), data = d)
ue5_m1_ri %>% 
  summary(correlation=FALSE)

# Random slope mit ein3
ue5_m0_rs = lmer(verhint3 ~ ein3 + factor(wave) + (ein3 | IDsosci), data = d)
ue5_m0_rs %>% 
  summary(correlation=FALSE)
# Modellvergleich
anova(ue5_m0_ri, ue5_m1_ri, ue5_m0_rs)

# Dummy: 50+
d = d %>% 
  mutate(C_age50 = as.integer(C_alter > 49))

# Interaktion mit Geschlecht
ue5_m1_rs = lmer(verhint3 ~ ein3 * C_age50 + factor(wave) + (ein3 | IDsosci), data = d)
ue5_m1_rs %>% 
  summary(correlation=FALSE)
# Modellvergleich
anova(ue5_m0_ri, ue5_m1_ri, ue5_m0_rs, ue5_m1_rs)
# Reduktion der Varianz in random slope durch Interaktion
1 - (as.numeric(VarCorr(ue5_m1_rs)$IDsosci["ein3", "ein3"] / as.numeric(VarCorr(ue5_m0_rs)$IDsosci["ein3", "ein3"])))

ue5_m0_rs %>% 
  ranef() %>%
  augment(ci.level = 0.95) %>% 
  slice(seq(1, nrow(.), by = 5)) %>% 
  as_tibble() %>% 
  left_join(select(d, level = IDsosci, C_age50)) %>% 
  mutate(level = reorder(level, estimate),
         age = factor(C_age50, labels = c("-49", "50+"))) %>% 
  ggplot(aes(estimate, level, xmin = lb, xmax = ub, color = age)) + geom_pointrangeh() + labs(y = "IDsosci")
