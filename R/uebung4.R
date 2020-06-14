# Übung 4
# Pakete
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggstance, broom, broom.mixed, haven, plm, lmtest, lme4, lmerTest, performance)
theme_set(theme_bw()) # ggplot theme
# Daten
d = read_rds("R/data/data.rds")

# Null-Modell
ue4_m0 = lmer(verhint3 ~ 1 + factor(wave) + (1 | IDsosci), data = d)
icc(ue4_m0)

# Mit ein3
ue4_m1 = lmer(verhint3 ~ ein3 + factor(wave) + (1 | IDsosci), data = d)
ue4_m1 %>% 
  summary(correlation=FALSE)
anova(ue4_m0, ue4_m1)
1 - (sigma(ue4_m1)^2 / sigma(ue4_m0)^2) # L1
1 - (as.numeric(VarCorr(ue4_m1)$IDsosci) / as.numeric(VarCorr(ue4_m0)$IDsosci)) # L2

# + desnormp3
ue4_m2 = lmer(verhint3 ~ ein3 + desnormp3 + factor(wave) + (1 | IDsosci), data = d)
ue4_m2 %>% 
  summary(correlation=FALSE)
anova(ue4_m0, ue4_m1, ue4_m2)
1 - (sigma(ue4_m2)^2 / sigma(ue4_m1)^2) # L1
1 - (as.numeric(VarCorr(ue4_m2)$IDsosci) / as.numeric(VarCorr(ue4_m1)$IDsosci)) # L2

# + C_sex
ue4_m3 = lmer(verhint3 ~ ein3 + desnormp3 + C_sex + factor(wave) + (1 | IDsosci), data = d)
ue4_m3 %>% 
  summary(correlation=FALSE)
anova(ue4_m0, ue4_m1, ue4_m2, ue4_m3)
1 - (sigma(ue4_m3)^2 / sigma(ue4_m2)^2) # L1
1 - (as.numeric(VarCorr(ue4_m3)$IDsosci) / as.numeric(VarCorr(ue4_m2)$IDsosci)) # L2
