# Übung 4

# Null-Modell
m0_ue = lmer(verhint3 ~ 1 + factor(wave) + (1 | IDsosci), data = d)
icc(m0)

# Mit med2
m1_ue = lmer(verhint3 ~ med2 + factor(wave) + (1 | IDsosci), data = d)
m1_ue %>% 
  tidy(effects = "fixed") %>% 
  mutate_if(is.numeric, round, 2)
anova(m0_ue, m1_ue)
1 - (sigma(m1_ue)^2 / sigma(m0_ue)^2) # L1
1 - (as.numeric(VarCorr(m1_ue)$IDsosci) / as.numeric(VarCorr(m0_ue)$IDsosci)) # L2

# + med1
m2_ue = lmer(verhint3 ~ med2 + med1 + factor(wave) + (1 | IDsosci), data = d)
m2_ue %>% 
  tidy(effects = "fixed") %>% 
  mutate_if(is.numeric, round, 2)
anova(m0_ue, m1_ue, m2_ue)
1 - (sigma(m2_ue)^2 / sigma(m1_ue)^2) # L1
1 - (as.numeric(VarCorr(m2_ue)$IDsosci) / as.numeric(VarCorr(m1_ue)$IDsosci)) # L2

# + C_sex
m3_ue = lmer(verhint3 ~ med2 + med1 + C_sex + factor(wave) + (1 | IDsosci), data = d)
m3_ue %>% 
  tidy(effects = "fixed") %>% 
  mutate_if(is.numeric, round, 2)
anova(m0_ue, m1_ue, m2_ue, m3_ue)
1 - (sigma(m3_ue)^2 / sigma(m2_ue)^2) # L1
1 - (as.numeric(VarCorr(m3_ue)$IDsosci) / as.numeric(VarCorr(m2_ue)$IDsosci)) # L2

