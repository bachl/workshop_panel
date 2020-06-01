## ---- lmer-2ndtv

# Null-Modell 
m0_int1 = lmer(verhint1 ~ 1 + factor(wave) + (1 | IDsosci), data = d)
m0_int1 %>% 
  tidy(effects = "fixed") %>% 
  mutate_if(is.numeric, round, 2)
icc(m0_int1) # conditional ICC takes the fixed effects variances into account

# Modelle mit Prädiktoren
m1_int1 = lmer(verhint1 ~ ein1 + desnormp1 + injnormp1 + factor(wave) + (1 | IDsosci), data = d)
m1_int1 %>% 
  tidy(effects = "fixed") %>% 
  mutate_if(is.numeric, round, 2)

m2_int1 = lmer(verhint1 ~ ein1 + desnormp1 + injnormp1 + C_sex + factor(wave) + (1 | IDsosci), data = d)
m2_int1 %>% 
  tidy(effects = "fixed") %>% 
  mutate_if(is.numeric, round, 2)


# Modellvergleiche
# Wald und Info-Kriterien
anova(m0_int1, m1_int1, m2_int1)

# Varianzreduktion
# Vorsicht: Das ergibt hier keinen Sinn, da Vergleich mit M00 (ohne Periodeneffekte)
# Reduktion der Varianz (Delta R^2) gegenüber M0
r2(m1_int1, by_group = TRUE)

# Wir sehen stattdessen das Modell mit Perioden-FE als Null-Referenz
# Reduktion der Varianz (Delta R^2) in M1_int gegenüber M0_int
1 - (sigma(m1_int1)^2 / sigma(m0_int1)^2) # L1
1 - (as.numeric(VarCorr(m1_int1)$IDsosci) / as.numeric(VarCorr(m0_int1)$IDsosci)) # L2

# Reduktion der Varianz (Delta R^2) in M2_int gegenüber M1_int
1 - (sigma(m2_int1)^2 / sigma(m1_int1)^2) # L1
1 - (as.numeric(VarCorr(m2_int1)$IDsosci) / as.numeric(VarCorr(m1_int1)$IDsosci)) # L2
