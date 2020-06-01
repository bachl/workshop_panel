## ---- lmer-person

# Modellspezifikation 
m3 = lmer(verh1 ~ verhint1 + C_sex + factor(wave) + (1 | IDsosci), data = d)

m3 %>% 
  summary(correlation = FALSE)

# Modellvergleich
# Wald und Info-Kriterien
anova(m0, m1, m2, m3)
# Reduktion der Varianz (Delta R^2) gegenüber M0
r2(m3, by_group = TRUE)
# Reduktion der Varianz (Delta R^2) gegenüber M2
1 - (sigma(m3)^2 / sigma(m2)^2) # L1
1 - (as.numeric(VarCorr(m3)$IDsosci) / as.numeric(VarCorr(m2)$IDsosci)) # L2
