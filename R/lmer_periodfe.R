## ---- lmer-periodfe

# Modellspezifikation 
m2 = lmer(verh1 ~ verhint1 + factor(wave) + (1 | IDsosci), data = d)

m2 %>% 
  summary(correlation = FALSE)

# Modellvergleich
# Wald und Info-Kriterien
anova(m0, m1, m2)
# Reduktion der Varianz (Delta R^2) gegenüber M0
r2(m2, by_group = TRUE)
# Reduktion der Varianz (Delta R^2) gegenüber M1
1 - (sigma(m2)^2 / sigma(m1)^2) # L1
1 - (as.numeric(VarCorr(m2)$IDsosci) / as.numeric(VarCorr(m1)$IDsosci)) # L2
