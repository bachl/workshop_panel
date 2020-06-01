## ---- lmervgl

# Wald Test und Informationskriterien
anova(m0, m1)

# Reduktion der Varianz (Delta R^2) - manuell
1 - (sigma(m1)^2 / sigma(m0)^2) # L1
1 - (as.numeric(VarCorr(m1)$IDsosci) / as.numeric(VarCorr(m0)$IDsosci)) # L2

# Reduktion der Varianz (Delta R^2) - mit performance::r2() (Vergleicht immer mit Null-Modell)
r2(m1, by_group = TRUE)
