## ---- wb2

# Random slope für within-Effekt
wb_rs = lmer(verh1 ~ verhint1_w + verhint1_b + factor(wave) + (verhint1_w | IDsosci), data = d)
wb_rs %>% 
  summary(correlation=FALSE)
# Modellvergleich
anova(wb_ri, wb_rs)

# Interaktion des within-Prädiktors mit Geschlecht
wb_rs_ia = lmer(verh1 ~ verhint1_w * C_sex + verhint1_b + factor(wave) + (verhint1_w | IDsosci), data = d)
wb_rs_ia %>% 
  summary(correlation=FALSE)
# Modellvergleich
anova(wb_ri, wb_rs, wb_rs_ia)
# Reduktion der Varianz in random slope durch Interaktion
1 - (as.numeric(VarCorr(wb_rs_ia)$IDsosci["verhint1_w", "verhint1_w"] / as.numeric(VarCorr(wb_rs)$IDsosci["verhint1_w", "verhint1_w"])))
