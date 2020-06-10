# Übung 5

# Index alle Medien - z-transformiert für bessere Konvergenz
d = d %>% 
  rowwise() %>% 
  mutate(med_all = sum(med1, med2, med3, med4, med5)) %>% 
  ungroup() %>% 
  mutate(med_all_z = as.numeric(scale(med_all)))

# random intercept
mue_ri = lmer(verhint3 ~ med_all_z + factor(wave) + (1 | IDsosci), data = d)
mue_ri %>% summary(correlation=FALSE)

# random slope
mue_rs = lmer(verhint3 ~ med_all_z + factor(wave) + (med_all_z | IDsosci), data = d)
mue_rs %>% summary(correlation=FALSE)

# Haupteffekt Geschlecht
mue_rs2 = lmer(verhint3 ~ med_all_z + C_sex + factor(wave) + (med_all_z | IDsosci), data = d)
mue_rs2 %>% summary(correlation=FALSE)

# Interaktion
mue_rs3 = lmer(verhint3 ~ med_all_z * C_sex + factor(wave) + (med_all_z | IDsosci), data = d)
mue_rs3 %>% summary(correlation=FALSE)

# Modellvergleich
anova(mue_ri, mue_rs, mue_rs2, mue_rs3)

# Reduktion der Varianz in random slope durch Interaktion
1 - (as.numeric(VarCorr(mue_rs3)$IDsosci["med_all_z", "med_all_z"] / as.numeric(VarCorr(mue_rs2)$IDsosci["med_all_z", "med_all_z"])))
