## ---- wb1

# Transformation: Trennen von within- und between-Präditor
d = d %>% 
  group_by(IDsosci) %>% 
  mutate(verhint1_w = verhint1 - mean(verhint1), # within
         verhint1_b = mean(verhint1)) %>% # between
  ungroup()
# Zwei Personen zur Illustration
d %>% 
  filter(IDsosci %in% c("050IPY", "02E6C8")) %>% 
  select(IDsosci, wave, starts_with("verhint1"))

# Null-Modell als Referenz
wb_0 = lmer(verh1 ~ 1 + factor(wave) + (1 | IDsosci), data = d)

# Within-between Modell mit random intercept
wb_ri = lmer(verh1 ~ verhint1_w + verhint1_b + factor(wave) + (1 | IDsosci), data = d)
wb_ri %>% 
  summary(correlation=FALSE)
# Varianzreduktion
1 - (sigma(wb_ri)^2 / sigma(wb_0)^2) # L1
1 - (as.numeric(VarCorr(wb_ri)$IDsosci) / as.numeric(VarCorr(wb_0)$IDsosci)) # L2

# Vergleich mit fixed effects Modell
fe =  plm(verh1 ~ verhint1 + factor(wave), data = d, index = "IDsosci", model = "within")
fe %>% 
  summary() %>% 
  coef()
