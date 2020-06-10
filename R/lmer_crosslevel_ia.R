## ---- lmer-crosslevel-ia

# Modell mit Random Slope als Referenz
m_rs = lmer(verhint2 ~ ein2 + factor(wave) + (ein2 | IDsosci), data = d)

# Modell mit HE Geschlecht
m_rs_sex1 = lmer(verhint2 ~ ein2 + C_sex + factor(wave) + (ein2 | IDsosci), data = d)
m_rs_sex1 %>% 
  summary(correlation = FALSE)

# Modell mit IA Geschlecht*Einstellung
m_rs_sex2 = lmer(verhint2 ~ ein2 * C_sex + factor(wave) + (ein2 | IDsosci), data = d)
m_rs_sex2 %>% 
  summary(correlation = FALSE)

# Wald-Test
anova(m_rs, m_rs_sex1, m_rs_sex2)

# Reduktion der Varianz in random slope durch Interaktion
1 - (as.numeric(VarCorr(m_rs_sex2)$IDsosci["ein2", "ein2"] / as.numeric(VarCorr(m_rs_sex1)$IDsosci["ein2", "ein2"])))




