# Übung 6
# Pakete
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggstance, broom, broom.mixed, haven, plm, lmtest, lme4, lmerTest, performance)
theme_set(theme_bw()) # ggplot theme
# Daten
d = read_rds("R/data/data.rds")

# Daten transformieren
d = d %>% 
  group_by(IDsosci) %>% 
  mutate_at(vars(ein3, desnormp3), .funs = list("w" = ~ . - mean(.), "b" = ~ mean(.))) %>% 
  ungroup() %>% 
  mutate_at(vars(ends_with("_b")), .funs = list("c" = ~ . - mean(.)))

# Random intercept Null-Modell mit period FE
ue6_ri_m0 = lmer(verhint3 ~ 1 + factor(wave) + (1 | IDsosci), data = d)
ue6_ri_m0 %>% 
  summary(correlation = FALSE)

# Random intercept
ue6_ri_m1 = lmer(verhint3 ~ 
                   ein3_w + desnormp3_w + # within
                   ein3_b + desnormp3_b + # between
                   factor(wave) + # period fe
                   (1 | IDsosci), # random intercept
                 data = d)
ue6_ri_m1 %>% 
  summary(correlation = FALSE)
# Modellvergleich
anova(ue6_ri_m0, ue6_ri_m1)
1 - (sigma(ue6_ri_m1)^2 / sigma(ue6_ri_m0)^2) # L1
1 - (as.numeric(VarCorr(ue6_ri_m1)$IDsosci) / as.numeric(VarCorr(ue6_ri_m0)$IDsosci)) # L2
# Zum Vergleich
plm(verhint3 ~ ein3 + desnormp3 + factor(wave), data = d, index = "IDsosci", model = "within") %>% 
  summary() %>% 
  coef()

# Dummy: 50+
d = d %>% 
  mutate(C_age50 = as.integer(C_alter > 49))

# + Personenmerkmal Ab 50
ue6_ri_m2 = lmer(verhint3 ~ 
                   ein3_w + desnormp3_w + # within
                   C_age50 + # Ab 50
                   ein3_b + desnormp3_b + # between
                   factor(wave) + # period fe
                   (1 | IDsosci), # random intercept
                 data = d)
ue6_ri_m2 %>% 
  summary(correlation = FALSE)
# Modellvergleich
anova(ue6_ri_m0, ue6_ri_m1, ue6_ri_m2)
1 - (sigma(ue6_ri_m2)^2 / sigma(ue6_ri_m1)^2) # L1
1 - (as.numeric(VarCorr(ue6_ri_m2)$IDsosci) / as.numeric(VarCorr(ue6_ri_m1)$IDsosci)) # L2

# Moderation der kausalen Effekte durch Alter (Ab 50)
ue6_ri_m3 = lmer(verhint3 ~ 
                   (ein3_w + desnormp3_w) * C_age50 + # within by sex
                   ein3_b + desnormp3_b + # between
                   factor(wave) + # period fe
                   (1 | IDsosci), # random intercept
                 data = d)
ue6_ri_m3 %>% 
  summary(correlation = FALSE)
# Modellvergleich
anova(ue6_ri_m0, ue6_ri_m1, ue6_ri_m2, ue6_ri_m3)

# Random slope
# Hier kommen wir wieder an die Grenze der Daten
# Nach einigen Checks sind die Modelle mit Konvergenz-Warnung halbwegs zuverlässig
# Für Publikation etc. sollte das natürlich noch validiert werden
# Oder gleich andere Schätzverfahren
ue6_rs_m0 = lmer(verhint3 ~ 
                   ein3_w + desnormp3_w + # within
                   ein3_b_c + desnormp3_b_c + # between
                   factor(wave) + # period fe
                   (ein3_w + desnormp3_w | IDsosci), # random slope
                 data = d)
ue6_rs_m0 %>% 
  summary(correlation = FALSE)
anova(ue6_ri_m1, ue6_rs_m0)
confint(ue6_rs_m0, oldNames = FALSE)

# Nochmal Effekt * Alter (50+)
ue6_rs_m1 = lmer(verhint3 ~ 
                   (ein3_w + desnormp3_w) * C_age50 + # within X Sex
                   ein3_b_c + desnormp3_b_c + # between
                   factor(wave) + # period fe
                   (ein3_w + desnormp3_w | IDsosci), # random slope
                 data = d)
ue6_rs_m1 %>% 
  summary(correlation = FALSE)
anova(ue6_rs_m0, ue6_rs_m1)

# Within * between (zentrierte between-Prädiktoren)
ue6_rs_m2 = lmer(verhint3 ~ 
                   ein3_w * ein3_b_c +
                   desnormp3_w * desnormp3_b_c + 
                   factor(wave) + # period fe
                   (ein3_w + desnormp3_w | IDsosci), # random slope
                 data = d)
ue6_rs_m2 %>% 
  summary(correlation = FALSE)
anova(ue6_rs_m0, ue6_rs_m2)
# Reduktion der Varianz in random slope durch Interaktion
1 - (as.numeric(VarCorr(ue6_rs_m2)$IDsosci["ein3_w", "ein3_w"] / as.numeric(VarCorr(ue6_rs_m0)$IDsosci["ein3_w", "ein3_w"])))
1 - (as.numeric(VarCorr(ue6_rs_m2)$IDsosci["desnormp3_w", "desnormp3_w"] / as.numeric(VarCorr(ue6_rs_m0)$IDsosci["desnormp3_w", "desnormp3_w"])))

# Visualisierung der Interaktionseffekte
tibble(b1 = fixef(ue6_rs_m2)["ein3_w"],
       b3 = fixef(ue6_rs_m2)["ein3_w:ein3_b_c"],
       Z = quantile(model.frame(ue6_rs_m2)[, "ein3_b_c"], probs = (0:10)/10),
       theta = b1 + Z * b3,
       se_b1 = coef(summary(ue6_rs_m2))["ein3_w", 2],
       COV_b1b3 = vcov(ue6_rs_m2)["ein3_w", "ein3_w:ein3_b_c"],
       se_b3 = coef(summary(ue6_rs_m2))["ein3_w:ein3_b_c", 2],
       se_theta = sqrt(se_b1^2 + 2 * Z * COV_b1b3 + Z^2 * se_b3^2),
       ci.lo_theta = theta+qt(0.05/2, df.residual(ue6_rs_m2))*se_theta,
       ci.hi_theta = theta+qt(1-0.05/2, df.residual(ue6_rs_m2))*se_theta
) %>% 
  ggplot(aes(Z, theta, ymin=ci.lo_theta, ymax=ci.hi_theta)) + geom_ribbon(alpha = .2) + geom_line() + geom_rug(data = filter(d, wave == 1), aes(ein3_b_c, 0), sides = "b", inherit.aes = F, position = position_jitter(width = 0.1, height = 0), alpha = 0.2, length = unit(1, "cm")) + labs(x = "Einstellung Between (zentriert)", y = " Within-Effekt der Einstellung") + geom_hline(yintercept = 0, color = "red")

tibble(b1 = fixef(ue6_rs_m2)["desnormp3_w"],
       b3 = fixef(ue6_rs_m2)["desnormp3_w:desnormp3_b_c"],
       Z = quantile(model.frame(ue6_rs_m2)[, "desnormp3_b_c"], probs = (0:10)/10),
       theta = b1 + Z * b3,
       se_b1 = coef(summary(ue6_rs_m2))["desnormp3_w", 2],
       COV_b1b3 = vcov(ue6_rs_m2)["desnormp3_w", "desnormp3_w:desnormp3_b_c"],
       se_b3 = coef(summary(ue6_rs_m2))["desnormp3_w:desnormp3_b_c", 2],
       se_theta = sqrt(se_b1^2 + 2 * Z * COV_b1b3 + Z^2 * se_b3^2),
       ci.lo_theta = theta+qt(0.05/2, df.residual(ue6_rs_m2))*se_theta,
       ci.hi_theta = theta+qt(1-0.05/2, df.residual(ue6_rs_m2))*se_theta
) %>% 
  ggplot(aes(Z, theta, ymin=ci.lo_theta, ymax=ci.hi_theta)) + geom_ribbon(alpha = .2) + geom_line() + geom_rug(data = filter(d, wave == 1), aes(desnormp3_b_c, 0), sides = "b", inherit.aes = F, position = position_jitter(width = 0.1, height = 0), alpha = 0.2, length = unit(1, "cm")) + labs(x = "Deskriptive Norm Between (zentriert)", y = " Within-Effekt der deskriptiven Norm") + geom_hline(yintercept = 0, color = "red")
