## ---- wb3

# Zentrieren des between-Prädiktors
d = d %>% 
  mutate(verhint1_bc = verhint1_b - mean(verhint1_b))

# Interaktion von within- und between Prädiktor
wb_rs_iawb = lmer(verh1 ~ verhint1_w * verhint1_bc + factor(wave) + (verhint1_w | IDsosci), data = d)
wb_rs_iawb  %>% 
  summary(correlation=FALSE)
# Reduktion der Varianz in random slope durch Interaktion
1 - (as.numeric(VarCorr(wb_rs_iawb)$IDsosci["verhint1_w", "verhint1_w"] / as.numeric(VarCorr(wb_rs)$IDsosci["verhint1_w", "verhint1_w"])))
# Plot des kausalen Effekts als Funktion des between-Prädiktors
tibble(b1 = fixef(wb_rs_iawb)["verhint1_w"],
       b3 = fixef(wb_rs_iawb)["verhint1_w:verhint1_bc"],
       Z = quantile(model.frame(wb_rs_iawb)[, "verhint1_bc"], probs = (0:10)/10),
       theta = b1 + Z * b3,
       se_b1 = coef(summary(wb_rs_iawb))["verhint1_w", 2],
       COV_b1b3 = vcov(wb_rs_iawb)["verhint1_w", "verhint1_w:verhint1_bc"],
       se_b3 = coef(summary(wb_rs_iawb))["verhint1_w:verhint1_bc", 2],
       se_theta = sqrt(se_b1^2 + 2 * Z * COV_b1b3 + Z^2 * se_b3^2),
       ci.lo_theta = theta+qt(0.05/2, df.residual(wb_rs_iawb))*se_theta,
       ci.hi_theta = theta+qt(1-0.05/2, df.residual(wb_rs_iawb))*se_theta
) %>% 
  ggplot(aes(Z, theta, ymin=ci.lo_theta, ymax=ci.hi_theta)) + geom_ribbon(alpha = .2) + geom_line() + geom_rug(data = filter(d, wave == 1), aes(verhint1_bc, 0), sides = "b", inherit.aes = F, position = position_jitter(width = 0.1, height = 0), alpha = 0.2, length = unit(1, "cm")) + labs(x = "Verhaltensintention Between (zentriert)", y = " Within-Effekt der Verhaltensintention")





