## ---- lmer-random-slope2

# Modell mit Random Intercept als Referenz
m_ri = lmer(verhint2 ~ ein2 + factor(wave) + (1 | IDsosci), data = d)

# Modell mit Random Slope
m_rs = lmer(verhint2 ~ ein2 + factor(wave) + (ein2 | IDsosci), data = d)
m_rs %>% 
  summary(correlation = FALSE)
# profile confidence intervals 
confint(m_rs, oldNames = FALSE)
# Wald-Test
anova(m_ri, m_rs)

# RE mit 95%-CIs (aus Darstellungsgründen nur jede fünfte Person)
m_rs %>% 
  ranef() %>% 
  augment(ci.level = 0.95) %>% 
  as_tibble() %>% 
  filter(variable == "ein2") %>% 
  slice(seq(1, nrow(.), by = 5)) %>% 
  mutate(level = reorder(level, estimate)) %>% 
  ggplot(aes(estimate, level, xmin = lb, xmax = ub)) + geom_pointrangeh() + labs(y = "IDsosci")
