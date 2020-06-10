## ---- lmer-random-slope

# Modellspezifikation 
m4 = lmer(verh1 ~ verhint1 + factor(wave) + (verhint1 | IDsosci), data = d)
m4 %>% 
  summary(correlation = FALSE)
