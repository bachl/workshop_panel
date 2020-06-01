## ---- lmerstart

m1 = lmer(verh1 ~ verhint1 + (1 | IDsosci), data = d)

m1 %>% 
  summary(correlation = FALSE)
