## ---- period-fe

lm(verh1 ~ verhint1 + factor(wave) + factor(IDsosci), data = d) %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print(n = 17)
