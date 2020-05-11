## ---- wrong-ols

lm(verh1 ~ verhint1, data = d) %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2)
  