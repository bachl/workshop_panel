## ---- plm-twopreds

d %>%
  plm(verhint1 ~ ein1 + desnormp1 + injnormp1 + factor(wave), data = ., index = "IDsosci", model = "within") %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 2)

