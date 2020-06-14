## ---- plm-twopreds

plm(verhint1 ~ ein1 + desnormp1 + injnormp1 + factor(wave), data = d, index = "IDsosci", model = "within") %>% 
  summary() %>% 
  coef()
