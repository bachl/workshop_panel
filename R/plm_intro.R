## ---- plm-intro

plm(verh1 ~ verhint1, data = d, index = "IDsosci", model = "within") %>% 
  summary()
