## ---- plm-intro

d %>%
  plm(verh1 ~ verhint1, data = ., index = "IDsosci", model = "within") %>% 
  summary()
