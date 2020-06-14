## ---- plm-mod

plm(verh1 ~ verhint1 * C_sex + factor(wave), data = d, index = "IDsosci", model = "within") %>% 
  summary() %>% 
  coef()
