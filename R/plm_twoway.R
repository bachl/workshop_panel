## ---- plm-twoway

plm(verh1 ~ verhint1, data = d, index = c("IDsosci", "wave"), model = "within", effect = "twoways") %>% 
  summary()

mdl_pfe_pdv = plm(verh1 ~ verhint1 + factor(wave), data = d, index = "IDsosci", model = "within")
mdl_pfe_pdv %>% 
  summary()
