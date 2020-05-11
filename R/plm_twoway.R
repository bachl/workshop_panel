## ---- plm-twoway

d %>%
  plm(verh1 ~ verhint1, data = ., index = c("IDsosci", "wave"), model = "within", effect = "twoways") %>% 
  summary()

mdl_pfe_pdv = d %>%
  plm(verh1 ~ verhint1 + factor(wave), data = ., index = "IDsosci", model = "within")
mdl_pfe_pdv %>% 
  summary()
