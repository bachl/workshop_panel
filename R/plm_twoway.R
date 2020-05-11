## ---- plm-twoway

d %>%
  plm(verh1 ~ verhint1, data = ., effect = "twoways", model = "within", index = c("IDsosci", "wave")) %>% 
  summary()

d %>%
  plm(verh1 ~ verhint1 + factor(wave), data = ., index = "IDsosci", model = "within") %>% 
  summary()
