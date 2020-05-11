## ---- plm-mod

d %>%
  plm(verh1 ~ verhint1 * C_sex + factor(wave), data = ., index = "IDsosci", model = "within") %>% 
  tidy() %>% 
  mutate_if(is.numeric, round, 2)
