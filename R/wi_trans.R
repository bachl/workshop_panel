## ---- wi-trans

d_wi = d %>%
  select(IDsosci, verh1, verhint1) %>% 
  group_by(IDsosci) %>%
  mutate(verh1_wi = verh1 - mean(verh1),
         verhint1_wi = verhint1 - mean(verhint1)) %>%
  ungroup()

d_wi %>% 
  select(-IDsosci) %>% 
  summary

lm(verh1_wi ~ verhint1_wi, data = d_wi) %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2)
