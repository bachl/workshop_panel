## ---- wb-trans

# Transformation: Trennen von within- und between-Prädiktor
d = d %>% 
  group_by(IDsosci) %>% 
  mutate(verhint1_w = verhint1 - mean(verhint1), # within
         verhint1_b = mean(verhint1)) %>% # between
  ungroup()
# Zwei Personen zur Illustration
d %>% 
  filter(IDsosci %in% c("050IPY", "02E6C8")) %>% 
  select(IDsosci, wave, starts_with("verhint1"))
