## ---- icc2

# Null-Modell mit zwei Gruppierungsfaktoren
d %>%
  lmer(verh1 ~ 1 + (1 | IDsosci) + (1 | wave), data = .) %>% 
  icc(by_group = TRUE)
