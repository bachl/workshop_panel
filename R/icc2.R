## ---- icc2

# Null-Modell mit zwei Gruppierungsfaktoren
lmer(verh1 ~ 1 + (1 | IDsosci) + (1 | wave), data = d) %>% 
  icc(by_group = TRUE)
