## ---- icc

# Null-Modell
m0 = d %>%
  lmer(verh1 ~ 1 + (1 | IDsosci), data = .)

m0 %>% 
  summary()

# ICC "von Hand"
round(0.5938 / (0.5938 + 0.4065), 3)

# Mit performance::icc()
icc(m0)

# icc(lmer(verh1 ~ 1 + (1 | IDsosci) + (1 | wave), data = d), by_group = TRUE)
