## ---- icc

# Null-Modell
m0 = lmer(verh1 ~ 1 + (1 | IDsosci), data = d)

m0 %>% 
  summary()

# ICC "von Hand"
0.5938 / (0.5938 + 0.4065)

# Mit performance::icc()
icc(m0)
