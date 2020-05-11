# Übung 2: Effekt der Information via ÖR TV auf Verhaltensintion "zu wenig Abstand halten"

# Modell mit plm
d %>%
  plm(verhint3 ~ med2 + factor(wave), data = ., model = "within", index = "IDsosci") %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2)

# + med1
d %>%
  plm(verhint3 ~ med2 + med1 + factor(wave), data = ., model = "within", index = "IDsosci") %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2)

# + med1*C_sex
d %>%
  plm(verhint3 ~ med2 + med1*C_sex + factor(wave), data = ., model = "within", index = "IDsosci")
  tidy() %>% 
  mutate_if(is.numeric, round, 2)
