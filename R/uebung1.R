# Übung 1: Effekt der Information via ÖR TV auf Verhaltensintion "zu wenig Abstand halten"

# Visualisierung
id_smple = sample(unique(d$IDsosci), 10)
d %>% 
  filter(IDsosci %in% id_smple) %>% 
  select(IDsosci, wave, verhint3, med2) %>% 
  gather(variable, value, -IDsosci, -wave) %>% 
  ggplot(aes(wave, value, group = IDsosci, color = IDsosci)) + geom_line(position = position_jitter(height = 0.1, width = 0), show.legend = FALSE) + facet_wrap("variable")

# Falsches POLS
d %>%
  lm(verhint3 ~ med2, data = .) %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2)

# Einfaches LSDV Modell
d %>%
  lm(verhint3 ~ med2 + factor(IDsosci), data = .) %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print(n = 17)

# LSDV Modell + Perioden
d %>%
  lm(verhint3 ~ med2 + factor(wave) + factor(IDsosci), data = .) %>%
  tidy() %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print(n = 17)

