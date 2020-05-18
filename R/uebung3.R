# Übung 3: ICC der Verhaltensintention "zu wenig Abstand halten"

# ICC nur Personen
d %>%
  lmer(verhint3 ~ 1 + (1 | IDsosci), data = .) %>% 
  icc()

# ICC Personen und Wellen
d %>%
  lmer(verhint3 ~ 1 + (1 | IDsosci) + (1 | wave), data = .) %>% 
  icc(by_group = TRUE)
