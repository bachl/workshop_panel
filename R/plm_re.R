## ---- plm-re

# Einfaches RE Modell
d %>%
  plm(verh1 ~ verhint1, data = ., index = "IDsosci", model = "random") %>% 
  summary()

# Mit zusätzlichem Faktor Welle
d %>%
  plm(verh1 ~ verhint1, data = ., index = c("IDsosci", "wave"), model = "random", effect="twoways") %>% 
  summary()

# Mit FE für Welle
d %>%
  plm(verh1 ~ verhint1 + factor(wave), data = ., index = "IDsosci", model = "random") %>% 
  summary()

# Mit Prädiktor auf Personenebene (funktioniert nicht in FE, siehe oben)
d %>%
  plm(verh1 ~ verhint1 + C_sex, data = ., index = c("IDsosci", "wave"), model = "random", effect="twoways") %>% 
  summary()
