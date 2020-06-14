## ---- plm-re

# Einfaches RE Modell
plm(verh1 ~ verhint1, data = d, index = "IDsosci", model = "random") %>% 
  summary()

# Mit zusätzlichem Faktor Welle
plm(verh1 ~ verhint1, data = d, index = c("IDsosci", "wave"), model = "random", effect="twoways") %>% 
  summary()

# Mit FE für Welle
plm(verh1 ~ verhint1 + factor(wave), data = d, index = "IDsosci", model = "random") %>% 
  summary()

# Mit Prädiktor auf Personenebene (funktioniert nicht in FE, siehe oben)
plm(verh1 ~ verhint1 + C_sex, data = d, index = c("IDsosci", "wave"), model = "random", effect="twoways") %>% 
  summary()
