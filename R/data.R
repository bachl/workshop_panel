## ---- data

# Lesen des SPSS Datensatzes
d_wide = haven::read_spss("R/data/Ausschnitt_panelisten_corona_gelabelt_2020-05-07.sav")

# Über die Zeit konstant (angenommen)
d_constants = d_wide %>% 
  select(IDsosci,
         contains("alter"),
         contains("gesch_d"),
         contains("bildung"),
         contains("kollek")) %>% 
  mutate_at(vars(contains("alter")), as.integer) %>%
  mutate_at(vars(contains("alter")), ~if_else(.x < 1, NA_integer_, .x)) %>% # missings in Alter
  rowwise() %>% 
  # Alter und Geschlecht ändern sich bei wenigen Personen, hier möglichst einfach als (round) mean 
  mutate(C_alter = mean(c(w1_alter, w2_alter, w3_alter, w4_alter), na.rm = TRUE),
         C_sex = round(mean(c(w1_gesch_d, w2_gesch_d, w3_gesch_d, w4_gesch_d), na.rm = TRUE))) %>% 
  ungroup() %>% 
  rename(C_kollek = w1_kollek4,
         C_edu = w1_bildung) %>% 
  select(IDsosci, starts_with("C_"))

# Über die Zeit variabel in das long format
d = d_wide %>% 
  select(-contains("alter"),
         -contains("gesch"),
         -contains("bildung"),
         -contains("kollek"),
         -contains("_ID"),
         -w1, -w2, -w3, -w4, -w4_coronapeers,
         -contains("lfdn")) %>% 
  gather(variable, value, -IDsosci) %>% 
  separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>% 
  spread(variable, value) %>% 
  mutate(wave = as.integer(str_sub(wave, start = 2))) %>% 
  # Und Konstanten wieder anhängen
  left_join(d_constants)

# Speichern der aufbereiteten Daten im long format
saveRDS(d, "R/data/data.rds")

# Überblick über die Variablen
d_wide %>% 
  select(starts_with("w4"),
         sex = w4_gesch_d,
         -w4_gesch,
         -contains("_ID"),
         -w1, -w2, -w3, -w4, -w4_coronapeers,
         -contains("lfdn")) %>% 
  map_chr(~attr(.x, "label")) %>% 
  enframe(name = "Variablenname", value = "Label") %>% 
  mutate(Variablenname = str_replace(Variablenname, "w4_", "")) %>% 
  arrange(Variablenname) %>% 
  knitr::kable()
