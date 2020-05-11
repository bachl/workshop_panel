## ---- vis-ex2

id_smple = sample(unique(d$IDsosci), 100)
d %>% 
  filter(IDsosci %in% id_smple) %>%
  select(IDsosci, wave, verh1) %>% 
  group_by(IDsosci) %>% 
  mutate(verh1_within = verh1 - mean(verh1)) %>% 
  ungroup() %>% 
  gather(transformation, value, -IDsosci, -wave) %>% 
  ggplot(aes(wave, value, group = IDsosci)) + geom_line(position = position_jitter(width = 0, height = 0.3), show.legend = FALSE, alpha = 0.5) + facet_wrap("transformation")
