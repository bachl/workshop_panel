## ---- vis-ex1

id_smple = sample(unique(d$IDsosci), 10)

d %>% 
  filter(IDsosci %in% id_smple) %>% 
  select(IDsosci, wave, verh1, verhint1) %>% 
  gather(variable, value, -IDsosci, -wave) %>% 
  ggplot(aes(wave, value, group = IDsosci, color = IDsosci)) + geom_line(position = position_jitter(height = 0.1, width = 0), show.legend = FALSE) + facet_wrap("variable")
  
