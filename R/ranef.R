## ---- ranef

# tibble der RE und ihre Verteilung als Histogramm
m0 %>% 
  ranef() %>%
  augment(ci.level = 0.95) %>% 
  as_tibble() %>% 
  print(n = 12) %>% 
  ggplot(aes(estimate)) + geom_histogram()

# RE mit 95%-CIs (aus Darstellungsgründen nur jede fünfte Person)
m0 %>% 
  ranef() %>%
  augment(ci.level = 0.95) %>% 
  slice(seq(1, nrow(.), by = 5)) %>% 
  ggplot(aes(estimate, level, xmin = lb, xmax = ub)) + geom_pointrangeh() + labs(y = "IDsosci")
