## ---- no-pooling

d %>%
  group_by(IDsosci) %>%
  nest() %>%
  mutate(mdls = map(data, ~tidy(lm(verh1 ~ verhint1, data = .x)))) %>% 
  unnest(mdls) %>%
  ungroup() %>% 
  select(-data) %>% 
  na.omit() %>%
  filter(statistic != Inf) %>%
  filter(term == "verhint1") %>% 
  mutate_if(is.numeric, round, 2) %>% 
  print %>% 
  summarise(estimate = mean(estimate),
            std.error = sqrt(mean(std.error^2))) # simple approximation
