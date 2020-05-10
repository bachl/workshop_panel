## ---- no-pooling

n = 8; t = 6; tce = 1
example_data = tibble(id = 1:n,
                      z = seq(-3, 3, length.out = n),
                      m_beer = z * 1 + rnorm(n)) %>% 
  replicate(t, ., simplify = FALSE) %>% 
  bind_rows() %>% 
  mutate(beer = m_beer + rnorm(n*t),
         beer = beer + abs(min(beer)),
         hangover = beer * tce + z * -2 + rnorm(n*t))

example_data %>%
  group_by(id) %>%
  nest() %>%
  mutate(mdls = map(data, ~tidy(lm(hangover ~ beer, data = .x)))) %>% 
  unnest(mdls) %>%
  ungroup() %>% 
  select(-data) %>% 
  filter(term == "beer") %>% 
  mutate_if(is.numeric, round, 2)
