## ---- example-wb

set.seed(1383);

n = 8; t = 6; tce = 1

example_data = tibble(id = 1:n,
                      z = seq(-3, 3, length.out = n),
                      m_beer = z * 1 + rnorm(n)) %>% 
  replicate(t, ., simplify = FALSE) %>% 
  bind_rows() %>% 
  mutate(beer = m_beer + rnorm(n*t),
         beer = beer + abs(min(beer)),
         hangover = beer * tce + z * -2 + rnorm(n*t))

plt_pool = example_data %>% 
  ggplot(aes(beer, hangover)) + geom_point() + stat_smooth(method = "lm", se = FALSE)

plt_nopool = example_data %>% 
  ggplot(aes(beer, hangover, color = factor(id))) + geom_point(show.legend = FALSE) + stat_smooth(method = "lm", se = FALSE, show.legend = FALSE)
