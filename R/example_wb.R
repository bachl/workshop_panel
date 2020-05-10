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

# Später löschen
# # Wrong: pooled OLS
# example_data %>%
#   lm(hangover ~ beer, data = .)
# # True z
# example_data %>%
#   lm(hangover ~ beer + z, data = .) %>%
#   summary() %>% 
#   coef() %>% 
#   .[2, ] %>% 
#   round(3)
# # Fixed effects 1: dummies
# example_data %>%
#   lm(hangover ~ beer + factor(id), data = .) %>%
#   summary() %>% 
#   coef() %>% 
#   .[2, ] %>% 
#   round(3)
# # Fixed effecs 2: de-meaning
# example_data %>%
#   group_by(id) %>%
#   mutate(beer = beer - mean(beer),
#          hangover = hangover - mean(hangover)) %>%
#   ungroup() %>%
#   lm(hangover ~ beer, data = .) %>%
#   summary() %>% 
#   coef() %>% 
#   .[2, ] %>% 
#   round(3)
# # Fixed effects 3: no pooling
# example_data %>%
#   group_by(id) %>%
#   nest() %>%
#   mutate(coefs = map_dbl(data, ~coef(lm(hangover ~ beer, data = .x))[2])) %>%
#   pull(coefs) %>%
#   summary()
