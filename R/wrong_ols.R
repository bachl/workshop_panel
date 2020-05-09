## ---- wrong-ols

lm(y ~ x, data = d) %>%
  summary(show.resid = F)
