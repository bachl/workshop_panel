## ---- lmtest-demo

# Normale SE und CI
mdl_pfe_pdv %>% 
  coeftest() %>% 
  round(3)

mdl_pfe_pdv %>% 
  coefci() %>% 
  round(3)

# Heteroskedasticity-robust SE and CI
mdl_pfe_pdv %>% 
  coeftest(vcov. = vcovHC) %>% 
  round(3)

mdl_pfe_pdv %>% 
  coefci(vcov. = vcovHC) %>% 
  round(3)
