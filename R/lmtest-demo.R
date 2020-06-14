## ---- lmtest-demo

# Normale SE und CI
mdl_pfe_pdv %>% 
  coeftest()

mdl_pfe_pdv %>% 
  coefci()

# Heteroskedasticity-robust SE and CI
mdl_pfe_pdv %>% 
  coeftest(vcov. = vcovHC)

mdl_pfe_pdv %>% 
  coefci(vcov. = vcovHC)
