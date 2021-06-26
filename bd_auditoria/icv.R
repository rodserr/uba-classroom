library(tidyverse)

icv <- read_csv('bd_auditoria/indice-calidad-vida.csv')

# Resumen de variables
skimr::skim(icv)

# Indices Fuera de Rango
icv %>% 
  transmute(
    across(starts_with('i'), ~.x < 0 | .x > 1 )
  ) %>% 
  summarise(
    across(everything(), ~sum(.x)/n())
  )

# Radio Censal: Falta el digito 0 en todos los casos
icv %>% 
  transmute(dpt = str_sub(radio10, start = 1L, end = 2L)) %>% 
  distinct()

# Superficie segun google 2500 km2
sum(icv$sup_ha)

# Diferencia en densidad de poblacion
icv %>% 
  mutate(dens_diff = round(pobl_tot/sup_ha, 2)-dens_ha) %>% 
  filter(dens_diff != 0)

# Duplicados en radio10
unique(icv$radio10) %>% length()

# Pertenencia entre partido y radio01
icv %>% 
  mutate(dpt = str_sub(radio10, start = 1L, end = 2L)) %>% 
  distinct(dpt, partido)


