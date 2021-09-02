library(tidyverse)
library(geojsonsf)
library(sf)

icv <- read_csv('bd_auditoria/indice-calidad-vida.csv')

# Coord------
icv$geojson[3538] <- str_remove(icv$geojson[3538], '\\}$')
icv_coord <- icv %>% 
  rowwise() %>% 
  mutate(
    coord = geojson_sf(geojson) %>% 
      sf::st_area() %>% 
      units::set_units(ha)
    ) %>% 
  ungroup()

icv_coord_diff <- icv_coord %>% 
  transmute(diff = abs(as.numeric(coord)-sup_ha)/sup_ha)

icv_coord$coord[1] %>% as.numeric()

# ICV------
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


# Educacion-----

educ <- read_csv('bd_auditoria/ind_educacion.csv')

skimr::skim(educ)

# a_esp a_prom fuera de rango
educ %>% 
  transmute(across(c('a_esp', 'a_prom'), ~.x < 0 | .x > 19)) %>% 
  summarise(across(everything(), sum))/nrow(educ)

# a_esp_std y a_prom_std distintos a los de la base
educ %>%
  mutate(
    across(ends_with('_std'), ~str_replace_all(.x, '\\,', '\\.') %>% as.numeric()),
    across(ends_with('_std'), ~(.x-min(.x))/(max(.x)-min(.x)), .names = '{col}_check'),
    a_esp_diff = abs(a_esp_std-a_esp_std_check),
    a_prom_diff = abs(a_prom_std-a_prom_std_check)
  ) %>% 
  transmute(across(ends_with('_diff'), ~.x > .05)) %>% 
  summarise(across(everything(), sum))/nrow(educ)
