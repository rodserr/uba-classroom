library(tidyverse)
library(sf)
library(lubridate)

siniestros_raw <- read_csv('data/data_glm/incidentesviales_julio22.csv')

names(siniestros_raw)

siniestros <- siniestros_raw %>%
  filter(!is.na(hora)) %>% 
  mutate(
    timestamp = paste(fecha, hora) %>% dmy_hms(),
    across(ends_with('_edad'), as.numeric),
    veh_tip = paste(vehiculo1_tipo, vehiculo2_tipo, vehiculo3_tipo, sep = '-')
  )

siniestros %>% skimr::skim()

data_uba <- siniestros %>% #slice_sample(n = 100) %>%
  rowwise() %>% 
  mutate(
    edad_promedio = mean(c(vehiculo1_edad, vehiculo2_edad, vehiculo3_edad), na.rm = T)
  ) %>% 
  ungroup() %>% 
  transmute(
    total_lesionados,
    total_muertos,
    timestamp,
    causa,
    tipo_accidente,
    tipo_vialidad,
    situacion_pavimento,
    situacion_climatica,
    peaton = str_detect(veh_tip, 'PEAT'),
    bicicleta = str_detect(veh_tip, 'BICICLETA'),
    motocicleta = str_detect(veh_tip, 'MOTOCICLETA'),
    transporte_publico = str_detect(veh_tip, 'PUBLICO|URBANO'),
    transporte_carga = str_detect(veh_tip, 'CARGA'),
    edad_promedio
  ) %>% 
  as_tibble()

data_uba %>% write_csv('data/data_glm/siniestros_viales.csv')
