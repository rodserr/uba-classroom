# El objetivo de este trabajo es analizar datos de un estudio realizado por nu3, donde expone el consumo por tipo de alimento en 130 paises 
# y su relacion con la huella de carbono. Los datos estan expresado en kg por persona al año. Tanto para la emision de CO2 (co2_emmission)
# como para la produccion de alimento destinada a consumo (consumption).
# Buscaremos agrupar los paises segun su patron de consumo y como afecta su huella de carbono

# Librerias----
library(tidytuesdayR) # Obtencion de los datos
library(tidyverse) # "Metapaquete" para cargar diversar librerias orientadas a la manipulacion de datos tidy
library(gt) # Creacion de tablas
library(highcharter) # Graficos dinamicos

# Lectura de los datos----
# Leemos los datos desde el paquete tidytuesdayR con la funcion tt_load
tuesdata <- tidytuesdayR::tt_load(2020, week = 8)
food_consumption <- tuesdata$food_consumption %>% 
  mutate(food_category = word(food_category, 1)) # limpiamos los nombres de las categorias

# Ejemplo de datos de Argentina
food_consumption %>%
  filter(country == 'Argentina') %>%
  gt::gt() %>% 
  gt::tab_header(title = 'Argentina', subtitle = '-')

# Grafico de boxplot por tipo de alimento
food_consumption %>%
  pivot_longer(c(consumption, co2_emmission)) %>%
  ggplot(aes(x = value, y = food_category)) +
  geom_boxplot() +
  facet_wrap(~name, scales = 'free_x', ncol = 2) +
  labs(x = '', y = '', title = 'Distribucion de las variables')

# Escritura de grafico
ggplot2::ggsave('boxplot_tipo_alimento.jpg')

# Aplicacion de funcion apply----
# Podemos observar los promedios de ambas variables con la funcion sapply
food_consumption %>% 
  select(where(is.numeric)) %>% # Seleccionamos solo variables numericas
  sapply(mean)

# Comparacion con purrr::map
food_consumption %>% 
  select(where(is.numeric)) %>% # Seleccionamos solo variables numericas
  purrr::map(mean)

# Extra pmap-----

# Distancia euclidieana del consumption por tipo de alimento
consumption_distance <- food_consumption %>% 
  select(-co2_emmission) %>%
  pivot_wider(names_from = food_category, values_from = consumption) %>%
  column_to_rownames('country') %>%
  scale() %>%
  dist(method = "euclidean")

# Funcion para obtener los cluster a partir de un objeto hclust
cut_hc <- function(.hc, .k = 3, .column_name = 'country'){
  .hc %>% 
    cutree(k = .k) %>%
    as.data.frame() %>%
    rownames_to_column(var = .column_name) %>% 
    rename(cluster = '.') %>% 
    mutate(cluster = as_factor(cluster))
}

# Primera lista que va a determinar el metodo del cluster
lista_metodos <- list(
  single = 'single', complete = 'complete', centroid = 'centroid', ward = 'ward.D2'
)

# lista que contiene el dataframe a agrupar 
lista_distancia <- list(consumption_distance)

# la funcion pmap solo recibe listas, por eso hay que setear las listas a iterar dentro de una lista "contenedora"
n_cluster <- list(
  lista_metodos,
  lista_distancia
) %>% 
  pmap_df(.id = 'method', ~.y %>% hclust(method = .x) %>% cut_hc() %>% count(cluster)) 

# Si quisieramos mostrar los resultados visualmente mas ordenados: pivoteamos y desplegamos tabla
n_cluster %>% 
  pivot_wider(names_from = method, values_from = n) %>% 
  gt::gt() %>% 
  gt::tab_header(title = 'Cantidad de paises en cada cluster por Metodo', subtitle = '-')

# Agrupacion con el metodo optimo
clusters <- consumption_distance %>% 
  hclust(method = "ward.D2") %>% 
  cut_hc()

# Escribimos los datos resultantes en un csv
clusters %>% write_csv('consumption_cluster.csv')

# Extra highcharter----
# Modificacion de nombres de algunos paises para el join con la tabla de highcharter
aux_country <- tibble(
  aux_name = c(
    'United States of America', 'Republic of Serbia', 'The Bahamas', 'Taiwan',
    'United Republic of Tanzania', 'Republic of Congo',
    'Democratic Republic of the Congo'
  ),
  country = c(
    'USA', 'Serbia', 'Bahamas', 'Taiwan. ROC', 'Tanzania', 'Congo', 'Congo'
  )
)

# Dataframe de clusters con nombres modificados
country_cluster_clean <- clusters %>% 
  left_join(aux_country, by = 'country') %>% 
  transmute(
    name = if_else(is.na(aux_name), country, aux_name),
    grupo = as.numeric(cluster)
  ) 

# Configuracion de colores y nombres de los grupos
data_class <-list(
  list(name = '1', from = 1, to = 2, color = '#e9832d'),
  list(name = '2', from = 2, to = 3, color = '#04a494'),
  list(name = '3', from = 3, to = 4, color = '#4a4a4a')
)

# Grafico de Mapa Mundial 
hcmap(
  "custom/world",
  data = country_cluster_clean,
  joinBy = 'name', 
  value = 'grupo'
) %>% 
  hc_colorAxis(dataClassColor = "category", dataClasses = data_class)



