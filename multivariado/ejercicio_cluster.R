# libaries----
library(tidyverse)
library(readxl)
library(NbClust)

paises <- read_excel('multivariado/Base de datos Ejemplo 2 ACP ( países europeos ).xlsx') %>% 
  rename(pais = ...1)

which(is.na(paises))

# skimr::skim(paises)

paises %>% 
  pivot_longer(-pais) %>% 
  ggplot(aes(x = name, y = value/100)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(x = '', y = '')

paises %>% filter(Agricultura > 60)

paises_clean <- paises %>% 
  filter(Agricultura < 60)

hc <- paises_clean %>% 
  column_to_rownames('pais') %>%
  scale() %>%
  dist(method = "euclidean") %>% 
  hclust(method = 'ward.D2')

plot(hc)

ncluster <- paises_clean %>% 
  column_to_rownames('pais') %>% 
  NbClust(distance = "euclidean", min.nc = 2, max.nc = 8, method = "ward.D2", index = "alllong")

centroids <- paises_clean %>% 
  mutate(hcluster = ncluster$Best.partition) %>% 
  group_by(hcluster) %>% 
  summarise(
    across(where(is.numeric), mean)
  ) %>% 
  select(-hcluster)
  
km_cent <- paises_clean %>% 
  column_to_rownames('pais') %>% 
  kmeans(centroids)

km_random <- paises_clean %>% 
  column_to_rownames('pais') %>% 
  kmeans(2)

list(centroid = km_cent, random = km_random) %>% 
  map_dfr(~.x[c('totss', 'tot.withinss', 'betweenss')] %>% flatten_df(), .id = 'center_type') %>% 
  gt::gt()

paises_clust <- paises_clean %>% 
  mutate(cluster = as_factor(km_cent$cluster))

list_variables <- paises_clust %>% 
  select(-pais, -cluster) %>% 
  as.list()

list_test <- map2(list_variables, list(km_cent$cluster), ~t.test(.x~.y))

list_test %>% 
  map_dfr(~.x[c('p.value', 'estimate')] %>% flatten_df(), .id = 'sector') %>%
  gt::gt() %>% 
  gt::fmt_number('p.value', decimals = 4) %>% 
  gt::fmt_number(contains('mean'), decimals = 1)

paises_clust %>% 
  ggplot(aes(x = `Servicios Ind`, y = Agricultura, fill = cluster, label = pais)) + 
  geom_label(color = 'white') +
  theme(
    legend.position = 'top',
    legend.justification = 'left'
  ) +
  scale_fill_manual(values = c("#e9832d", "#04a494", "#4a4a4a"))

# Map
library(highcharter)
countries <- c(
  "Germany", "Austria", "Belgium", "Bulgaria", "Czechoslovakia",
  "Denmark", "Spain", "Finland", "France", "Greece", "Netherlands", 
  "Hungary", "Ireland", "Italy", "Luxembourg", "Norway",
  "Poland", "Portugal", "United Kingdom", "Romania", "Russia",
  "Sweden", "Switzerland"
)

data_class <-list(
  list(name = 'Oficina', from = 1, to = 2, color = '#e9832d'),
  list(name = 'Agricultores', from = 2, to = 3, color = '#04a494')
)

hcmap(
  "custom/europe",
  data = paises_clust %>%  
    arrange(pais) %>% 
    transmute(grupo = as.numeric(cluster), name = countries) %>% 
    add_row(grupo = c(2, 2), name = c('Slovakia', 'Czech Republic')),
  joinBy = 'name',
  value = 'grupo'
) %>% 
  hc_colorAxis(dataClassColor = "category", dataClasses = data_class)
