library(tidyverse) # Manipulacion de datos
library(tidytuesdayR) # Datos para el problema de PL
library(psych)
library(gt)
library(highcharter)

thematic::thematic_on(
  bg = "#ffffff", fg = '#4a4a4a', accent = '#e9832d', font = 'Roboto'
)
# https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018

tuesdata <- tidytuesdayR::tt_load(2020, week = 8)

food_consumption <- tuesdata$food_consumption %>% 
  mutate(food_category = word(food_category, 1))

# food_consumption %>% write_csv('data/food_consumption.csv')
food_consumption <- read_csv('data/food_consumption.csv')

skimr::skim(food_consumption)

food_consumption %>%
  pivot_longer(-c(country, food_category, source)) %>%
  ggplot(aes(value)) +
  geom_density() +
  facet_wrap(~name) +
  labs(x = '')

food_consumption %>%
  filter(country == 'Argentina') %>% 
  gt::gt()

food_consumption %>%
  pivot_longer(c(consumption, co2_emmission)) %>% 
  ggplot(aes(x = value, y = food_category)) +
  geom_boxplot() +
  facet_wrap(~name, scales = 'free_x', ncol = 2) +
  labs(x = '', y = '')

food_consumption <- food_consumption %>% 
  mutate(
    source = if_else(food_category %in% c('Wheat', 'Rice', 'Nuts', 'Soybeans'), 'non-animal', 'animal')
  ) %>% 
  filter(Milk < 350, Fish < 100)

food_consumption %>% 
  group_by(source) %>% 
  summarise(
    consumption = sum(consumption),
    co2_emmission = sum(co2_emmission),
    .groups = 'drop'
    ) %>% 
  pivot_longer(-source) %>% 
  ggplot(aes(x = value, y = source)) +
  geom_col() +
  facet_wrap(~name, scales = 'free_x', ncol = 2) +
  labs(x = '')

food_consumption %>% 
  group_by(country) %>% 
  summarise(
    consumption = sum(consumption),
    co2_emmission = sum(co2_emmission),
    .groups = 'drop'
  ) %>% 
  ggplot(aes(x = consumption, y = co2_emmission, label = country)) +
  geom_label(size = 3)

food_consumption %>% 
  group_by(food_category) %>% 
  summarise(consumption = sum(consumption)) %>% 
  ggplot(aes(x = reorder(food_category, consumption), y = consumption)) +
  geom_col()

# Consumption Only----
country_consumption <- food_consumption %>% 
  select(-co2_emmission, -source) %>%
  pivot_wider(names_from = food_category, values_from = consumption)

consumption_cor <- country_consumption %>% 
  select(-country) %>% 
  cor()

consumption_cor %>% 
  corrplot::corrplot(
    type = 'lower', diag = F, order = 'hclust', tl.srt = 20, tl.col = 'black',
    col = colorRampPalette(c("#04a494", "#ffffff", "#e9832d"))(20)
    )

# Outliers----
country_variables <- country_consumption %>%
  select(-country)

D2 <- mahalanobis(
  country_variables,
  center = colMeans(country_variables),
  cov = cov(country_variables),
  inverted = FALSE
)

pval <- pchisq(D2, df = ncol(country_variables), lower.tail = FALSE)
# pval < (0.05 / length(D2))

outliers <- which(D2 > qchisq(.999, df = ncol(country_variables)))

country_consumption[outliers,]

country_consumption <- country_consumption[-outliers,]

# Cluster-----

consumption_distance <- country_consumption %>%
  column_to_rownames('country') %>%
  scale() %>%
  dist(method = "euclidean")

cut_hc <- function(.hc, .k = 3, column_id = 'country'){
  cutree(.hc, k = .k) %>%
    as.data.frame() %>%
    rownames_to_column(var = column_id) %>% 
    rename(cluster = '.') %>% 
    mutate(cluster = as_factor(cluster))
}

# Preview cluster count 
list(
  list(
    single = 'single', complete = 'complete', centroid = 'centroid', ward = 'ward.D2'
  ),
  list(consumption_distance)
) %>% 
  pmap_df(.id = 'method', function(.method, .df){
     .df %>% 
      hclust(method = .method) %>% 
      cut_hc(.k = 2) %>% 
      count(cluster)
  }) %>% 
  pivot_wider(names_from = method, values_from = n) %>% 
  gt::gt() %>% 
  gt::tab_header(title = 'Cantidad de paises en cada cluster por Metodo', subtitle = '-')

nb <- country_consumption %>%
  column_to_rownames('country') %>%
  scale() %>%
  NbClust::NbClust(distance = "euclidean", min.nc = 2, max.nc = 4, method = "ward.D2", index = "alllong")

# Final Cluster
hc <- consumption_distance %>% 
  hclust(method = "ward.D2")

clusters <- cut_hc(hc, .k = 3)

# Kmeans
# centroids <- country_consumption %>% 
#   mutate(across(where(is.numeric), scale)) %>% 
#   left_join(clusters, by = 'country') %>% 
#   group_by(cluster) %>% 
#   summarise(
#     across(where(is.numeric), mean)
#   ) %>% 
#   select(-cluster)
# 
# km <- country_consumption %>%
#   column_to_rownames('country') %>% 
#   scale() %>%
#   kmeans(centroids)
#   # kmeans(3)
# 
# clusters <- km$cluster %>% 
#   as.data.frame() %>%
#   rownames_to_column(var = 'country') %>% 
#   rename(cluster = '.') %>% 
#   mutate(cluster = as_factor(cluster))
  
food_consumption %>% 
  group_by(country) %>% 
  summarise(
    consumption = sum(consumption),
    co2_emmission = sum(co2_emmission),
    .groups = 'drop'
  ) %>% 
  left_join(clusters, by = 'country') %>% 
  ggplot(aes(x = consumption, y = co2_emmission, fill = cluster, label = country)) +
  geom_point(aes(color = cluster), size = 4, alpha = .5) +
  scale_fill_manual(values = c("#e9832d", "#04a494", "#4a4a4a")) +
  labs(title = 'Consumo vs Huella de Carbono')

food_consumption %>% 
  group_by(country, source) %>% 
    summarise(
    consumption = sum(consumption),
    .groups = 'drop'
  ) %>% 
  pivot_wider(names_from = source, values_from = consumption) %>% 
  left_join(clusters, by = 'country') %>% 
  ggplot(aes(x = animal, y = `non-animal`, fill = cluster, label = country)) +
  geom_point(aes(color = cluster), size = 4, alpha = .5) +
  scale_fill_manual(values = c("#e9832d", "#04a494", "#4a4a4a")) +
  labs(title = 'Consumo segun origen del Alimento')

clusters %>% 
  group_by(cluster) %>% 
  arrange(desc(country)) %>% 
  mutate(.position = 1:n()) %>%
  ggplot(aes(x = cluster, y = .position, label = country, color = cluster)) +
  geom_text(check_overlap = TRUE) +
  scale_color_manual(values = c("#e9832d", "#04a494", "#4a4a4a")) +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank(),
    axis.title.y = element_blank()
  )

# Map----

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

hdata <- clusters %>% 
  left_join(aux_country, by = 'country') %>% 
  transmute(
    name = if_else(is.na(aux_name), country, aux_name),
    grupo = as.numeric(cluster)
  ) 

# mapdata <- get_data_from_map(download_map_data("custom/world"))
# left_join(mapdata %>% select(name, subregion), by = c('country' = 'name'))
# m %>% filter(is.na(subregion))
# mapdata$name[str_detect(mapdata$name, 'Congo')]

data_class <-list(
  list(name = '1', from = 1, to = 2, color = '#e9832d'),
  list(name = '2', from = 2, to = 3, color = '#04a494'),
  list(name = '3', from = 3, to = 4, color = '#4a4a4a')
)

hcmap(
  "custom/world",
  data = hdata,
  joinBy = 'name',
  value = 'grupo'
) %>% 
  hc_colorAxis(dataClassColor = "category", dataClasses = data_class)

country_consumption %>%
  left_join(clusters, by = 'country') %>%
  group_by(cluster) %>%
  summarise(
    across(where(is.numeric), mean)
  ) %>%
  gt() %>% 
  fmt_number(columns = where(is.numeric), decimals = 1)


# PCA----
fit_pca <- country_consumption %>% 
  column_to_rownames('country') %>% 
  FactoMineR::PCA(scale.unit = T, graph = F)

factoextra::fviz_screeplot(fit_pca, addlabels = TRUE)
factoextra::get_pca_var(fit_pca)$contrib
factoextra::fviz_pca_var(fit_pca, axes = c(1,2), repel = TRUE)
factoextra::fviz_pca_var(fit_pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
factoextra::fviz_cos2(fit_pca, choice = "var", axes = 1:2)
fviz_contrib(fit_pca, choice = "var", axes = 1, top = 15)


factoextra::get_pca_ind(fit_pca)$coord %>% 
  as_tibble(rownames = 'country') %>% 
  left_join(clusters, 'country') %>% 
  ggplot(aes(x = Dim.1, y = Dim.2, color = cluster)) +
  geom_point(aes(color = cluster), size = 4, alpha = .5)