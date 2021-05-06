library(tidyverse) # Manipulacion de datos
library(tidytuesdayR) # Datos para el problema de PL

# library(FactoMineR)
# library(factoextra)
library(psych)

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
  )

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

# PCA----
fit_pca <- country_consumption %>% 
  select(-country) %>% 
  PCA(scale.unit = T, graph = F)

fviz_screeplot(fit_pca, addlabels = TRUE)
get_pca_var(fit_pca)$coord
fviz_pca_var(fit_pca, axes = c(1,2), repel = TRUE)
fviz_pca_var(fit_pca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_cos2(fit_pca, choice = "var", axes = 1:5)
fviz_contrib(fit_pca, choice = "var", axes = 1, top = 15)

# Analisis Factorial-----
consumption_cor %>% psych::cortest.bartlett(n = nrow(country_consumption))
country_consumption %>% select(-country) %>% KMO()

svd(consumption_cor)$d

fit_pa <- fa(consumption_cor, fm = 'pa', nfactors = 3, rotate = 'varimax', n.obs = nrow(country_consumption))

fit_pa <- principal(consumption_cor, nfactors = 3, rotate = 'varimax', n.obs = nrow(country_consumption))

fit_pa

fit_pa$loadings[1:11,] %>% 
  as.data.frame() %>% 
  rownames_to_column('food_type') %>% 
  ggplot(aes(x = RC1, y = RC2, label = food_type, fill = RC3)) +
  geom_hline(yintercept = 0, color = 'gray') +
  geom_vline(xintercept = 0, color = 'gray') +
  geom_label(color = 'white')

food_consumption %>% 
  group_by(food_category) %>% 
  slice_max(order_by = consumption, n = 5) %>% 
  ggplot(aes(x = consumption, y = reorder(country, consumption))) +
  geom_col() +
  facet_wrap(~food_category, scales = 'free', ncol = 3)

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

# Preview
list(
  list(
    single = 'single', complete = 'complete', centroid = 'centroid', ward = 'ward.D2'
  ),
  list(consumption_distance)
) %>% 
  pmap_df(.id = 'method', function(.method, .df){
     .df %>% 
      hclust(method = .method) %>% 
      cut_hc(.k = 3) %>% 
      count(cluster)
  }) %>% 
  pivot_wider(names_from = method, values_from = n) %>% 
  gt::gt() %>% 
  gt::tab_header(title = 'Cantidad de paises en cada cluster por Metodo', subtitle = '-')

# Final Cluster
hc <- consumption_distance %>% 
  hclust(method = "ward.D2")

as.dendrogram(hc) %>% 
  plot(type = "rectangle")

ape::as.phylo(hc) %>% 
  plot(type = "unrooted", cex = 0.6, no.margin = T, rotate.tree = 90, lab4ut="horizontal", edge.width=1, use.edge.length=F)

ape::as.phylo(hc) %>% 
  plot(type = "fan", cex = 0.6)

clusters <- cut_hc(hc, .k = 3)

food_consumption %>% 
  group_by(country) %>% 
  summarise(
    consumption = sum(consumption),
    co2_emmission = sum(co2_emmission),
    .groups = 'drop'
  ) %>% 
  left_join(clusters, by = 'country') %>% 
  ggplot(aes(x = consumption, y = co2_emmission, fill = cluster, label = country)) +
  geom_label(color = 'white', size = 2.8) +
  scale_fill_manual(values = c("#e9832d", "#04a494", "#4a4a4a"))

food_consumption %>% 
  group_by(country, source) %>% 
  summarise(
    co2_emmission = sum(co2_emmission),
    .groups = 'drop'
  ) %>% 
  pivot_wider(names_from = source, values_from = co2_emmission) %>% 
  left_join(clusters, by = 'country') %>% 
  ggplot(aes(x = animal, y = `non-animal`, fill = cluster, label = country)) +
  geom_label(color = 'white', size = 2.8) +
  scale_fill_manual(values = c("#e9832d", "#04a494", "#4a4a4a"))
