test_pred_full <- propiedades_test %>% 
  filter(lat > -35, lat < -33, lon > -58.55, lon < -57) %>% 
  left_join(test_preds %>% rename(price2=price), by = 'id')

#Cuantos NA hay por barrio??
propiedades_test %>% 
  group_by(barrio) %>% 
  summarise(
    n = n(),
    na = sum(is.na(surface_total)),
    na_prop = round(na/n, 2)
  ) %>%
  arrange(n) %>% 
  View('ts')

extract_price <- function(.title){
  
  if(str_detect(.title, 'usd ')){
    p <- .title %>% str_extract('(?<=usd )\\d+')
    
  }else if(str_detect(.title, 'usd')){
    p <- .title %>% str_extract('(?<=usd)\\d+')
    
  }else if(str_detect(.title, 'u\\$s ')){
    p <- .title %>% str_extract('(?<=u\\$s )\\d+')
    
  }else if(str_detect(.title, 'u\\$s')){
    p <- .title %>% str_extract('(?<=u\\$s)\\d+')
    
  } else{
    p <- .title
  }
  
  suppressWarnings(as.numeric(p))
  
}

# muestra con precio en title
test_pred_full %>% 
  rowwise() %>% 
  mutate(
    title=str_to_lower(title) %>% str_remove_all('\\.'),
    has_surface = !is.na(surface_total),
    title_price= extract_price(title)
  ) %>% 
  filter(!is.na(title_price), title_price>10000) %>% 
  transmute(id, title, 
            price2 = price2/1000, 
            title_price = title_price/1000,
            dif = round(price2-title_price, 0),
            dif_p = round(price2/title_price-1, 3),
            has_surface) %>% 
  ungroup() %>%
  View()
  # rmse(title_price, price2)

# Chequeo individual
propiedades_test %>% 
  filter(id == 49015) %>% 
  transmute(description, rooms, exp(surface_total)) %>% 
  as.list()

test %>% filter(id == 46133) %>% 
  pull(description) %>% extract_surface()

# Quien cono cuesta 10k???
validation %>% filter(exp(price)<20000) %>% 
  pull(description)


# Map hex
test_pred_full %>% 
  # ggplot(aes(lon, lat, z = log(price2))) +
  ggplot(aes(lon, lat, z = surface_total)) +
  stat_summary_hex(alpha = 0.8, bins = 70) +
  scale_fill_viridis_c() +
  labs(fill = "Mean price (log)", title = 'Precio por Hex Test')

propiedades_clean %>%
  # ggplot(aes(lon, lat, z = price)) +
  ggplot(aes(lon, lat, z = price)) +
  stat_summary_hex(alpha = 0.8, bins = 70) +
  scale_fill_viridis_c() +
  labs(fill = "Mean price (log)", title = 'Precio por Hex Train')


# test_pred_full %>% 
propiedades_clean %>% 
  ggplot(aes(rooms, surface_total)) +
  geom_point(alpha=.5) +
  geom_smooth(method='lm')

# Surface vs Price
test_pred_full %>%
  ggplot(aes(log(price2), surface_total)) +
  geom_point(alpha=.5) +
  geom_smooth(method='lm') +
  labs(title='test')

propiedades_clean %>%
  ggplot(aes(price, surface_total)) +
  geom_point(alpha=.5) +
  geom_smooth(method='lm') +
  labs(title='train')

propiedades_test %>% 
  filter(is.na(surface_total)) %>% 
  slice_sample(n=1) %>% 
  select(rooms, bedrooms, bathrooms, description) %>% 
  as.list()

test %>% 
  select(lat, lon) %>%
  naniar::gg_miss_upset()

# lat vs Price
test_pred_full %>%
  ggplot(aes(log(price2), reorder(barrio,log(price2)))) +
  geom_boxplot(alpha=.5) +
  labs(title='test')

propiedades_clean %>%
  ggplot(aes(price, reorder(barrio, price))) +
  geom_boxplot(alpha=.5) +
  labs(title='train')
