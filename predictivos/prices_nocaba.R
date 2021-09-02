propiedades_test %>%
  filter(lon<(-60)) %>%
  ggplot(aes(lon, lat)) +
  geom_point(alpha=.4)

propiedades %>%
  filter(lon<(-60)) %>%
  ggplot(aes(lon, lat, color = log(price))) +
  geom_point(alpha=.4)

test_no_caba <- propiedades_test %>% 
  filter(lon<(-60))

train_no_caba <- propiedades %>%
  filter(lon<(-60))

train_no_caba %>% 
  select(lat, lon, price, surface_total, rooms, bedrooms, bathrooms, description) %>% 
  arrange(lat, lon) %>% 
  View('train_nc')

test_no_caba %>% 
  select(lat, lon, surface_total, rooms, bedrooms, bathrooms, description, id) %>% 
  arrange(lat, lon) %>% 
  View('test_nc')

tpfinal_nocaba <- read_csv('predictivos/entregas/tpfinal_nocaba.csv')
tpfinal_1 <- read_csv('predictivos/entregas/tpfinal_1.csv')

tpfinal_nocaba %>% left_join(tpfinal_1 %>% rename(price_1=price), by='id')

tpfinal_3 <- tpfinal_1 %>%
  rename(price_1=price) %>% 
  left_join(tpfinal_nocaba %>% rename(price_nocaba=price), by='id') %>%
  transmute(
    id, price = if_else(is.na(price_nocaba), price_1, price_nocaba)
  )

tpfinal_3 %>% write_csv('predictivos/entregas/tpfinal_3.csv')

tpfinal_3 %>% ggplot(aes(log(price+1))) + geom_histogram()+labs(title = 'pred')

propiedades %>% ggplot(aes(log(price+1))) +geom_histogram()+labs(title='train')
