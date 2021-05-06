library(tidyverse) # Manipulacion de datos
library(lpSolveAPI) # Programacion Lineal
library(tidytuesdayR) # Datos para el problema de PL

# https://www.nu3.de/blogs/nutrition/food-carbon-footprint-index-2018

tuesdata <- tidytuesdayR::tt_load(2020, week = 8)

food_consumption <- tuesdata$food_consumption %>% 
  mutate(food_category = word(food_category, 1))

food_consumption %>% write_csv('tecnicas_investigacion/food_consumption.csv')
food_consumption <- read_csv('tecnicas_investigacion/food_consumption.csv')

food_consumption$country %>% unique() %>% length()
food_consumption$food_category %>% unique()

food_consumption %>% filter(country == 'Argentina')

# Objective Function
co2_footprint <- food_consumption %>% 
  filter(country == 'USA') %>%
  transmute(
    food_category,
    kg_co2 = co2_emmission/consumption
  ) %>% 
  arrange(food_category)

obj_function <- co2_footprint$kg_co2
names(obj_function) <- co2_footprint$food_category

# Constrains
# Dietary requirements
nutritional_values <- tibble(
  food_category = unique(food_consumption$food_category) %>% sort(),
  calories_min = c(250, 100, 206, 294, 42, 607, 242, 272, 130, 168, 364)*10,
  proteins = c(26, 13, 22, 25, 3.4, 20, 27, 27, 2.7, 12.4, 12.6)*10,
  potassium = c(.318, .126, .384, .310, .150, .632, .423, .223, .35, .539, .107)*10,
  calories_max = c(250, 100, 206, 294, 42, 607, 242, 272, 130, 168, 364)*10,
  gluten = c(rep(0, 10), 12.4)*10,
  saturated_fat = c(6, 3.3, 2.5, 9, 0.6, 9, 5, 3.8, 0.1, 6.4, 2.5)*10,
  lactose = c(rep(0, 4), 4, rep(0, 6))*10
)

cons_list <- nutritional_values %>% 
  as.list() %>% 
  .[-1]

cons_rhs <- c(
  calories_min = 2000, proteins = 50, potassium = 4.5,
  calories_max = 2500, gluten = 10, saturated_fat = 20, lactose = 15
  )*365

cons_type <- c(rep('>=', 3), rep('<=', 4))

# Limits Boundaries
limit_consumption <- food_consumption %>%
  group_by(food_category) %>% 
  summarise(
    min_consumption = min(consumption),
    max_consumption = max(consumption)
  ) %>% 
  arrange(food_category)

min_co2 <- solve_lp(
  obj_function, cons_list, cons_rhs, cons_type, optimization_type = 'min', model_name = 'CO2 Emmisison',
  .lower = limit_consumption$min_consumption, .upper = limit_consumption$max_consumption
  )
min_co2

min_co2$Resultado.lp$Optimo

min_co2$Resultado.lp$Var.Dec.%*%nutritional_values$proteins

min_co2$SensObj %>%
  as_tibble(rownames = 'food_type') %>% 
  mutate(across(where(is.numeric), ~if_else(abs(.x) > 1e10, Inf, .x))) %>% 
  gt::gt(rowname_col = 'food_type') %>% 
  gt::fmt_number(everything(), decimals = 2) %>% 
  gt::tab_header(title = 'Analisis de Sensibilidad de la Funcion Objetivo')

min_co2$SensRHS %>%
  as_tibble(rownames = 'constrain') %>% 
  mutate(across(where(is.numeric), ~if_else(abs(.x) > 1e10, Inf, .x)/365)) %>% 
  gt::gt(rowname_col = 'constrain') %>% 
  gt::fmt_number(everything(), decimals = 2) %>% 
  gt::tab_header(title = 'Analisis de Sensibilidad RHS')

# Inspect----
food_consumption %>% 
  group_by(country) %>% 
  summarise(co2_emmission = sum(co2_emmission)) %>% 
  ggplot(aes(co2_emmission)) +
  geom_boxplot()

# nutritional_values %>% 
#   bind_rows() %>% 
#   write_csv('tecnicas_investigacion/nutritional_values.csv')
# 
# limit_consumption %>% 
#   write_csv('tecnicas_investigacion/limit_coef.csv')
# 
# tibble(
#   foot_type = names(obj_function),
#   coef = unname(obj_function)
# ) %>% write_csv('tecnicas_investigacion/coef_objective.csv')

