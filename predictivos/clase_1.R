library(tidyverse)
library(tidymodels)

rrhh <- readxl::read_excel('predictivos/RRHH.xlsx') 

skimr::skim(rrhh)

rh <- rrhh %>% 
  mutate(
    across(where(is.character), as.factor),
    aumentos_ultimos_2_años = as_factor(aumentos_ultimos_2_años),
    años_en_la_empresa = case_when(
      años_en_la_empresa >= 5 ~ 'old',
      años_en_la_empresa > 3 ~ 'recent',
      T ~ 'new'
    ) %>% as_factor(),
    cantidad_proyectos = case_when(
      cantidad_proyectos >= 4 ~ 'muchos',
      T ~ 'pocos'
    ) %>% as_factor(),
    area = case_when(
      area == 'fabricación' ~ 'fab',
      T ~ 'admin'
    ) %>% as_factor()
  )

skimr::skim(rh)

data_split <- initial_split(rrhh, prop = 3/4, strata = se_fue)

train_data <- training(data_split)
test_data  <- testing(data_split)

cvfolds <- vfold_cv(train_data, v = 2, repeats = 1)

rf_cla_model <- rand_forest(trees = 1000, mtry = tune(), min_n = tune()) %>% 
  set_engine("ranger", importance = "impurity", 
             regularization.factor = tune("regularization"), verbose = TRUE) %>% 
  set_mode("classification")

rec <- recipe(se_fue ~ ., data = train_data) %>% 
  step_dummy(all_nominal_predictors())

rf_cla_wflow <- workflow() %>% 
  add_model(rf_cla_model) %>% 
  add_recipe(rec)

rf_cla_param <- rf_cla_wflow %>% 
  parameters() %>%
  finalize(train_data) %>% 
  grid_regular(levels = c(mtry = 2, min_n = 1, regularization = 1))

.start_rf_cla <- Sys.time()
rf_cla_res <- rf_cla_wflow %>%
  tune_grid(
    resamples = cvfolds,
    grid = rf_cla_param,
    control = control_resamples(save_pred = TRUE)
  )
Sys.time()-.start_rf_cla

show_best(rf_cla_res)
collect_metrics(rf_cla_res)
autoplot(rf_cla_res)


