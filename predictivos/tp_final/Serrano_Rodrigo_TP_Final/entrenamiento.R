# Librerias----
library(tidyverse)
library(scales)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(geodist)
library(stacks)
library(fuzzyjoin)
library(beepr)
library(ranger)
library(glmnet)
library(vip)
doParallel::registerDoParallel(cores = 4)
# Leer datos----
propiedades_clean <- read_csv('predictivos/tp_final/full_train.csv')
propiedades_test <- read_csv('predictivos/tp_final/test_clean.csv')
no_CABA_path <- 'predictivos/tp_final/tpfinal_nocaba.csv'

# helper functions----

blend_models <- function(model_1, model_2, .train = train, .validation= validation){
  
  m1 <- model_1 %>% filter_parameters(parameters = select_best(model_1))
  m2 <- model_2 %>% filter_parameters(parameters = select_best(model_2))
  
  blended_candidates <- stacks() %>%
    add_candidates(m1) %>%
    add_candidates(m2) %>%
    blend_predictions()
  beepr::beep()
  
  blended_fit <- fit_members(blended_candidates)
  
  blended_candidates_fulldata <- blended_candidates
  blended_candidates_fulldata$train <- bind_rows(.train, .validation)
  blended_fit_fulldata <- fit_members(blended_candidates_fulldata)
  
  list(
    blend_plot = autoplot(blended_candidates, type = 'weights'),
    blended_fit_fulldata = blended_fit_fulldata
  )
  
}

check_model <- function(.model, .validation=validation){
  result <- validation %>%
    bind_cols(predict(.model, .)) %>% 
    rmse(exp(.pred), exp(price))
  
  print(result)
  
  p <- .validation %>%
    bind_cols(predict(.model, .)) %>% 
    ggplot(aes(exp(price), exp(.pred))) +
    geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
    geom_point(alpha = 0.2) +
    scale_x_log10(labels = scales::dollar_format()) +
    scale_y_log10(labels = scales::dollar_format()) +
    coord_fixed()+
    labs(color = NULL, x = "True price", y = "Predicted price", title = 'Dispersion estimates')
  
  plot(p)
  
}

replace_nocaba <- function(df, .path = noCABA_path){
  tpfinal_nocaba <- read_csv(.path) %>%
    mutate(id= as.character(id))
  
  df %>%
    rename(price_1=price) %>%
    left_join(tpfinal_nocaba %>% rename(price_nocaba=price), by='id') %>%
    transmute(
      id, price = if_else(is.na(price_nocaba), price_1, price_nocaba)
    )
}

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

# Split & Cross Validation----
set.seed(123)
.split <- initial_split(propiedades_clean, strata = price)
train <- training(.split)
validation <- testing(.split)
cv_folds <- vfold_cv(train, v = 5, strata = price)

grid_control <- control_grid(save_pred = TRUE,
                             save_workflow = TRUE)

# GLM----

# Workflow
.custom_words <- c('nº', 'av', 'br', 'x', 'o', 'dos', 'tres', 'n', 'hs', 'm')
text_wf <- recipe(price ~ description+lat+lon+surface_total+rooms+bedrooms+ surface_outdoor+
                    bathrooms+barrio+delitos+embajadas+precio_m2_ext+escuelas+
                    subte+hospitales+culturales+wifi+arbolado, data = train) %>%
  step_mutate(description = str_remove_all(description, '\\d|\\n')) %>% 
  step_tokenize(description) %>%
  step_stopwords(description, language = 'es') %>% 
  step_stopwords(description, custom_stopword_source = .custom_words) %>% 
  step_tokenfilter(description, max_tokens = tune()) %>%
  step_tf(description) %>%
  step_impute_knn(surface_total, 
                  impute_with = imp_vars(rooms, bedrooms, bathrooms, lat, lon)) %>% 
  step_impute_knn(surface_outdoor,
                  impute_with = imp_vars(rooms, bedrooms, bathrooms, lat, lon, surface_total)) %>%
  step_mutate(barrio = factor(barrio)) %>%
  step_other(barrio, threshold = .01) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  step_mutate(precio_m2_ext=precio_m2_ext*surface_total) %>% 
  workflow(linear_reg(engine = "glmnet", penalty = tune()))

# Tune
text_tune <- text_wf %>%
  tune_grid(cv_folds,
            control = grid_control,
            metrics = metric_set(rmse),
            grid = crossing(penalty = 10 ^ c(-4),
                            max_tokens = c(200)))
beepr::beep()
autoplot(text_tune)

# Finalize
text_fit <- text_wf %>%
  finalize_workflow(select_best(text_tune)) %>%
  fit(train)

# Audit model
text_fit %>%
  augment(validation) %>%
  rmse(exp(.pred), exp(price))

text_fit$fit$fit$fit %>%
  tidy() %>%
  filter(lambda >= select_best(text_tune)$penalty) %>%
  filter(lambda == min(lambda),
         term != "(Intercept)") %>%
  top_n(50, abs(estimate)) %>%
  mutate(term = fct_reorder(term, estimate)) %>%
  ggplot(aes(estimate, term, fill = estimate > 0)) +
  geom_col()

# Randon Forest----

# Workflow
rf_wf <- recipe(price~lat+lon+surface_total+rooms+bedrooms+
                  bathrooms+barrio+delitos+embajadas+precio_m2_ext+escuelas+surface_outdoor+
                  subte+hospitales+culturales+wifi+arbolado, data = train) %>% 
  step_impute_knn(surface_total, 
                  impute_with = imp_vars(rooms, bedrooms, bathrooms, lat, lon)) %>% 
  step_impute_knn(surface_outdoor, 
                  impute_with = imp_vars(rooms, bedrooms, bathrooms, lat, lon, surface_total)) %>%
  step_mutate(barrio = factor(barrio)) %>%
  step_other(barrio, threshold = .01) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_mutate(precio_m2_ext=precio_m2_ext*surface_total) %>% 
  workflow(
    rand_forest(mtry = tune(), min_n = 5, trees = 1000) %>% 
      set_engine("ranger", importance = 'impurity') %>% 
      set_mode("regression")
  )

# Tune
rf_tune <- rf_wf %>%
  tune_grid(cv_folds,
            control = grid_control,
            metrics = metric_set(rmse),
            grid = crossing(mtry=c(12)))
beepr::beep()
autoplot(rf_tune)

# Finalize
rf_fit <- rf_wf %>%
  finalize_workflow(select_best(rf_tune)) %>%
  fit(train)

rf_fit %>%
  augment(validation) %>%
  rmse(exp(.pred), exp(price))

# Audit model
vip::vip(rf_fit$fit$fit$fit, num_features= 20)
check_model(rf_fit)

# Stack----
final_model <- blend_models(text_tune, rf_tune)
final_model$blend_plot
check_model(final_model$blended_fit)

# Prediccion----

# Prediccion Base
test_preds <- propiedades_test %>%
  bind_cols(predict(final_model$blended_fit_fulldata, .)) %>%
  transmute(id, price=exp(.pred)) %>% 
  replace_nocaba()

# Encontrar Precio en title
price_finder <- propiedades_test %>% 
  rowwise() %>% 
  transmute(
    id,
    title = str_to_lower(title) %>% str_remove_all('\\.'),
    title_price = extract_price(title)
  ) %>% 
  ungroup() %>% 
  filter(!is.na(title_price), title_price>10000)

# Prediccion Final
test_preds %>%
  mutate(id = as.character(id)) %>%
  left_join(price_finder %>% select(id, title_price), by = 'id') %>%
  transmute(id, price = if_else(is.na(title_price), price, title_price)) %>%
  write_csv("predictivos/entregas/tpfinal_entrega.csv")
