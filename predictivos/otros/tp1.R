library(tidyverse)
library(lubridate)
library(tidymodels)
library(rpart)
library(skimr)
library(thematic)

# Plot themes
thematic::thematic_on()

# Read raw udemy
udemy_raw <- readxl::read_excel('data/udemy_entrenamiento.xlsx')

# Tidymodels Workflow----
dt_model <- decision_tree(tree_depth = 10, min_n = 4) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

wflow <- workflow() %>% 
  add_model(dt_model) %>% 
  add_formula(bestseller ~ .)

# Helper to compute accuracy
check_accuracy <- function(.data, .wflow = wflow){
  # Split
  set.seed(2001)
  data_split <- initial_split(.data, prop = .8)
  train <- training(data_split)
  test  <- testing(data_split)
  
  # Fit
  baseline_fit <- fit(.wflow, train)
  
  test_pred <- test %>% 
    mutate(pred = predict(baseline_fit, new_data = .)$.pred_class)
  
  # Throw accuracy
  ac <- test_pred %>% 
    accuracy(truth = bestseller, estimate = pred) %>% 
    pull(.estimate)
  
  cm <- yardstick::conf_mat(test_pred, truth = bestseller, estimate = pred)
  
  list(
    accuracy = ac, 
    fit = baseline_fit, 
    test = test_pred, 
    cm = cm
    )
  
}

# Initial skim & class proportion
skimr::skim(udemy_raw)
table(udemy_raw$bestseller)/nrow(udemy_raw)

# Baseline model----
udemy_baseline <- udemy_raw %>% 
  mutate(across(where(is.character), as_factor))

udemy_baseline_acc <- check_accuracy(udemy_baseline)

# 1st Try--------

skimr::skim(udemy_baseline)

# Helper Content Info
parse_content_duration <- function(.coeff, .type){
  case_when(
    .type == 'preguntas' ~ NA_real_,
    str_detect(.type, 'hora') ~ .coeff*60,
    T ~ .coeff
  )
}

# Instructors
  # Maximum number of ìnstructors in one course
max_instructors <- str_count(udemy_baseline$instructors, '\\,') %>% max(na.rm = T)+1

  # Separate ando pivot
instructors <- udemy_baseline %>% 
  select(id, instructors) %>% 
  separate(
    instructors, sep = '\\,', into = paste('inst', 1:max_instructors), fill = 'right'
  ) %>% 
  pivot_longer(-id, names_to = 'instructor_n', values_to = 'instructor_id', values_drop_na = T) 

  # Treshold to identify experienced instructors
instructors %>% count(instructor_id) %>% pull(n) %>% quantile(.99)
instructors_exp_id <- instructors %>% count(instructor_id) %>% filter(n > 2) %>% pull(instructor_id)

instructors_exp <- instructors %>% 
  mutate(ins_exp = instructor_id %in% instructors_exp_id) %>% 
  group_by(id) %>% 
  summarise(ins_exp = any(ins_exp) %>% as_factor())

instructors_exp %>% count(ins_exp)

  # Build new dataset
useless_feat <- c(
  'id', 'title', 'headline', 'captions', 'last_update_date', 'created_date', 'published_date',
  'content_info_short', 'list_price', 'content_coeff', 'content_type'
  )

udemy_try1 <- udemy_baseline %>% 
  mutate(
    # How many captions in one course
    captions_count = (str_count(captions, '\\,')+1) %>% replace_na(0),
    
    # Parse dates and compute features
    across(ends_with('_date'), ~as.character(.x) %>% lubridate::ymd()),
    last_update_date = if_else(is.na(last_update_date), published_date, last_update_date),
    days_since_last_update = difftime(today(), last_update_date, units = 'days') %>% as.numeric(),
    days_since_created = difftime(today(), created_date, units = 'days') %>% as.numeric(),
    days_since_updated = difftime(last_update_date, published_date, units = 'days') %>% as.numeric(),
    revision_time = difftime(published_date, created_date, units = 'days') %>% as.numeric(),
    published_month = month(published_date) %>% as_factor(),
    
    # Content duration
    content_coeff = content_info_short %>% str_extract('.*(?=\\s)') %>% str_replace('\\,', '\\.') %>% as.numeric(),
    content_type = content_info_short %>% str_extract('(?<=\\s).*'),
    content_duration = parse_content_duration(content_coeff, content_type),
    
    # Parse prices and compute discount percent
    across(ends_with('_price'), ~str_remove(.x, '\\$') %>% as.numeric()),
    discount_price = if_else(is.na(discount_price), list_price, discount_price),
    discount = (list_price-discount_price)/list_price
  ) %>%
  left_join(instructors_exp, by = 'id') %>%
  select(-all_of(useless_feat))
 
skimr::skim(udemy_try1)

udemy_try1_acc <- check_accuracy(udemy_try1)

# To try with rapidminer
udemy_try1 %>% write.csv('predictivos/udemy_try1.csv')

# 2nd Try-----
  # Check densities
udemy_try1 %>% 
  select(where(is.numeric), bestseller) %>% 
  rownames_to_column('id') %>% 
  pivot_longer(-c(id, bestseller)) %>% 
  ggplot(aes(x = value, fill = bestseller)) +
  geom_density(alpha = .4) +
  facet_wrap(~name, scales = 'free')

   # Check contingencia table
udemy_try1 %>% 
  select(where(is.factor), -instructors, -subcategory, -objectives) %>%
  rownames_to_column('id') %>% 
  pivot_longer(-c(id, bestseller)) %>% 
  count(bestseller, name, value) %>%  
  ggplot(aes(y = n, x = value, fill = bestseller)) +
  geom_col() +
  facet_wrap(~name, scales = 'free')
  
  # Buld new dataset
udemy_try2 <- udemy_try1 %>% 
  select(bestseller, ins_exp, instructors, category, objectives, days_since_updated, days_since_last_update, days_since_created)

udemy_try2_acc <- check_accuracy(udemy_try2)

# To try with rapidminer
udemy_try2 %>% write.csv('predictivos/udemy_try2.csv')

skimr::skim(udemy_try2)


# 3rd Try----

instructor_courses <- instructors %>% 
  left_join(
    count(instructors, instructor_id),
    by = 'instructor_id'
  ) %>% 
  group_by(id) %>% 
  summarise(intructor_n_courses = max(n) %>% as_factor())

udemy_try3 <- udemy_try2 %>% 
  rownames_to_column('id') %>% 
  mutate(id = as.numeric(id)) %>% 
  left_join(instructor_courses, by = 'id') %>% 
  select(-id, -days_since_created, -days_since_last_update)

udemy_try3_acc <- check_accuracy(udemy_try3)

# To try with rapidminer
udemy_try3 %>% write.csv('predictivos/udemy_try3.csv')

# 4th Try----
instructor_eff <- udemy_try3 %>%  
  count(instructors, bestseller) %>%
  pivot_wider(values_from = n, names_from = bestseller) %>%
  mutate(
    across(c(no, yes), ~replace_na(.x, 0)),
    eff = yes/(no+yes)
  ) %>% 
  arrange(desc(eff))

instructor_eff %>% ggplot(aes(x = eff)) + geom_density()

udemy_try4 <- udemy_try3 %>% 
  left_join(instructor_eff %>% select(-yes, -no), by = 'instructors') %>% 
  select(-instructors)

udemy_try4_acc <- check_accuracy(udemy_try4)

# To try with rapidminer
udemy_try4 %>% write.csv('predictivos/udemy_try4.csv')

# 5th Try----
objectives_eff <- udemy_try4 %>% 
  count(objectives, bestseller) %>%
  pivot_wider(values_from = n, names_from = bestseller) %>%
  mutate(
    across(c(no, yes), ~replace_na(.x, 0)),
    obj_eff = yes/(no+yes)
  ) %>% 
  arrange(desc(obj_eff))

udemy_try5 <- udemy_try4 %>% 
  select(-objectives)

udemy_try5_acc <- check_accuracy(udemy_try5)

# To try with rapidminer
udemy_try5 %>% write.csv('predictivos/udemy_try5.csv')

# 6th Try----
category_eff <- udemy_try5 %>% 
  count(bestseller, category) %>% 
  pivot_wider(values_from = n, names_from = bestseller) %>%
  mutate(
    across(c(no, yes), ~replace_na(.x, 0)),
    obj_eff = yes/(no+yes)
  ) 

udemy_try6 <- udemy_try5 %>% 
  left_join(category_eff %>% select(-yes, -no), by = 'category') %>% 
  select(-category)

udemy_try6_acc <- check_accuracy(udemy_try6)

# To try with rapidminer
udemy_try6 %>% write.csv('predictivos/udemy_try6.csv')

# Compares Trys-----

list(
  baseline = udemy_baseline_acc, 
  try1 = udemy_try1_acc, 
  try2 = udemy_try2_acc,
  try3 = udemy_try3_acc,
  try4 = udemy_try4_acc,
  try5 = udemy_try5_acc,
  try6 = udemy_try6_acc
  ) %>% 
  map(~.$accuracy)
  # map(~.$cm)

