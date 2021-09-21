library(tidyverse)
library(scales)
library(tidymodels)
library(tidytext)
library(textrecipes)
library(beepr)

udemy <- readxl::read_excel('data/fmap-2021-udemy/udemy_entrenamiento.xlsx') %>% 
  unite('title_headline', title, headline, remove = F, sep = ' ') %>%
  filter(!is.na(headline)) %>% 
  mutate(bestseller = as.factor(bestseller))

set.seed(123)
.split <- initial_split(udemy, strata = bestseller, prop = .8)
train <- training(.split)
validation <- testing(.split)

.ntrees <- 100
.max_tokens <- 100

# Title
models <- list(
  
  # Base
  base = recipe(bestseller ~ title+headline+category+rating, data = train) %>%
    step_other(title, headline, threshold = .01),
  
  # Title
  title_1 = recipe(bestseller ~ title+headline+category+rating, data = train) %>%
    step_other(headline, threshold = .01) %>%
    step_tokenize(title) %>%
    step_tokenfilter(title, max_tokens = .max_tokens) %>%
    step_tf(title),
  
  title_2 = recipe(bestseller ~ title+headline+category+rating, data = train) %>%
    step_other(headline, threshold = .01) %>%
    step_tokenize(title) %>%
    step_stopwords(title, language = 'es') %>%
    step_tokenfilter(title, max_tokens = .max_tokens) %>%
    step_tf(title),
  
  title_3 = recipe(bestseller ~ title+headline+category+rating, data = train) %>%
    step_other(headline, threshold = .01) %>%
    step_tokenize(title) %>%
    step_stopwords(title, language = 'es') %>%
    step_stem(title) %>% 
    step_tokenfilter(title, max_tokens = .max_tokens) %>%
    step_tf(title),
  
  title_4 = recipe(bestseller ~ title+headline+category+rating, data = train) %>%
    step_other(headline, threshold = .01) %>%
    step_tokenize(title) %>%
    step_stopwords(title, language = 'es') %>%
    step_stem(title) %>% 
    step_ngram(title) %>% 
    step_tokenfilter(title, max_tokens = .max_tokens) %>%
    step_tf(title),
  
  # Title & Headline
  title_headline_1 = recipe(bestseller ~ title_headline+category+rating, data = train) %>%
    step_tokenize(title_headline) %>%
    step_tokenfilter(title_headline, max_tokens = .max_tokens) %>%
    step_tf(title_headline),
  
  title_headline_2 = recipe(bestseller ~ title_headline+category+rating, data = train) %>%
    step_tokenize(title_headline) %>%
    step_stopwords(title_headline, language = 'es') %>%
    step_tokenfilter(title_headline, max_tokens = .max_tokens) %>%
    step_tf(title_headline),
  
  title_headline_3 = recipe(bestseller ~ title_headline+category+rating, data = train) %>%
    step_tokenize(title_headline) %>%
    step_stopwords(title_headline, language = 'es') %>%
    step_stem(title_headline) %>% 
    step_tokenfilter(title_headline, max_tokens = .max_tokens) %>%
    step_tf(title_headline),
  
  title_headline_4 = recipe(bestseller ~ title_headline+category+rating, data = train) %>%
    step_tokenize(title_headline) %>%
    step_stopwords(title_headline, language = 'es') %>%
    step_stem(title_headline) %>% 
    step_ngram(title_headline) %>% 
    step_tokenfilter(title_headline, max_tokens = .max_tokens) %>%
    step_tf(title_headline)
  
) %>% 
  map(~.x %>% workflow(rand_forest(mode = 'classification', engine = "ranger", trees = .ntrees)) %>%
        fit(train))
beepr::beep()

models %>%
  map_df(
    .id = 'model',
    ~validation %>%
        bind_cols(predict(.x, new_data = ., type = 'prob')) %>% 
        roc_auc(truth = bestseller, estimate = .pred_no)
  ) %>% 
  mutate(
    diff_perc = round(.estimate/0.734-1, 4)*100
  )
  
