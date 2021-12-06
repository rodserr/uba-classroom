library(shiny)
library(tidyverse)
library(bslib)
library(thematic)
library(shinyWidgets)
library(scales)
library(shinycssloaders)
library(workflows)
library(vip)
library(ranger)
library(recipes)
library(htmltools)
library(markdown)

options(spinner.color="#e9832d")

thematic::thematic_on(
  bg = "#ffffff", fg = '#4a4a4a', accent = '#e9832d', font = 'Roboto',
  qualitative = c("#e9832d", "#04a494", "#4a4a4a", "skyblue")
)

# https://archive.ics.uci.edu/ml/datasets/automobile
automobile <- read_csv('data/automobile_dashboard.csv') %>% 
  mutate(
    maker_target = as_factor(maker_target) %>% forcats::lvls_reorder(c(3, 4, 1, 2))
  )

choices_maker <- split(automobile$make, automobile$maker_target) %>% map(unique)

RF <- readRDS('data/karvana_RF.RDS')

predictors_df <- automobile %>% 
  select(-lnprice, -price, -prediction, -error, -maker_target) %>% 
  mutate(across(where(is.character), as_factor))

features_stats <- predictors_df %>% 
  select(where(is.numeric)) %>% 
  summarise(across(everything(), list('mean' = mean, 'sd' = sd), .names = '{.col}-{.fn}')) %>%
  pivot_longer(everything(), names_to = c('variable', 'stat'), names_sep = '-') %>% 
  pivot_wider(names_from = stat, values_from = value)

make_mapper <- automobile %>% distinct(make, maker_target)


make_layout <- function(.list){
  fluidRow(.list)
}

make_ui <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    val <- rng[1]+(rng[2]-rng[1])/2
    widget <- sliderInput(var, var, min = rng[1], max = rng[2], value = val)
  } else if (is.factor(x)) {
    levs <- levels(x)
    widget <- selectInput(var, var, choices = levs, multiple = F)
  } else {
    # Not supported
    NULL
  }
  
  column(2, widget)
  
}

update_ui_input <- function(x, var) {
  if (is.numeric(x)) {
    rng <- range(x, na.rm = TRUE)
    
    val <- runif(1, min = rng[1], max = rng[2])
    
    updateSliderInput(inputId = var, value = val)
    
  } else if (is.factor(x)) {
    val <- levels(x) %>% sample(1)
    updateSelectInput(inputId = var, selected = val)
    
  } else {
    # Not supported
    NULL
  }
  
}
