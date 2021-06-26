library(tidyverse)
library(FactoMineR)
library(factoextra)

# Tenure: Indicates the total amount of months that the customer has been with the company.
telco_raw <- read_csv('data/WA_Fn-UseC_-Telco-Customer-Churn.csv')

telco_raw %>% 
  # select(-customerID) %>% 
  mutate(across(where(is.character), as_factor)) %>% 
  skimr::skim()

telco <- telco_raw %>% 
  na.omit() %>% 
  transmute(
    Contract, InternetService, PhoneService, 
    MultipleLines = if_else(MultipleLines == 'No phone service', 'No', MultipleLines), 
    # PaymentMethod = if_else(str_detect(PaymentMethod, 'automatic'), 'auto', 'no_auto'),
    # Companion = if_else(Partner == 'No' & Dependents == 'No', 'No', 'Yes'),
    Streaming = if_else(StreamingTV == 'No' & StreamingMovies == 'No', 'No', 'Yes'),
    SeniorCitizen = if_else(SeniorCitizen == 0, 'No', 'Yes'),
    # OnlineSecurity = if_else(OnlineSecurity == 'No internet service', 'No', OnlineSecurity),
    # TechSupport = if_else(TechSupport == 'No internet service', 'No', TechSupport),
    tenure = case_when(
      tenure < 12*2 ~ 'new_custm',
      tenure < 12*4 ~ 'regular_custm',
      T ~ 'old_custm'
    ),
    MonthlyCharges = case_when(
      MonthlyCharges < 35 ~ 'low_charg',
      MonthlyCharges < 80 ~ 'standard_charg',
      T ~ 'high_charg'
    )
  ) %>% 
  mutate(across(everything(), as_factor))

telco %>% 
  rownames_to_column('id') %>% 
  pivot_longer(-id) %>% 
  count(name, value) %>% 
  ggplot(aes(x = value, y = n)) +
  geom_col() +
  facet_wrap(~name, scales = 'free')

mca <- MCA(telco, ncp = 8, graph = F)

get_eigenvalue(mca)
fviz_screeplot(mca, addlabels = TRUE)

get_mca_var(mca)$coord
fviz_mca_var(mca, axes = c(1,2), choice = "mca.cor", repel = TRUE)
fviz_mca_var(mca, axes = c(1,2), choice = "var.cat", repel = TRUE)
fviz_mca_var(mca, col.var = "cos2", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = TRUE)
fviz_cos2(mca, choice = "var", axes = 1:5)
fviz_contrib(mca, choice = "var", axes = 1:5, top = 15)

get_mca_ind(mca)$coord %>% as_tibble() %>% 
  bind_cols(telco_raw %>% na.omit() %>% dplyr::select(Churn)) %>% 
  ggplot(aes(x = `Dim 1`, y = `Dim 2`, color = Churn)) +
  geom_point()

# Discriminant
telco_numeric <- get_mca_ind(mca)$coord %>%
  as_tibble() %>% 
  bind_cols(telco_raw %>% na.omit() %>% dplyr::select(Churn)) %>% 
  janitor::clean_names() %>% 
  mutate(churn = as_factor(churn))

telco_numeric <- telco %>% 
  bind_cols(telco_raw %>% na.omit() %>% dplyr::select(Churn)) %>% 
  janitor::clean_names() %>% 
  mutate(churn = as_factor(churn))

telco_numeric <- telco_raw %>% 
  mutate(churn = as_factor(Churn)) %>% 
  dplyr::select(-customerID, -Churn, -TotalCharges)

library(MASS)

lda_model <- lda(data = telco_numeric, churn ~ .)
plot(lda_model)

telco_numeric %>% 
  mutate(.predict = predict(lda_model)$class) %>% 
  yardstick::conf_mat(truth = churn, estimate = .predict)

