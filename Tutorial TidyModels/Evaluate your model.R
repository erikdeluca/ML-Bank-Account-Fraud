library(tidymodels) # for the rsample package, along with the rest of tidymodels

# Helper packages
library(modeldata)  # for the cells data


data(cells, package = "modeldata")
cells


cells %>% 
  count(class) %>% 
  mutate(prop = n/sum(n))


set.seed(123)
cell_split <- initial_split(cells %>% select(-case), 
                            strata = class)


cell_train <- training(cell_split)
cell_test  <- testing(cell_split)

nrow(cell_train)
nrow(cell_train)/nrow(cells)

# training set proportions by class
cell_train %>% 
  count(class) %>% 
  mutate(prop = n/sum(n))

# test set proportions by class
cell_test %>% 
  count(class) %>% 
  mutate(prop = n/sum(n))


rf_mod <- 
  rand_forest(trees = 1000) %>% 
  set_engine("ranger") %>% 
  set_mode("classification")

set.seed(234)
rf_fit <- 
  rf_mod %>% 
  fit(class ~ ., data = cell_train)
rf_fit


rf_training_pred <- 
  predict(rf_fit, cell_train) %>% 
  bind_cols(predict(rf_fit, cell_train, type = "prob")) %>% 
  # Add the true outcome data back in
  bind_cols(cell_train %>% 
              select(class))


rf_training_pred %>%                # training set predictions
  roc_auc(truth = class, .pred_PS)

rf_training_pred %>%                # training set predictions
  accuracy(truth = class, .pred_class)


rf_testing_pred <- 
  predict(rf_fit, cell_test) %>% 
  bind_cols(predict(rf_fit, cell_test, type = "prob")) %>% 
  bind_cols(cell_test %>% select(class))

rf_testing_pred %>% 
  roc_auc(truth = class, .pred_PS)

rf_testing_pred %>% 
  accuracy(truth = class, .pred_class)


set.seed(345)
folds <- vfold_cv(cell_train, v = 10)
folds


rf_wf <- 
  workflow() %>%
  add_model(rf_mod) %>%
  add_formula(class ~ .)


set.seed(456)
rf_fit_rs <- 
  rf_wf %>% 
  fit_resamples(folds)

rf_fit_rs


collect_metrics(rf_fit_rs)


rf_testing_pred %>%                   # test set predictions
  roc_auc(truth = class, .pred_PS)

rf_testing_pred %>%                   # test set predictions
  accuracy(truth = class, .pred_class)
