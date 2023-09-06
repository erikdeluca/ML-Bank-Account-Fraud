library(tidymodels)
cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 500) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

ct_wflw_parallel = ct_wflw %>%
  update_model(rf_mod)

# ct_fit = fit(ct_wflw, data_train)

rf_mod

extract_parameter_set_dials(rf_mod)


set.seed(234)
data_val <- validation_split(data_train, 
                             strata = fraud, 
                             prop = 0.80)
data_val

set.seed(1)
rf_res <- 
  ct_wflw_parallel %>% 
  tune_grid(data_val,
            grid = 5,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))
