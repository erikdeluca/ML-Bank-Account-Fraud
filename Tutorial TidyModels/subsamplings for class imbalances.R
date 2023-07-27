library(tune)
library(discrim)
library(klaR)
library(readr)
library(ROSE)
library(themis)
library(tidymodels)

imbal_data <- 
  readr::read_csv("https://tidymodels.org/learn/models/sub-sampling/imbal_data.csv") %>% 
  mutate(Class = factor(Class))
dim(imbal_data)

table(imbal_data$Class)


imbal_rec <- 
  recipe(Class ~ ., data = imbal_data) %>%
  step_rose(Class)


qda_mod <- 
  discrim_regularized(frac_common_cov = 0, frac_identity = 0) %>% 
  set_engine("klaR")


qda_rose_wflw <- 
  workflow() %>% 
  add_model(qda_mod) %>% 
  add_recipe(imbal_rec)
qda_rose_wflw


set.seed(5732)
cv_folds <- vfold_cv(imbal_data, strata = "Class", repeats = 5)


cls_metrics <- metric_set(roc_auc, j_index)


set.seed(2180)
qda_rose_res <- fit_resamples(
  qda_rose_wflw, 
  resamples = cv_folds, 
  metrics = cls_metrics
)

collect_metrics(qda_rose_res)


qda_wflw <- 
  workflow() %>% 
  add_model(qda_mod) %>% 
  add_formula(Class ~ .)

set.seed(2180)
qda_only_res <- fit_resamples(qda_wflw,
                              resamples = cv_folds,
                              metrics = cls_metrics)
collect_metrics(qda_only_res)


no_sampling <- 
  qda_only_res %>% 
  collect_metrics(summarize = F) %>% 
  dplyr::select(-.estimator) %>% 
  mutate(sampling = "no_sampling")

with_sampling <- 
  qda_rose_res %>% 
  collect_metrics(summarize = F) %>% 
  dplyr::select(-.estimator) %>% 
  mutate(sampling = "rose")

bind_rows(no_sampling, with_sampling) %>% 
  mutate(label = paste(id, id2)) %>% 
  ggplot(aes(x = sampling, y =  .estimate, group = label)) +
  geom_line(alpha = .4) + 
  facet_wrap(~ .metric, scales = "free_y")
