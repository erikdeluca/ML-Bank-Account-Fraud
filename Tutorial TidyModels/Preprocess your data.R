library(tidymodels)      # for the recipes package, along with the rest of tidymodels

# Helper packages
library(nycflights13)    # for flight data
library(skimr)           # for variable summaries


set.seed(123)

flight_data <- 
  flights %>% 
  mutate(
    # Convert the arrival delay to a factor
    arr_delay = ifelse(arr_delay >= 30, "late", "on_time"),
    arr_delay = factor(arr_delay),
    # We will use the date (not date-time) in the recipe below
    date = lubridate::as_date(time_hour)
  ) %>% 
  # Include the weather data
  inner_join(weather, by = c("origin", "time_hour")) %>% 
  # Only retain the specific columns we will use
  select(dep_time, flight, origin, dest, air_time, distance, 
         carrier, date, arr_delay, time_hour) %>% 
  # Exclude missing data
  na.omit() %>% 
  # For creating models, it is better to have qualitative columns
  # encoded as factors (instead of character strings)
  mutate_if(is.character, as.factor)


flight_data %>% 
  count(arr_delay) %>% 
  mutate(prop = n/sum(n))


glimpse(flight_data)


flight_data %>% 
  skimr::skim(dest, carrier) 


# Fix the random numbers by setting the seed 
# This enables the analysis to be reproducible when random numbers are used 
set.seed(222)
# Put 3/4 of the data into the training set 
data_split <- initial_split(flight_data, prop = 3/4)

# Create data frames for the two sets:
train_data <- training(data_split)
test_data  <- testing(data_split)


flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") 


flights_rec %>% 
  summary


flight_data %>% 
  distinct(date) %>% 
  mutate(numeric_date = as.numeric(date)) 


flights_rec <- 
  recipe(arr_delay ~ ., data = train_data) %>% 
  update_role(flight, time_hour, new_role = "ID") %>% 
  step_date(date, features = c("dow", "month")) %>%               
  step_holiday(date, 
               holidays = timeDate::listHolidays("US"), 
               keep_original_cols = FALSE) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors())


test_data %>% 
  distinct(date) %>% 
  anti_join(train_data)


lr_mod <- 
  logistic_reg() %>% 
  set_engine("glm")


flights_wflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(flights_rec)

flights_wflow


flights_fit <- 
  flights_wflow %>% 
  fit(data = train_data)


flights_fit %>% 
  extract_fit_parsnip() %>% 
  tidy()


predict(flights_fit, test_data)


flights_aug <- 
  augment(flights_fit, test_data)

flights_aug %>%
  select(arr_delay, time_hour, flight, .pred_class, .pred_on_time)


flights_aug %>% 
  roc_curve(truth = arr_delay, .pred_late) %>% 
  autoplot()

flights_aug %>% 
  roc_auc(truth = arr_delay, .pred_late)
