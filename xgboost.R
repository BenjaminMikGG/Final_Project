# Script 04 - XGBoost Model Training

# 1) Setup ----
library(tidymodels)
library(tidyverse)
library(vip)
library(xgboost)
library(finetune)

# 2) Load feature-engineered data ----
training_fe <- read_rds("data1/training_fe.rds") %>%
  mutate(
    prev_crop_clean = as.factor(prev_crop_clean),
    site            = as.factor(site),
    hybrid          = as.factor(hybrid)
  ) %>%
  filter(!is.na(yield_mg_ha))

testing_fe <- read_rds("data1/testing_fe.rds") %>%
  mutate(
    prev_crop_clean = as.factor(prev_crop_clean),
    site            = as.factor(site),
    hybrid          = as.factor(hybrid)
  )

training_fe
glimpse(training_fe)

# 3) Data split ----
# 70% training / 30% testing for internal model evaluation
set.seed(931735)

corn_split <- initial_split(
  training_fe,
  prop   = 0.70,
  strata = yield_mg_ha
)

corn_train <- training(corn_split)
corn_test  <- testing(corn_split)

corn_train
corn_test

# Check yield distribution across splits
ggplot() +
  geom_density(data = corn_train, aes(x = yield_mg_ha), color = "blue") +
  geom_density(data = corn_test,  aes(x = yield_mg_ha), color = "red") +
  labs(
    title = "Yield distribution: Train (blue) vs Test (red)",
    x = "Yield (Mg/ha)",
    y = "Density"
  ) +
  theme_bw()

# 4) Recipe ----
xgb_recipe <- recipe(yield_mg_ha ~ ., data = corn_train) %>%
  step_rm(year, site, hybrid) %>%
  step_dummy(prev_crop_clean, one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors())

xgb_recipe

# Optional check
xgb_prep <- prep(xgb_recipe)
xgb_prep
bake(xgb_prep, new_data = NULL) %>% glimpse()

# 5) Model specification ----
xgb_spec <- boost_tree(
  trees          = tune(),
  tree_depth     = tune(),
  learn_rate     = tune(),
  mtry           = tune(),
  min_n          = tune(),
  loss_reduction = tune(),
  sample_size    = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec

# 6) Resampling strategy ----
set.seed(34549)

resampling_foldcv <- vfold_cv(
  corn_train,
  v = 5,
  strata = yield_mg_ha
)

resampling_foldcv

# 7) Hyperparameter tuning: simulated annealing ----
set.seed(12345)

xgb_grid_result <- tune_sim_anneal(
  object       = xgb_spec,
  preprocessor = xgb_recipe,
  resamples    = resampling_foldcv,
  iter         = 50,
  metrics      = metric_set(rmse, rsq),
  control      = control_sim_anneal(
    verbose = TRUE,
    no_improve = 10
  )
)

xgb_grid_result

# 8) Explore tuning results ----
xgb_grid_result %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  ggplot(aes(x = .iter, y = mean)) +
  geom_line() +
  geom_point(aes(color = mean)) +
  scale_color_viridis_c(direction = -1) +
  labs(
    title = "XGBoost - RMSE across tuning iterations",
    x = "Iteration",
    y = "RMSE"
  ) +
  theme_bw()

xgb_grid_result %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  ggplot(aes(x = .iter, y = mean)) +
  geom_line() +
  geom_point(aes(color = mean)) +
  scale_color_viridis_c() +
  labs(
    title = "XGBoost - R² across tuning iterations",
    x = "Iteration",
    y = "R²"
  ) +
  theme_bw()

# 9) Select best hyperparameters ----
# Recommended: use RMSE for final regression model choice
best_rmse_xgb <- xgb_grid_result %>%
  select_best(metric = "rmse")

best_rmse_xgb

#  inspect best by R²
best_r2_xgb <- xgb_grid_result %>%
  select_best(metric = "rsq")

best_r2_xgb

# Use best RMSE model as final choice
best_params_xgb <- best_rmse_xgb

# 10) Final model specification ----
final_xgb_spec <- boost_tree(
  trees          = best_params_xgb$trees,
  tree_depth     = best_params_xgb$tree_depth,
  learn_rate     = best_params_xgb$learn_rate,
  mtry           = best_params_xgb$mtry,
  min_n          = best_params_xgb$min_n,
  loss_reduction = best_params_xgb$loss_reduction,
  sample_size    = best_params_xgb$sample_size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

final_xgb_spec

# 11) Final fit on training split, evaluate on internal test split ----
final_xgb_fit <- last_fit(
  final_xgb_spec,
  xgb_recipe,
  split = corn_split
)

# Test set metrics
xgb_metrics <- final_xgb_fit %>% collect_metrics()
xgb_metrics

# Test set predictions
xgb_predictions <- final_xgb_fit %>% collect_predictions()
xgb_predictions

# 12) Predicted vs Observed plot ----
r2_val <- xgb_metrics %>%
  filter(.metric == "rsq") %>%
  pull(.estimate) %>%
  round(3)

rmse_val <- xgb_metrics %>%
  filter(.metric == "rmse") %>%
  pull(.estimate) %>%
  round(3)

ggplot(xgb_predictions, aes(x = yield_mg_ha, y = .pred)) +
  geom_point(alpha = 0.3, size = 0.8, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    title = "XGBoost: Predicted vs Observed Yield",
    subtitle = paste0("R² = ", r2_val, " | RMSE = ", rmse_val, " Mg/ha"),
    x = "Observed Yield (Mg/ha)",
    y = "Predicted Yield (Mg/ha)"
  ) +
  theme_bw()

# 13) Compare train vs test performance ----
train_fit <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(final_xgb_spec) %>%
  fit(data = corn_train)

train_predictions <- predict(train_fit, new_data = corn_train) %>%
  bind_cols(corn_train %>% select(yield_mg_ha))

train_metrics <- bind_rows(
  train_predictions %>%
    rmse(truth = yield_mg_ha, estimate = .pred) %>%
    mutate(dataset = "train"),
  train_predictions %>%
    rsq(truth = yield_mg_ha, estimate = .pred) %>%
    mutate(dataset = "train"),
  xgb_metrics %>%
    select(.metric, .estimate) %>%
    mutate(dataset = "test")
)

train_metrics

# 14) Fit final workflow on full training_fe data ----
# This model is used for variable importance and 2024 prediction
final_xgb_workflow <- workflow() %>%
  add_recipe(xgb_recipe) %>%
  add_model(final_xgb_spec)

xgb_full_fit <- final_xgb_workflow %>%
  fit(data = training_fe)

# 15) Variable importance ----
xgb_importance_plot <- xgb_full_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20, geom = "col") +
  labs(
    title = "XGBoost: Top 20 Variable Importance",
    x = "Importance",
    y = NULL
  ) +
  theme_bw()

xgb_importance_plot

ggsave(
  "output/xgb_importance.png",
  xgb_importance_plot,
  width = 8,
  height = 7,
  dpi = 300
)

# 16) Predict 2024 external test set ----
# Workflow handles preprocessing internally, so no manual bake needed
xgb_2024_preds <- predict(xgb_full_fit, new_data = testing_fe)

xgb_2024_preds

testing_submission_xgb <- testing_fe %>%
  select(year, site, hybrid) %>%
  bind_cols(xgb_2024_preds) %>%
  rename(yield_mg_ha = .pred)

testing_submission_xgb

# 17) Export results ----
write_csv(testing_submission_xgb, "output/testing_submission_xgb.csv")
write_csv(xgb_metrics, "output/xgb_metrics.csv")
write_csv(xgb_predictions, "output/xgb_train_predictions.csv")
write_csv(train_metrics, "output/xgb_train_vs_test_metrics.csv")

message("Script 04 complete. XGBoost model trained and predictions saved.")

setdiff(names(training_fe), names(testing_fe))
setdiff(names(testing_fe), names(training_fe))
names(testing_fe)
