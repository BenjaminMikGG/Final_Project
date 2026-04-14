
# Script 04 - XGBoost Model Training


# 1) Setup ----
library(tidymodels)
library(tidyverse)
library(vip)
library(xgboost)
library(finetune)


# 2) Load feature-engineered data ----


training_fe <- read_csv("data1/training_fe.csv") %>%
  # Convert character columns to factors for modeling
  mutate(
    prev_crop_clean = as.factor(prev_crop_clean),
    site            = as.factor(site),
    hybrid          = as.factor(hybrid)
  ) %>%
  # Drop rows with missing yield (target variable)
  filter(!is.na(yield_mg_ha))

testing_fe <- read_csv("data1/testing_fe.csv") %>%
  mutate(
    prev_crop_clean = as.factor(prev_crop_clean),
    site            = as.factor(site),
    hybrid          = as.factor(hybrid)
  )

training_fe
glimpse(training_fe)


# 3) Pre-processing: Data split ----
# 70% training / 30% testing

set.seed(931735)

corn_split <- initial_split(
  training_fe,
  prop   = 0.70,
  strata = yield_mg_ha
)

corn_split

corn_train <- training(corn_split)
corn_test  <- testing(corn_split)

corn_train
corn_test

# Check yield distribution across splits
ggplot() +
  geom_density(data = corn_train, aes(x = yield_mg_ha), color = "blue") +
  geom_density(data = corn_test,  aes(x = yield_mg_ha), color = "red") +
  labs(title = "Yield distribution: Train (blue) vs Test (red)",
       x = "Yield (Mg/ha)", y = "Density") +
  theme_bw()


# 4) Pre-processing: Recipe ----

xgb_recipe <- recipe(yield_mg_ha ~ ., data = corn_train) %>%
  # Remove ID columns not useful as predictors
  step_rm(year, site, hybrid) %>%
  # Encode previous crop as dummy variables
  step_dummy(prev_crop_clean, one_hot = TRUE) %>%
  # Remove columns with zero variance (single value)
  step_zv(all_predictors()) %>%
  # Impute NAs with median (for any remaining NAs)
  step_impute_median(all_numeric_predictors())

xgb_recipe

# Prep and bake to check
xgb_prep <- xgb_recipe %>% prep()
xgb_prep

bake(xgb_prep, new_data = NULL) %>% glimpse()

# ============================================================
# 5) Model specification ----
# XGBoost with hyperparameters to tune:
#   - trees (number of boosting rounds)
#   - tree_depth (max depth of each tree)
#   - learn_rate (shrinkage)
#   - mtry (fraction of columns sampled per tree)
#   - min_n (min observations in node)
#   - loss_reduction (gamma - min loss for split)
#   - sample_size (row sampling fraction)
# ============================================================

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


# 6) Resampling strategy: 5-fold cross validation ----


set.seed(34549)

resampling_foldcv <- vfold_cv(corn_train, v = 5, strata = yield_mg_ha)

resampling_foldcv


# 7) Hyperparameter tuning: Simulated annealing ----


set.seed(12345)

xgb_grid_result <- tune_sim_anneal(
  object       = xgb_spec,
  preprocessor = xgb_recipe,
  resamples    = resampling_foldcv,
  iter         = 50,
  metrics      = metric_set(rmse, rsq),
  control      = control_sim_anneal(verbose = TRUE,
                                    no_improve = 10)
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
  labs(title = "XGBoost - RMSE across tuning iterations",
       x = "Iteration", y = "RMSE") +
  theme_bw()

# R2 across iterations
xgb_grid_result %>%
  collect_metrics() %>%
  filter(.metric == "rsq") %>%
  ggplot(aes(x = .iter, y = mean)) +
  geom_line() +
  geom_point(aes(color = mean)) +
  scale_color_viridis_c() +
  labs(title = "XGBoost - R² across tuning iterations",
       x = "Iteration", y = "R²") +
  theme_bw()


# 9) Select best hyperparameters ----

# Best by RMSE
best_rmse_xgb <- xgb_grid_result %>%
  select_by_one_std_err(metric = "rmse", desc(trees))

best_rmse_xgb

# Best by R2
best_r2_xgb <- xgb_grid_result %>%
  select_by_one_std_err(metric = "rsq", desc(trees))

best_r2_xgb

# Use best R2 model
best_params_xgb <- best_r2_xgb


# 10) Final model specification with best hyperparameters ----


final_xgb_spec <- boost_tree(
  trees          = best_params_xgb$trees,
  tree_depth     = best_params_xgb$tree_depth,
  learn_rate     = best_params_xgb$learn_rate,
  mtry           = best_params_xgb$mtry,
  min_n          = best_params_xgb$min_n,
  loss_reduction = best_params_xgb$loss_reduction,
  sample_size    = best_params_xgb$sample_size
) %>%
  set_engine("xgboost", importance = "permutation") %>%
  set_mode("regression")

final_xgb_spec


# 11) Final fit: train on full train set, predict test set ----


final_xgb_fit <- last_fit(
  final_xgb_spec,
  xgb_recipe,
  split = corn_split
)

# Test set metrics
final_xgb_fit %>% collect_metrics()

# Test set predictions
xgb_predictions <- final_xgb_fit %>% collect_predictions()
xgb_predictions




# 12) Predicted vs Observed plot ----


xgb_metrics <- final_xgb_fit %>% collect_metrics()
r2_val   <- xgb_metrics %>% filter(.metric == "rsq")  %>% pull(.estimate) %>% round(3)
rmse_val <- xgb_metrics %>% filter(.metric == "rmse") %>% pull(.estimate) %>% round(3)

ggplot(xgb_predictions, aes(x = yield_mg_ha, y = .pred)) +
  geom_point(alpha = 0.3, size = 0.8, color = "blue") +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 1) +
  labs(
    title = "XGBoost: Predicted vs Observed Yield",
    subtitle = paste0("R² = ", r2_val, "  |  RMSE = ", rmse_val, " Mg/ha"),
    x = "Observed Yield (Mg/ha)",
    y = "Predicted Yield (Mg/ha)"
  ) +
  theme_bw()




# 13) Variable importance ----


# Fit on full training data for importance
xgb_full_fit <- final_xgb_spec %>%
  fit(yield_mg_ha ~ .,
      data = bake(xgb_prep, new_data = training_fe %>%
                    filter(!is.na(yield_mg_ha))))

xgb_importance_plot <- xgb_full_fit %>%
  vip(num_features = 20, geom = "col") +
  labs(title = "XGBoost: Top 20 Variable Importance",
       x = "Importance", y = NULL) +
  theme_bw()

xgb_importance_plot

ggsave("output/xgb_importance.png", xgb_importance_plot,
       width = 8, height = 7, dpi = 300)


# 14) Predict 2024 test set ----


# Prep test data through the same recipe
testing_fe_baked <- bake(xgb_prep,
                          new_data = testing_fe %>%
                            select(-yield_mg_ha))

# Generate predictions
xgb_2024_preds <- predict(xgb_full_fit, new_data = testing_fe_baked)

xgb_2024_preds

# Add predictions to submission file
testing_submission_xgb <- testing_fe %>%
  select(year, site, hybrid) %>%
  bind_cols(xgb_2024_preds) %>%
  rename(yield_mg_ha = .pred)

testing_submission_xgb


# 15) Export results ----


# Save predictions
write_csv(testing_submission_xgb, "output/testing_submission_xgb.csv")

# Save model metrics
xgb_metrics %>%
  write_csv("output/xgb_metrics.csv")

# Save predictions for Shiny app use
write_csv(xgb_predictions, "output/xgb_train_predictions.csv")

message("Script 04 complete. XGBoost model trained and predictions saved.")
