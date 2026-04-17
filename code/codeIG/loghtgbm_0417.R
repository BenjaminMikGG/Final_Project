
# LIGHTGBM MODEL — CORN YIELD PREDICTION


# STEP 1: LOAD LIBRARIES
 install.packages(c("bonsai", "lightgbm", "finetune", "vip"))
library(tidyverse)
library(rsample)
library(recipes)
library(tidymodels)
library(parallel)
library(doParallel)
library(finetune)
library(vip)
library(bonsai)
library(lightgbm)


# STEP 2: LOAD DATA
train_final <- readRDS("data/training/train_final.rds")
test_final  <- read_csv("data/testing/test_final.csv")

dim(train_final); dim(test_final)
glimpse(train_final); glimpse(test_final)


# STEP 3: SPLIT DATA
set.seed(931735)
yield_split <- initial_split(train_final, prop = 0.7, strata = yield_mg_ha)


# STEP 4 & 5: TRAINING AND TESTING DATA
yield_train <- training(yield_split)
yield_test  <- testing(yield_split)


# STEP 6: CHECK DISTRIBUTION
ggplot() +
  geom_density(data = yield_train, aes(x = yield_mg_ha), color = "red") +
  geom_density(data = yield_test,  aes(x = yield_mg_ha), color = "blue") +
  labs(title = "Yield Distribution: Train (red) vs Test (blue)",
       x = "Yield (Mg/ha)", y = "Density")


# STEP 7: RECIPE
# LightGBM handles categoricals natively — no step_dummy() needed
lgbm_recipe <- recipe(yield_mg_ha ~ ., data = yield_train) %>%
  step_rm(site, year, previous_crop) %>%
  step_novel(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors())


# STEP 8: PREP RECIPE
lgbm_prep <- lgbm_recipe %>% prep()


# STEP 9: MODEL SPECIFICATION
lgbm_spec <- boost_tree(
  trees      = tune(),
  tree_depth = tune(),
  min_n      = tune(),
  learn_rate = tune()
) %>%
  set_engine("lightgbm",
             num_threads      = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 4)),
             verbose          = -1,
             feature_fraction = 0.8,
             bagging_fraction = 0.8,
             bagging_freq     = 5) %>%
  set_mode("regression")


# STEP 10: CROSS-VALIDATION
set.seed(235)
resampling_foldcv <- vfold_cv(
  yield_train,
  v      = 5,
  strata = yield_mg_ha
)


# STEP 11: HYPERPARAMETER GRID
lgbm_grid <- grid_latin_hypercube(
  tree_depth(range  = c(3, 8)),
  min_n(range       = c(5, 30)),
  learn_rate(range  = c(-2, -0.5)),
  trees(range       = c(200, 1000)),
  size = 20
)


# STEP 12: VISUALIZE GRID
ggplot(data = lgbm_grid, aes(x = tree_depth, y = min_n)) +
  geom_point(aes(color = factor(round(learn_rate, 2)), size = trees),
             alpha = 0.5, show.legend = FALSE) +
  labs(title = "Hyperparameter Grid", x = "Tree Depth", y = "Min N")


# STEP 13: MODEL TUNING (RACE ANOVA)
set.seed(76544)

n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 4))
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

lgbm_wf <- workflow() %>%
  add_recipe(lgbm_recipe) %>%
  add_model(lgbm_spec)

tryCatch({
  lgbm_res <- tune_race_anova(
    object    = lgbm_wf,
    resamples = resampling_foldcv,
    grid      = lgbm_grid,
    metrics   = metric_set(rmse, rsq, mae),
    control   = control_race(
      save_pred    = TRUE,
      verbose_elim = TRUE,
      burn_in      = 3,
      allow_par    = TRUE
    )
  )
}, finally = {
  stopCluster(cl)
  registerDoSEQ()
})

saveRDS(lgbm_res, "output/lgbm_res.rds")
# lgbm_res <- readRDS("output/lgbm_res.rds")  # uncomment to reload without re-running


# STEP 14: VISUALIZE RACE RESULTS
plot_race(lgbm_res)


# STEP 15: SELECT BEST MODELS
best_rmse <- lgbm_res %>%
  select_best(metric = "rmse") %>%
  mutate(source = "best_rmse")

best_rmse_pct_loss <- lgbm_res %>%
  select_by_pct_loss("min_n", metric = "rmse", limit = 1) %>%
  mutate(source = "best_rmse_pct_loss")

best_rmse_one_std_err <- lgbm_res %>%
  select_by_one_std_err(metric = "rmse", trees) %>%
  mutate(source = "best_rmse_one_std_err")

best_r2 <- lgbm_res %>%
  select_best(metric = "rsq") %>%
  mutate(source = "best_r2")

best_r2_pct_loss <- lgbm_res %>%
  select_by_pct_loss("min_n", metric = "rsq", limit = 1) %>%
  mutate(source = "best_r2_pct_loss")

best_r2_one_std_error <- lgbm_res %>%
  select_by_one_std_err(metric = "rsq", trees) %>%
  mutate(source = "best_r2_one_std_error")


# STEP 16: COMPARE ALL BEST MODELS
best_rmse %>%
  bind_rows(best_rmse_pct_loss, best_rmse_one_std_err,
            best_r2, best_r2_pct_loss, best_r2_one_std_error)


# STEP 17: FINAL MODEL SPECIFICATION
# Using best_r2 — consistent with model selection strategy
final_spec <- boost_tree(
  trees      = best_r2$trees,
  tree_depth = best_r2$tree_depth,
  min_n      = best_r2$min_n,
  learn_rate = best_r2$learn_rate
) %>%
  set_engine("lightgbm",
             num_threads      = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 4)),
             verbose          = -1,
             feature_fraction = 0.8,
             bagging_fraction = 0.8,
             bagging_freq     = 5) %>%
  set_mode("regression")


# STEP 18: FINAL FIT ON SPLIT
set.seed(10)

final_wf <- workflow() %>%
  add_recipe(lgbm_recipe) %>%
  add_model(final_spec)

final_fit <- last_fit(final_wf, split = yield_split)

final_fit %>% collect_predictions()


# STEP 19: EVALUATE ON TEST SET (30% held-out split)
final_fit %>% collect_metrics()


# STEP 20: EVALUATE ON TRAINING SET
train_baked <- bake(lgbm_prep, yield_train)

final_model <- final_spec %>%
  fit(yield_mg_ha ~ ., data = train_baked)

final_model %>%
  augment(new_data = train_baked) %>%
  rmse(yield_mg_ha, .pred) %>%
  bind_rows(
    final_model %>%
      augment(new_data = train_baked) %>%
      rsq(yield_mg_ha, .pred)
  )

# Save LightGBM booster properly (saveRDS loses the booster object)
lgb_booster <- final_model$fit
lightgbm::lgb.save(lgb_booster, "output/lgbm_final_booster.txt")
# lgb_booster <- lightgbm::lgb.load("output/lgbm_final_booster.txt")  # reload


# STEP 21: PREDICTED VS OBSERVED PLOT
final_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = yield_mg_ha, y = .pred)) +
  geom_point(alpha = 0.4) +
  geom_abline(color = "red", linetype = "dashed") +
  geom_smooth(method = "lm") +
  labs(title = "LightGBM: Predicted vs Observed Yield",
       x = "Observed Yield (Mg/ha)",
       y = "Predicted Yield (Mg/ha)")


# STEP 22: VARIABLE IMPORTANCE
final_model %>%
  vi(type = "gain") %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "LightGBM Variable Importance (Gain)", y = NULL)


# STEP 23: PREDICT ON TEST DATA (2024 — no yield labels)
test_baked <- bake(lgbm_prep, test_final)

test_predictions <- final_model %>%
  augment(new_data = test_baked)

test_predictions %>% select(.pred) %>% head(10)


# STEP 24: SAVE PREDICTIONS
test_predictions %>%
  rename(predicted_yield_mg_ha = .pred) %>%
  write_csv("output/lightgbm_predictions.csv")