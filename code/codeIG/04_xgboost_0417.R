# 04_17
#STEP 1: LOAD LIBRARIES
library(tidyverse)
library(ggplot2)
library(dplyr)
library(lme4)
library(rsample)
library(recipes)
library(tidymodels)
library(parallel)
library(doParallel)
library(finetune)
library(vip)


# STEP 2: LOAD DATA SET
train_final <- readRDS("data/training/train_final.rds")
test_final  <- read_csv("data/testing/test_final.csv")

dim(train_final); dim(test_final)
glimpse(train_final); glimpse(test_final)


# STEP 3: SPLIT DATA
set.seed(931735)
yield_split <- initial_split(train_final, prop = .7, strata = yield_mg_ha)


# STEP 4 & 5: TRAINING AND TESTING DATA
yield_train <- training(yield_split)
yield_test  <- testing(yield_split)


# STEP 6: CHECK DISTRIBUTION
ggplot() +
  geom_density(data = yield_train, aes(x = yield_mg_ha), color = "red") +
  geom_density(data = yield_test,  aes(x = yield_mg_ha), color = "blue")

#step 7
yield_recipe <- recipe(yield_mg_ha ~ ., data = yield_train) %>%
  step_rm(site, year, previous_crop) %>%      # remove site/year but KEEP hybrid
  step_novel(all_nominal_predictors()) %>%    # handles new hybrid names in test set
  step_dummy(all_nominal_predictors()) %>%    # converts hybrid to dummy variables
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors())

# STEP 8: PREP RECIPE
yield_prep <- yield_recipe %>% prep()


# STEP 9: MODEL SPECIFICATION
xgb_spec <- boost_tree(
  trees      = tune(),
  tree_depth = tune(),
  min_n      = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost", nthread = 4) %>%
  set_mode("regression")

# STEP 10: CROSS-VALIDATION
set.seed(235)
resampling_foldcv <- vfold_cv(
  yield_train,
  v = 5,
  strata = yield_mg_ha
)


# STEP 11: HYPERPARAMETER GRID
xgb_grid <- grid_latin_hypercube(
  tree_depth(range = c(3, 8)),
  min_n(range = c(5, 30)),
  learn_rate(range = c(-2, -0.5)),
  trees(range = c(200, 1000)),
  size = 20
)


# STEP 12: VISUALIZE GRID
ggplot(data = xgb_grid, aes(x = tree_depth, y = min_n)) +
  geom_point(aes(color = factor(learn_rate), size = trees),
             alpha = .5, show.legend = FALSE)



# STEP 13: MODEL TUNING
set.seed(76544)

n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", unset = 4))
cl <- makePSOCKcluster(n_cores)
registerDoParallel(cl)

xgb_wf <- workflow() %>%
  add_recipe(yield_recipe) %>%
  add_model(xgb_spec)

tryCatch({
  xgb_res <- tune_race_anova(
    object    = xgb_wf,
    resamples = resampling_foldcv,
    grid      = xgb_grid,
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

saveRDS(xgb_res, "output/xgb_res.rds")
# xgb_res <- readRDS("output/xgb_res.rds")  # uncomment to reload

# STEP 14: VISUALIZE RACE RESULTS
plot_race(xgb_res)


# STEP 15: SELECT BEST MODELS
best_rmse <- xgb_res %>%
  select_best(metric = "rmse") %>%
  mutate(source = "best_rmse")

best_rmse_pct_loss <- xgb_res %>%
  select_by_pct_loss("min_n", metric = "rmse", limit = 1) %>%
  mutate(source = "best_rmse_pct_loss")

best_rmse_one_std_err <- xgb_res %>%
  select_by_one_std_err(metric = "rmse", trees) %>%
  mutate(source = "best_rmse_one_std_err")

best_r2 <- xgb_res %>%
  select_best(metric = "rsq") %>%
  mutate(source = "best_r2")

best_r2_pct_loss <- xgb_res %>%
  select_by_pct_loss("min_n", metric = "rsq", limit = 1) %>%
  mutate(source = "best_r2_pct_loss")

best_r2_one_std_error <- xgb_res %>%
  select_by_one_std_err(metric = "rsq", trees) %>%
  mutate(source = "best_r2_one_std_error")


# STEP 16: COMPARE ALL BEST MODELS
best_rmse %>%
  bind_rows(best_rmse_pct_loss, best_rmse_one_std_err,
            best_r2, best_r2_pct_loss, best_r2_one_std_error)


# STEP 17: FINAL MODEL SPECIFICATION
final_spec <- boost_tree(
  trees      = best_r2$trees,
  tree_depth = best_r2$tree_depth,
  min_n      = best_r2$min_n,
  learn_rate = best_r2$learn_rate
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


# STEP 18: FINAL FIT ON SPLIT
set.seed(10)

final_wf <- workflow() %>%
  add_recipe(yield_recipe) %>%
  add_model(final_spec)

final_fit <- last_fit(final_wf, split = yield_split)

final_fit %>% collect_predictions()


# STEP 19: EVALUATE ON TEST SET
final_fit %>% collect_metrics()


# STEP 20: EVALUATE ON TRAINING SET
train_baked <- bake(yield_prep, yield_train)

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


# STEP 21: PREDICTED VS OBSERVED PLOT
final_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = yield_mg_ha, y = .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method = "lm")


# STEP 22: VARIABLE IMPORTANCE
final_model %>%
  vi() %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)


# STEP 23: PREDICT ON TEST DATA
test_baked <- bake(yield_prep, test_final)

test_predictions <- final_model %>%
  augment(new_data = test_baked)

test_predictions %>% select(.pred) %>% head(10)


# STEP 24: SAVE PREDICTIONS
test_predictions %>%
  rename(predicted_yield_mg_ha = .pred) %>%
  write_csv("output/xgboost_predictions.csv")