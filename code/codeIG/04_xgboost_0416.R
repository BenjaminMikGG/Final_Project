
# STEP 1: LOAD LIBRARIES


library(tidymodels)
library(finetune)
library(vip)
library(xgboost)
library(tidyverse)
library(doParallel)
library(lme4)



# STEP 2: LOAD DATA SET


setwd("C:/Users/agrig/OneDrive - University of Georgia/courses/Spring 2026/CRSS8030/Final_Project")

train_final <- readRDS("data/training/train_final.rds")
test_final  <- read_csv("data/testing/test_final.csv")

dim(train_final)
dim(test_final)
glimpse(train_final)
glimpse(test_final)



# STEP 3: SPLIT DATA INTO TRAINING AND TESTING SETS

set.seed(931735)

yield_split <- initial_split(
  train_final,
  prop = .7,
  strata = yield_mg_ha
)

yield_split



# STEP 4: CREATE TRAINING DATA


yield_train <- training(yield_split)

dim(yield_train)



# STEP 5: CREATE TESTING DATA


yield_test <- testing(yield_split)

dim(yield_test)


# STEP 6: CHECK DISTRIBUTION OF TARGET VARIABLE


ggplot() +
  geom_density(
    data = yield_train,
    aes(x = yield_mg_ha),
    color = "red"
  ) +
  geom_density(
    data = yield_test,
    aes(x = yield_mg_ha),
    color = "blue"
  )



# STEP 7: CREATE DATA PREPROCESSING RECIPE


yield_recipe <- recipe(yield_mg_ha ~ ., data = yield_train) %>%
  step_rm(site, year, hybrid, previous_crop) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors())

yield_recipe



# STEP 8: PREPARE RECIPE


yield_prep <- yield_recipe %>%
  prep()

yield_prep



# STEP 9: MODEL SPECIFICATION (XGBOOST)


xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  learn_rate = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

xgb_spec



# STEP 10: CROSS-VALIDATION SETUP


set.seed(235)

resampling_foldcv <- vfold_cv(
  yield_train,
  v = 10,
  strata = yield_mg_ha
)

resampling_foldcv
resampling_foldcv$splits[[1]]



# STEP 11: CREATE HYPERPARAMETER GRID


xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  learn_rate(),
  trees(),
  size = 50
)

xgb_grid



# STEP 12: VISUALIZE GRID


ggplot(
  data = xgb_grid,
  aes(x = tree_depth, y = min_n)
) +
  geom_point(
    aes(color = factor(learn_rate),
        size = trees),
    alpha = .5,
    show.legend = FALSE
  )



# STEP 13: MODEL TUNING WITH ANOVA RACING


set.seed(76544)

cl <- makePSOCKcluster(parallel::detectCores() - 1)
registerDoParallel(cl)

xgb_res <- tune_race_anova(
  object = xgb_spec,
  preprocessor = yield_recipe,
  resamples = resampling_foldcv,
  grid = xgb_grid,
  control = control_race(save_pred = TRUE)
)

stopCluster(cl)
registerDoSEQ()

xgb_res$.metrics[[2]]



# STEP 14: VISUALIZE RACE RESULTS


plot_race(xgb_res)



# STEP 15: SELECT BEST MODELS

# Based on lowest RMSE
best_rmse <- xgb_res %>%
  select_best(metric = "rmse") %>%
  mutate(source = "best_rmse")

best_rmse

# Based on lowest RMSE within 1% loss
best_rmse_pct_loss <- xgb_res %>%
  select_by_pct_loss("min_n",
                     metric = "rmse",
                     limit = 1
  ) %>%
  mutate(source = "best_rmse_pct_loss")

best_rmse_pct_loss

# Based on lowest RMSE within 1 se
best_rmse_one_std_err <- xgb_res %>%
  select_by_one_std_err(metric = "rmse",
                        eval_time = 100,
                        trees
  ) %>%
  mutate(source = "best_rmse_one_std_err")

best_rmse_one_std_err

# Based on greatest R2
best_r2 <- xgb_res %>%
  select_best(metric = "rsq") %>%
  mutate(source = "best_r2")

best_r2

# Based on lowest R2 within 1% loss
best_r2_pct_loss <- xgb_res %>%
  select_by_pct_loss("min_n",
                     metric = "rsq",
                     limit = 1
  ) %>%
  mutate(source = "best_r2_pct_loss")

best_r2_pct_loss

# Based on lowest R2 within 1 se
best_r2_one_std_error <- xgb_res %>%
  select_by_one_std_err(metric = "rsq",
                        eval_time = 100,
                        trees
  ) %>%
  mutate(source = "best_r2_one_std_error")

best_r2_one_std_error



# STEP 16: COMPARE ALL BEST MODELS


best_rmse %>%
  bind_rows(
    best_rmse_pct_loss,
    best_rmse_one_std_err,
    best_r2,
    best_r2_pct_loss,
    best_r2_one_std_error
  )



# STEP 17: FINAL MODEL SPECIFICATION


final_spec <- boost_tree(
  trees = best_r2$trees,
  tree_depth = best_r2$tree_depth,
  min_n = best_r2$min_n,
  learn_rate = best_r2$learn_rate
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

final_spec



# STEP 18: FINAL FIT ON SPLIT (VALIDATION)


set.seed(10)

final_fit <- last_fit(
  final_spec,
  yield_recipe,
  split = yield_split
)

final_fit %>%
  collect_predictions()



# STEP 19: EVALUATE ON TEST SET

final_fit %>%
  collect_metrics()



# STEP 20: EVALUATE ON TRAINING SET


final_spec %>%
  fit(yield_mg_ha ~ .,
      data = bake(yield_prep,
                  yield_train)) %>%
  augment(new_data = bake(yield_prep,
                          yield_train)) %>%
  rmse(yield_mg_ha, .pred) %>%
  bind_rows(

    final_spec %>%
      fit(yield_mg_ha ~ .,
          data = bake(yield_prep,
                      yield_train)) %>%
      augment(new_data = bake(yield_prep,
                              yield_train)) %>%
      rsq(yield_mg_ha, .pred)
  )



# STEP 21: PREDICTED VS OBSERVED PLOT


final_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = yield_mg_ha,
             y = .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method = "lm")



# STEP 22: VARIABLE IMPORTANCE


final_spec %>%
  fit(yield_mg_ha ~ .,
      data = bake(yield_prep, yield_train)) %>%
  vi() %>%
  mutate(
    Variable = fct_reorder(Variable,
                           Importance)
  ) %>%
  ggplot(aes(x = Importance,
             y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)



# STEP 23: PREDICT ON TEST DATA


final_model <- final_spec %>%
  fit(yield_mg_ha ~ .,
      data = bake(yield_prep, yield_train))

test_baked <- bake(yield_prep, test_final)

test_predictions <- final_model %>%
  augment(new_data = test_baked)

test_predictions


# STEP 24: SAVE PREDICTIONS


write_csv(test_predictions, "output/xgboost_predictions.csv")
