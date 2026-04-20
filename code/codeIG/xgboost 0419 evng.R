# STEP 1: LOAD LIBRARIES
library(tidymodels)
library(finetune)
library(vip)
library(xgboost)
library(tidyverse)
library(doParallel)

tidymodels_prefer()
dir.create("output", showWarnings = FALSE)


# STEP 2: LOAD DATA
train_final <- readRDS("data/training/train_final.rds")
test_final  <- read_csv("data/testing/test_final.csv", show_col_types = FALSE)

cat("Train:", nrow(train_final), "rows x", ncol(train_final), "cols\n")
cat("Test: ", nrow(test_final),  "rows x", ncol(test_final),  "cols\n")


# STEP 3: CLEAN DATA
clean_data <- function(df) {
  df %>%
    mutate(
      previous_crop = tolower(trimws(previous_crop)),
      previous_crop = if_else(is.na(previous_crop), "unknown", previous_crop),
      previous_crop = case_when(
        str_detect(previous_crop, "soy")         ~ "soybean",
        str_detect(previous_crop, "corn|maize")  ~ "corn",
        str_detect(previous_crop, "wheat")       ~ "wheat",
        str_detect(previous_crop, "fallow|none") ~ "fallow",
        TRUE ~ previous_crop
      ),
      site   = tolower(trimws(site)),
      hybrid = as.character(hybrid)
    )
}

train_final <- clean_data(train_final)
test_final  <- clean_data(test_final)


# STEP 4: TARGET-ENCODE hybrid AND site
global_mean <- mean(train_final$yield_mg_ha, na.rm = TRUE)
k <- 20

hybrid_enc <- train_final %>%
  group_by(hybrid) %>%
  summarise(n = n(), mean_yield = mean(yield_mg_ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(hybrid_encoded = (n * mean_yield + k * global_mean) / (n + k)) %>%
  select(hybrid, hybrid_encoded)

site_enc <- train_final %>%
  group_by(site) %>%
  summarise(site_encoded = mean(yield_mg_ha, na.rm = TRUE), .groups = "drop")

train_final <- train_final %>%
  left_join(hybrid_enc, by = "hybrid") %>%
  left_join(site_enc,   by = "site")

test_final <- test_final %>%
  left_join(hybrid_enc, by = "hybrid") %>%
  left_join(site_enc,   by = "site") %>%
  mutate(
    hybrid_encoded = if_else(is.na(hybrid_encoded), global_mean, hybrid_encoded),
    site_encoded   = if_else(is.na(site_encoded),   global_mean, site_encoded)
  )


# STEP 5: SPLIT DATA
set.seed(931735)
yield_split <- initial_split(train_final, prop = 0.7, strata = yield_mg_ha)
yield_train <- training(yield_split)
yield_test  <- testing(yield_split)


# STEP 6: CHECK DISTRIBUTION
ggplot() +
  geom_density(data = yield_train, aes(x = yield_mg_ha), color = "red") +
  geom_density(data = yield_test,  aes(x = yield_mg_ha), color = "blue")


# STEP 7: RECIPE
derived_cols <- c(
  "total_gdd", "total_prcp", "total_rad",
  "avg_t2m",   "max_t2max",  "min_t2min",
  "gdd_early", "gdd_mid",    "gdd_late",
  "prcp_early","prcp_mid",   "prcp_late"
)

yield_recipe <- recipe(yield_mg_ha ~ ., data = yield_train) %>%
  step_rm(hybrid, site, all_of(derived_cols)) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors())


# STEP 8: PREP RECIPE
yield_prep <- yield_recipe %>% prep()
print(dim(bake(yield_prep, new_data = NULL)))


# STEP 9: MODEL SPECIFICATION
xgb_spec <- boost_tree(
  trees          = tune(),
  tree_depth     = tune(),
  min_n          = tune(),
  learn_rate     = tune(),
  mtry           = tune(),
  loss_reduction = tune(),
  sample_size    = tune()
) %>%
  set_engine("xgboost", nthread = 1) %>%
  set_mode("regression")


# STEP 10: CROSS-VALIDATION
set.seed(235)
resampling_foldcv <- vfold_cv(yield_train, v = 10, strata = yield_mg_ha)


# STEP 11: HYPERPARAMETER GRID
set.seed(42)
xgb_grid <- grid_space_filling(
  tree_depth(range     = c(3L, 10L)),
  min_n(range          = c(2L, 30L)),
  learn_rate(range     = c(-3, -0.5)),
  trees(range          = c(300L, 1500L)),
  mtry(range           = c(5L, 40L)),
  loss_reduction(range = c(-5, 1)),
  sample_size          = sample_prop(range = c(0.5, 1.0)),
  size = 25
)

ggplot(data = xgb_grid, aes(x = tree_depth, y = min_n)) +
  geom_point(aes(color = factor(learn_rate), size = trees),
             alpha = 0.5, show.legend = FALSE)


# STEP 12: MODEL TUNING
set.seed(76544)

n_cores <- as.integer(Sys.getenv("SLURM_CPUS_PER_TASK",
                                 unset = parallel::detectCores(logical = FALSE)))
registerDoParallel(cores = n_cores)

xgb_wf <- workflow() %>%
  add_recipe(yield_recipe) %>%
  add_model(xgb_spec)

xgb_res <- tune_race_anova(
  object    = xgb_wf,
  resamples = resampling_foldcv,
  grid      = xgb_grid,
  metrics   = metric_set(rmse, rsq, mae),
  control   = control_race(
    save_pred    = TRUE,
    verbose_elim = TRUE,
    burn_in      = 4,
    allow_par    = TRUE
  )
)

stopImplicitCluster()
saveRDS(xgb_res, "output/xgb_res.rds")
# xgb_res <- readRDS("output/xgb_res.rds")


# STEP 13: VISUALIZE RACE RESULTS
plot_race(xgb_res)


# STEP 14: SELECT BEST MODELS
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


# STEP 15: COMPARE ALL BEST MODELS
best_rmse %>%
  bind_rows(best_rmse_pct_loss, best_rmse_one_std_err,
            best_r2, best_r2_pct_loss, best_r2_one_std_error)


# STEP 16: FINAL MODEL SPECIFICATION
final_spec <- boost_tree(
  trees          = best_r2$trees,
  tree_depth     = best_r2$tree_depth,
  min_n          = best_r2$min_n,
  learn_rate     = best_r2$learn_rate,
  mtry           = best_r2$mtry,
  loss_reduction = best_r2$loss_reduction,
  sample_size    = best_r2$sample_size
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


# STEP 17: FINAL FIT
set.seed(10)
final_fit <- last_fit(final_spec, yield_recipe, split = yield_split)

final_fit %>% collect_predictions()


# STEP 18: EVALUATE ON TEST SET
final_fit %>% collect_metrics()


# STEP 19: EVALUATE ON TRAINING SET
final_spec %>%
  fit(yield_mg_ha ~ ., data = bake(yield_prep, yield_train)) %>%
  augment(new_data = bake(yield_prep, yield_train)) %>%
  rmse(yield_mg_ha, .pred) %>%
  bind_rows(
    final_spec %>%
      fit(yield_mg_ha ~ ., data = bake(yield_prep, yield_train)) %>%
      augment(new_data = bake(yield_prep, yield_train)) %>%
      rsq(yield_mg_ha, .pred)
  )


# STEP 20: PREDICTED VS OBSERVED PLOT
final_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = yield_mg_ha, y = .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method = "lm")


# STEP 21: VARIABLE IMPORTANCE
final_spec %>%
  fit(yield_mg_ha ~ ., data = bake(yield_prep, yield_train)) %>%
  vip::vi() %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL)


# STEP 22: PREDICT ON TEST DATA
train_full_baked <- bake(yield_prep, new_data = train_final)
test_baked       <- bake(yield_prep, new_data = test_final)

final_model_full <- final_spec %>%
  fit(yield_mg_ha ~ ., data = train_full_baked)

test_predictions <- augment(final_model_full, new_data = test_baked)

summary(test_predictions$.pred)

stopifnot(
  !any(is.na(test_predictions$.pred)),
  all(test_predictions$.pred > 0),
  nrow(test_predictions) == nrow(test_final)
)
cat("All sanity checks passed.\n")

test_final %>%
  select(year, site, hybrid) %>%
  mutate(predicted_yield_mg_ha = test_predictions$.pred) %>%
  write_csv("output/evening_xgboost_predictions_final.csv")

cat("Done! Saved to output/evening_xgboost_predictions_final.csv\n")

saveRDS(final_model_full, "output/xgboost_final_model_evening_0419.rds")
cat("Model saved!\n")

saveRDS(yield_prep, "output/yield_prep_evening_0419.rds")
cat("Prep saved!\n")
