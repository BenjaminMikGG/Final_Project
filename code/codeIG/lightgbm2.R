# STEP 1: LOAD LIBRARIES
library(tidyverse)
library(tidymodels)
library(finetune)
library(bonsai)
library(lightgbm)
library(parallel)
library(doParallel)


# STEP 2: LOAD DATA
train_final <- readRDS("data/training/train_final.rds")
test_final  <- read_csv("data/testing/test_final.csv", show_col_types = FALSE)


# STEP 3: FIX CASE MISMATCH & TARGET-ENCODE HYBRID + SITE
train_final <- train_final %>%
  mutate(
    previous_crop = tolower(trimws(previous_crop)),
    previous_crop = if_else(is.na(previous_crop), "unknown", previous_crop),
    site          = tolower(trimws(site))
  )

test_final <- test_final %>%
  mutate(
    previous_crop = tolower(trimws(previous_crop)),
    previous_crop = if_else(is.na(previous_crop), "unknown", previous_crop),
    site          = tolower(trimws(site))
  )

# Smoothed target encoding for hybrid (k=20 prevents overfitting on rare hybrids)
global_mean <- mean(train_final$yield_mg_ha, na.rm = TRUE)
k           <- 20

hybrid_enc <- train_final %>%
  group_by(hybrid) %>%
  summarise(n_hyb = n(), mean_hyb = mean(yield_mg_ha, na.rm = TRUE), .groups = "drop") %>%
  mutate(hybrid_encoded = (n_hyb * mean_hyb + k * global_mean) / (n_hyb + k)) %>%
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


# STEP 4: SPLIT DATA
set.seed(931735)
yield_split <- initial_split(train_final, prop = 0.7, strata = yield_mg_ha)
yield_train <- training(yield_split)
yield_test  <- testing(yield_split)


# STEP 5: CHECK DISTRIBUTION
ggplot() +
  geom_density(data = yield_train, aes(x = yield_mg_ha), color = "red") +
  geom_density(data = yield_test,  aes(x = yield_mg_ha), color = "blue") +
  labs(title = "Yield distribution: train (red) vs validation (blue)")


# STEP 6: RECIPE
# Drop hybrid/site (replaced by encoded versions), drop derived/aggregate cols
yield_recipe <- recipe(yield_mg_ha ~ ., data = yield_train) %>%
  step_rm(any_of(c(
    "hybrid", "site",
    "total_gdd", "total_prcp", "total_rad",
    "avg_t2m", "max_t2max", "min_t2min",
    "gdd_early", "gdd_mid", "gdd_late",
    "prcp_early", "prcp_mid", "prcp_late"
  ))) %>%
  step_novel(all_nominal_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors()) %>%
  step_impute_median(all_numeric_predictors())

yield_prep <- yield_recipe %>% prep()


# STEP 7: MODEL SPECIFICATION
lgbm_spec <- boost_tree(
  trees          = tune(),
  tree_depth     = tune(),
  min_n          = tune(),
  learn_rate     = tune(),
  mtry           = tune(),
  loss_reduction = tune(),
  sample_size    = tune()
) %>%
  set_engine("lightgbm", num_threads = 1) %>%
  set_mode("regression")


# STEP 8: CROSS-VALIDATION
set.seed(235)
resampling_foldcv <- vfold_cv(yield_train, v = 5, strata = yield_mg_ha)


# STEP 9: HYPERPARAMETER GRID
set.seed(42)
lgbm_grid <- grid_space_filling(
  tree_depth(range     = c(3L, 10L)),
  min_n(range          = c(2L, 30L)),
  learn_rate(range     = c(-3, -0.5)),
  trees(range          = c(300L, 1500L)),
  mtry(range           = c(5L, 40L)),
  loss_reduction(range = c(-5, 1)),
  sample_size          = sample_prop(range = c(0.5, 1.0)),
  size = 25
)


# STEP 10: MODEL TUNING
set.seed(76544)
cl <- makePSOCKcluster(parallel::detectCores(logical = FALSE))
registerDoParallel(cl)

lgbm_wf <- workflow() %>%
  add_recipe(yield_recipe) %>%
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
      burn_in      = 4,
      allow_par    = TRUE
    )
  )
}, finally = {
  stopCluster(cl)
  registerDoSEQ()
})

saveRDS(lgbm_res, "output/lgbm_res.rds")
# lgbm_res <- readRDS("output/lgbm_res.rds")


# STEP 11: SELECT BEST & FINALIZE
best_rmse <- lgbm_res %>% select_best(metric = "rmse")

final_wf  <- lgbm_wf %>% finalize_workflow(best_rmse)
final_fit <- last_fit(final_wf, split = yield_split)

final_fit %>% collect_metrics()


# STEP 12: PREDICTED VS OBSERVED PLOT
final_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = yield_mg_ha, y = .pred)) +
  geom_point(alpha = 0.3, size = 0.8, color = "blue") +
  geom_abline(linewidth = 0.8) +
  geom_smooth(method = "lm", color = "tomato", se = FALSE) +
  labs(
    title = "LightGBM: Predicted vs Observed (holdout)",
    x = "Observed yield (Mg/ha)", y = "Predicted yield (Mg/ha)"
  ) +
  theme_minimal()


# STEP 13: REFIT ON FULL TRAINING DATA & PREDICT TEST
train_full_baked <- bake(yield_prep, new_data = NULL)
test_baked       <- bake(yield_prep, new_data = test_final)

final_model_full <- extract_spec_parsnip(final_wf) %>%
  fit(yield_mg_ha ~ ., data = train_full_baked)

test_predictions <- augment(final_model_full, new_data = test_baked)

stopifnot(
  !any(is.na(test_predictions$.pred)),
  all(test_predictions$.pred > 0),
  nrow(test_predictions) == nrow(test_baked)
)
cat("Sanity checks passed.\n")


# STEP 14: METRICS
preds    <- final_fit %>% collect_predictions()
rmse_val <- sqrt(mean((preds$yield_mg_ha - preds$.pred)^2))
rsq_val  <- cor(preds$yield_mg_ha, preds$.pred)^2
mae_val  <- mean(abs(preds$yield_mg_ha - preds$.pred))

cat(sprintf("RMSE: %.4f | R2: %.4f | MAE: %.4f\n", rmse_val, rsq_val, mae_val))


# STEP 15: VARIABLE IMPORTANCE
final_model_full %>%
  vip::vi() %>%
  slice_max(Importance, n = 20) %>%
  mutate(Variable = fct_reorder(Variable, Importance)) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col(fill = "blue") +
  scale_x_continuous(expand = c(0, 0)) +
  labs(title = "Top 20 Features — LightGBM", y = NULL) +
  theme_minimal()

ggsave("output/lgbm_importance_evening_0419.png", width = 8, height = 6)


# STEP 16: SAVE RESULTS
dir.create("output", showWarnings = FALSE)

test_final %>%
  select(year, site, hybrid) %>%
  mutate(predicted_yield_mg_ha = test_predictions$.pred) %>%
  write_csv("output/lgbm_predictions_evening_0419.csv")

tibble(model = "LightGBM_evening_0419", rmse = rmse_val, r2 = rsq_val, mae = mae_val) %>%
  write_csv("output/lgbm_metrics_evening_0419.csv")

saveRDS(final_model_full, "output/lgbm_final_model_evening_0419.rds")
saveRDS(yield_prep,       "output/lgbm_yield_prep_evening_0419.rds")

cat("All outputs saved to output/\n")