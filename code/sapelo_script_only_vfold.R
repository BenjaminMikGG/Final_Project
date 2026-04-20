library(finetune)     # Additional tuning strategies (e.g., racing, ANOVA-based tuning)
library(vip)          # For plotting variable importance from fitted models
library(xgboost)      # XGBoost implementation in R
library(ranger)       # Fast implementation of Random Forests
library(tidyverse)    # Data wrangling and visualization
library(tidymodels)   # for func：initial_split
library(doParallel)   # For parallel computing (useful during resampling/tuning)
library(here)

corn <- readRDS("../data/training/train_final.rds")
head(corn)

set.seed(123) # Setting seed to get reproducible results 
corn_split <- initial_split(
  corn, 
  # proportion of split, more for test data set cuz we have lots of rows
  prop = .65, 
  strata = yield_mg_ha  # Stratify by target variable
  )
corn_split
# <Training/Testing/Total>
# <106908/57569/164477>

corn_train <- training(corn_split)  # 65% of data
corn_train #This is your training data frame

corn_test <- testing(corn_split)    # 35% of data
corn_test


ggplot() +
  geom_density(data = corn_train, 
               aes(x = yield_mg_ha),
               color = "red") +
  geom_density(data = corn_test, 
               aes(x = yield_mg_ha),
               color = "blue") 
  # almost perfect distribution

# Create recipe for data preprocessing
corn_recipe <- recipe(yield_mg_ha ~ ., data = corn_train) %>% # Remove identifier columns and months 
  step_rm(
    year,       # Remove year identifier
    site,        # Remove site identifier
    hybrid,      #Remove hybrid, cuz too many cols 
    parent1      #parent1
  ) %>%
  step_string2factor(parent2) %>%
  step_unknown(parent2) %>%
  step_unknown(previous_crop) %>%
  step_other(parent2, threshold = 0.01, other = "rare")%>%
  step_dummy(all_nominal_predictors())  # make cat to num for hybrid and pervious crop
corn_recipe

# Prep the recipe to estimate any required statistics
corn_prep <- corn_recipe %>% 
  prep()

# Examine preprocessing steps
corn_prep

xgb_spec <- #Specifying XgBoost as our model type, asking to tune the hyperparameters
  boost_tree(
   # Total number of boosting iterations
    trees = tune(),
         # Maximum depth of each tree
    tree_depth = tune(),
             # Minimum samples required to split a node
    min_n = tune(),
        # Step size shrinkage for each boosting step
    learn_rate = tune()
    ) %>%
        #specify engine 
  set_engine("xgboost") %>%
       # Set to mode
  set_mode("regression")
xgb_spec


set.seed(123)  
resampling_foldcv <- vfold_cv(corn_train, # Create 10-fold cross-validation resampling object from training data
                              v = 10)

resampling_foldcv
resampling_foldcv$splits[[1]]

xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  learn_rate(),
  trees(),
  # 10 for test on our desktop, 1000 for HPC
  size = 10
)

xgb_grid

ggplot(data = xgb_grid,
       aes(x = tree_depth, 
           y = min_n)) +
  geom_point(aes(color = factor(learn_rate),
                 size = trees),
             alpha = .5,
             show.legend = FALSE)

set.seed(123)
registerDoParallel(cores=parallel::detectCores()-1)
xgb_res <- tune_race_anova(object = xgb_spec,
                      preprocessor = corn_recipe,
                      resamples = resampling_foldcv,
                      grid = xgb_grid,
                      control = control_race(save_pred = TRUE))

stopImplicitCluster()

beepr::beep()
xgb_res$.metrics[[2]]

plot_race(xgb_res)

# Based on lowest RMSE
best_rmse <- xgb_res %>% 
  select_best(metric = "rmse")%>% 
  mutate(source = "best_rmse")

best_rmse


# Based on lowers RMSE within 1% loss
best_rmse_pct_loss <- xgb_res %>% 
  select_by_pct_loss("min_n",
                     metric = "rmse",
                     limit = 1
                     )%>% 
  mutate(source = "best_rmse_pct_loss")

best_rmse_pct_loss

# Based on lowest RMSE within 1 se
best_rmse_one_std_err <- xgb_res %>% 
  select_by_one_std_err(metric = "rmse",
                        eval_time = 100,
                        trees
                        )%>% 
  mutate(source = "best_rmse_one_std_err")

best_rmse_one_std_err

# Based on greatest R2
best_r2 <- xgb_res %>% 
  select_best(metric = "rsq")%>% 
  mutate(source = "best_r2")

best_r2

# Based on lowers R2 within 1% loss
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

best_rmse %>% 
  bind_rows(best_rmse_pct_loss, 
            best_rmse_one_std_err, 
            best_r2, 
            best_r2_pct_loss, 
            best_r2_one_std_error)

test_wf <- workflow() %>%
  add_recipe(corn_recipe) %>%
  add_model(final_spec)

test_fit <- fit(test_wf, data = corn_train)

predict(test_fit, new_data = corn_train) %>% head(20)

final_spec <- boost_tree(
  #trees = best_r2$trees,           # Number of boosting rounds (trees)
  trees = 116,
  #tree_depth = best_r2$tree_depth, # Maximum depth of each tree
  tree_depth = 9,
  #min_n = best_r2$min_n,           # Minimum number of samples to split a node
  min_n = 34,
  #learn_rate = best_r2$learn_rate  # Learning rate (step size shrinkage)
  learn_rate = 0.01690976
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

final_spec

set.seed(10)
final_fit <- last_fit(final_spec,
                corn_recipe,
                split = corn_split)

final_fit %>%
  collect_predictions()

# save the model
saveRDS(final_fit, "final_model.rds")

final_fit %>%
  collect_metrics()

final_spec %>%
  fit(yield_mg_ha ~ .,
      data = bake(corn_prep, 
                  corn_train)) %>%
  augment(new_data = bake(corn_prep, 
                          corn_train)) %>% 
  rmse(yield_mg_ha, .pred) %>%
  bind_rows(
    
    
# R2
final_spec %>%
  fit(yield_mg_ha ~ .,
      data = bake(corn_prep, 
                  corn_train)) %>%
  augment(new_data = bake(corn_prep, 
                          corn_train)) %>% 
  rsq(yield_mg_ha, .pred))

final_fit_plot <-final_fit %>%
  collect_predictions() %>%
  ggplot(aes(x = yield_mg_ha,
             y = .pred)) +
  geom_point() +
  geom_abline() +
  geom_smooth(method = "lm") +
  scale_x_continuous(limits = c(1, 25)) +
  scale_y_continuous(limits = c(1, 25)) 

#save the output
ggsave(plot = final_fit_plot, 
       path = here("output", "png"),
       filename = "final_fit_plot.png",
       height = 6,
       width = 9,
       dpi = 600)

final_spec %>%
  fit(yield_mg_ha ~ .,
         data = bake(corn_prep, corn_train)) %>% #There little change in variable improtance if you use full dataset
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

# only show top 20 
final_spec_plot <- final_spec %>%
  fit(
    yield_mg_ha ~ .,
    data = bake(corn_prep, corn_train)
  ) %>%
  vi() %>%
  slice_max(Importance, n = 20) %>%
  mutate(
    Variable = stringr::str_trunc(Variable, width = 25),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable)) +
  geom_col() +
  scale_x_continuous(expand = c(0, 0)) +
  labs(y = NULL) +
  theme(axis.text.y = element_text(size = 8))

# save the output
ggsave(plot = final_spec_plot, 
       path = here("output", "png"),
       filename = "final_spec_plot.png",
       height = 6,
       width = 9,
       dpi = 600)

final_wf <- workflow() %>%
  add_recipe(corn_recipe) %>%
  add_model(final_spec)

final_model <- fit(final_wf, data = corn_train)

saveRDS(final_model, "final_model.rds")

final_model <- readRDS("final_model.rds")

test_data <- read.csv("../data/testing/test_final.csv")

pred_test <- predict(final_model, new_data = test_data)
pred_test

test_submission <- read_csv("../data/testing/testing_submission.csv")

test_out <- bind_cols(test_submission, pred_test) %>%
  dplyr::select(-yield_mg_ha)  %>% 
  rename(yield_mg_ha = .pred)
  

write_csv(test_out,"../data/testing/result1.csv")

knitr::purl("XG-Boost_Training.qmd", output = "sapelo_script_only_vfold.R", documentation = 0)
