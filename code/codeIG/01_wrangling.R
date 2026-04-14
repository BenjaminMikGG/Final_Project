# ============================================================
# Script 01 - Data Wrangling


# 1) Setup ----
library(tidyverse)
library(janitor)


# 2) Load training data ----


# Trait data 
training_trait <- read_csv("data/training/training_trait.csv") %>%
  clean_names()

training_trait

# Meta data 
training_meta <- read_csv("data/training/training_meta.csv") %>%
  clean_names()

training_meta

# Soil data 
training_soil <- read_csv("data/training/training_soil.csv") %>%
  clean_names()

training_soil


# 3) Explore data ----


glimpse(training_trait)
glimpse(training_meta)
glimpse(training_soil)

summary(training_trait)
summary(training_meta)
summary(training_soil)

# Check unique sites and years
training_trait %>% distinct(year) %>% arrange(year)
training_trait %>% distinct(site) %>% arrange(site)
training_trait %>% distinct(year, site) %>% nrow() # site-years

# Check how many unique hybrids
training_trait %>% distinct(hybrid) %>% nrow()

# Distribution of yield
ggplot(training_trait, aes(x = yield_mg_ha)) +
  geom_histogram(bins = 50, fill = "blue", color = "white") +
  labs(title = "Distribution of Corn Yield",
       x = "Yield (Mg/ha)", y = "Count") +
  theme_bw()


# 4) Clean soil data ----


training_soil_clean <- training_soil %>%
  mutate(site = str_remove(site, "_\\d{4}$")) %>%
  rename(
    soil_ph = soilp_h,
    soil_om = om_pct,
    soil_k  = soilk_ppm,
    soil_p  = soilp_ppm
  )

training_soil_clean

# 5) Feature engineering from trait data ----


training_trait_clean <- training_trait %>%
  # Parse dates
  mutate(
    date_planted   = as.Date(date_planted, format = "%m/%d/%y"),
    date_harvested = as.Date(date_harvested, format = "%m/%d/%y")
  ) %>%
  # Growing season length (days from planting to harvest)
  mutate(
    growing_days = as.numeric(date_harvested - date_planted)
  ) %>%
  # Plant month and harvest month as features
  mutate(
    plant_month   = month(date_planted),
    harvest_month = month(date_harvested)
  )

training_trait_clean


# 6) Merge all training files ----
# Join on year + site (common keys across all three files)


training_merged <- training_trait_clean %>%
  # Left join meta (coordinates, previous crop)
  left_join(training_meta, by = c("year", "site")) %>%
  # Left join soil
  left_join(training_soil_clean, by = c("year", "site"))

training_merged

glimpse(training_merged)
summary(training_merged)

# Check NAs
training_merged %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  filter(n_na > 0) %>%
  arrange(desc(n_na))


# 7) Load and merge testing data ----


testing_submission <- read_csv("data/testing/testing_submission.csv", show_col_types = FALSE) %>%
  clean_names()

testing_meta <- read_csv("data/testing/testing_meta.csv", show_col_types = FALSE) %>%
  clean_names()

testing_soil <- read_csv("data/testing/testing_soil.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(site = str_remove(site, "_\\d{4}$")) %>%
  rename(
    soil_ph = soilp_h,
    soil_om = om_pct,
    soil_k  = soilk_ppm,
    soil_p  = soilp_ppm
  )
# Merge test data
testing_merged <- testing_submission %>%
  left_join(testing_meta, by = c("year", "site")) %>%
  left_join(testing_soil, by = c("year", "site"))

testing_merged
glimpse(testing_merged)


# 8) Export merged files ----
# ============================================================

write_csv(training_merged, "data/IGtraining_merged.csv")
write_csv(testing_merged,  "data/IGtesting_merged.csv")

message("Script 01 complete. Files saved to data1/")
