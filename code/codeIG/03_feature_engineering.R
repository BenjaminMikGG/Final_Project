# ============================================================
# Script 03 - Feature Engineering


# 1) Setup ----
library(tidyverse)
library(lubridate)


# 2) Load data ----
# ============================================================

weather_daily   <- read_csv("data1/weather_daily.csv",   show_col_types = FALSE)
training_merged <- read_csv("data1/training_merged.csv", show_col_types = FALSE)
testing_merged  <- read_csv("data1/testing_merged.csv",  show_col_types = FALSE)

glimpse(weather_daily)


# 3) Feature engineering - weather (monthly summaries) ----


fe_weather <- weather_daily %>%
  mutate(
    date  = as.Date(paste0(year, "-01-01")) + days(yday - 1),
    month = month(date, label = TRUE, abbr = TRUE)
  ) %>%
  filter(month %in% c("Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct")) %>%
  group_by(year, site, month) %>%
  summarise(
    tmax_c_mean   = mean(tmax_c,              na.rm = TRUE),
    tmin_c_mean   = mean(tmin_c,              na.rm = TRUE),
    tmean_c       = mean((tmax_c + tmin_c)/2, na.rm = TRUE),
    prcp_mm_sum   = sum(prcp_mm,              na.rm = TRUE),
    srad_wm2_mean = mean(srad_wm2,            na.rm = TRUE),
    vp_pa_mean    = mean(vp_pa,               na.rm = TRUE),
    dayl_s_mean   = mean(dayl_s,              na.rm = TRUE),
    .groups = "drop"
  )

# Pivot wider: each month-variable becomes its own column
fe_weather_wide <- fe_weather %>%
  pivot_wider(
    names_from  = month,
    values_from = c(tmax_c_mean, tmin_c_mean, tmean_c,
                    prcp_mm_sum, srad_wm2_mean, vp_pa_mean, dayl_s_mean),
    names_glue  = "{.value}_{month}"
  )

fe_weather_wide


# 4) Feature engineering - hybrid and site summaries ----


hybrid_means <- training_merged %>%
  group_by(hybrid) %>%
  summarise(
    hybrid_mean_yield = mean(yield_mg_ha, na.rm = TRUE),
    hybrid_n_trials   = n(),
    .groups = "drop"
  )

site_means <- training_merged %>%
  group_by(site) %>%
  summarise(
    site_mean_yield = mean(yield_mg_ha, na.rm = TRUE),
    site_n_years    = n_distinct(year),
    .groups = "drop"
  )

# 5) Encode previous crop ----


training_merged %>% count(previous_crop) %>% arrange(desc(n))

encode_prev_crop <- function(df) {
  df %>%
    mutate(
      prev_crop_clean = case_when(
        str_to_lower(previous_crop) %in% c("soybean", "soybeans") ~ "soybean",
        str_to_lower(previous_crop) %in% c("corn")                ~ "corn",
        str_to_lower(previous_crop) %in% c("cotton")              ~ "cotton",
        str_to_lower(previous_crop) %in% c("wheat")               ~ "wheat",
        str_to_lower(previous_crop) %in% c("peanut", "peanuts")   ~ "peanut",
        TRUE                                                        ~ "other"
      )
    )
}

training_merged <- training_merged %>% encode_prev_crop()
testing_merged  <- testing_merged  %>% encode_prev_crop()


# 6) Build final feature-engineered training dataset ----


training_fe <- training_merged %>%
  left_join(fe_weather_wide, by = c("year", "site")) %>%
  left_join(hybrid_means,    by = "hybrid") %>%
  left_join(site_means,      by = "site") %>%
  select(
    year, site, hybrid,
    yield_mg_ha,
    grain_moisture, growing_days, plant_month, harvest_month,
    soil_ph, soil_om, soil_k, soil_p,
    latitude, longitude,
    prev_crop_clean,
    hybrid_mean_yield, hybrid_n_trials,
    site_mean_yield, site_n_years,
    starts_with("tmax_c_mean_"),
    starts_with("tmin_c_mean_"),
    starts_with("tmean_c_"),
    starts_with("prcp_mm_sum_"),
    starts_with("srad_wm2_mean_"),
    starts_with("vp_pa_mean_"),
    starts_with("dayl_s_mean_")
  )

glimpse(training_fe)

# Check NAs
training_fe %>%
  summarise(across(everything(), ~sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_na") %>%
  filter(n_na > 0) %>%
  arrange(desc(n_na))


# 7) Build final feature-engineered testing dataset ----


testing_fe <- testing_merged %>%
  left_join(fe_weather_wide, by = c("year", "site")) %>%
  left_join(hybrid_means,    by = "hybrid") %>%
  left_join(site_means,      by = "site") %>%
  select(
    year, site, hybrid,
    yield_mg_ha,
    soil_ph, soil_om, soil_k, soil_p,
    latitude, longitude,
    prev_crop_clean,
    hybrid_mean_yield, hybrid_n_trials,
    site_mean_yield, site_n_years,
    starts_with("tmax_c_mean_"),
    starts_with("tmin_c_mean_"),
    starts_with("tmean_c_"),
    starts_with("prcp_mm_sum_"),
    starts_with("srad_wm2_mean_"),
    starts_with("vp_pa_mean_"),
    starts_with("dayl_s_mean_")
  )

glimpse(testing_fe)


# 8) Export feature-engineered datasets ----


write_csv(training_fe, "data1/training_fe.csv")
write_csv(testing_fe,  "data1/testing_fe.csv")

message("Script 03 complete. Feature-engineered data saved to data1/")