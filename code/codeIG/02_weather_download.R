# ============================================================
# Script 02 - Open-Source Weather Data (Daymet)


# 1) Setup ----
library(tidyverse)
library(daymetr)


# 2) Load merged data ----

training_merged <- read_csv("data1/IGtraining_merged.csv", show_col_types = FALSE)
testing_merged  <- read_csv("data1/IGtesting_merged.csv",  show_col_types = FALSE)


# 3) Build site-year coordinate lookup table ----


train_coords <- training_merged %>%
  distinct(year, site, latitude, longitude) %>%
  filter(!is.na(latitude) & !is.na(longitude))

test_coords <- testing_merged %>%
  distinct(year, site, latitude, longitude) %>%
  filter(!is.na(latitude) & !is.na(longitude))

# Combine and filter
all_coords <- bind_rows(train_coords, test_coords) %>%
  distinct(year, site, latitude, longitude) %>%
  filter(longitude < -50)

nrow(all_coords)


daymet_one <- download_daymet(
  lat      = all_coords$latitude[[1]],
  lon      = all_coords$longitude[[1]],
  start    = all_coords$year[[1]],
  end      = all_coords$year[[1]],
  internal = TRUE,
  silent   = TRUE
)

daymet_one$data %>% as_tibble()

# 5) Download Daymet - all site-years ----


safe_daymet <- function(lat, lon, year, site) {
  tryCatch({
    result <- download_daymet(
      lat      = lat,
      lon      = lon,
      start    = year,
      end      = year,
      internal = TRUE,
      silent   = TRUE
    )
    result$data %>%
      as_tibble() %>%
      mutate(year = year, site = site)
  }, error = function(e) {
    message("Failed for site: ", site, " year: ", year)
    NULL
  })
}

daymet_all <- all_coords %>%
  mutate(
    weather = pmap(
      list(lat = latitude, lon = longitude, year = year, site = site),
      safe_daymet
    )
  )


# 6) Unnest and clean weather data ----

daymet_long <- daymet_all %>%
  select(year, site, weather) %>%
  filter(!map_lgl(weather, is.null)) %>%
  mutate(weather = map(weather, ~select(.x, -year, -site))) %>%  # remove duplicate cols
  unnest(weather) %>%
  rename(
    dayl_s   = `dayl..s.`,
    prcp_mm  = `prcp..mm.day.`,
    srad_wm2 = `srad..W.m.2.`,
    swe_kgm2 = `swe..kg.m.2.`,
    tmax_c   = `tmax..deg.c.`,
    tmin_c   = `tmin..deg.c.`,
    vp_pa    = `vp..Pa.`
  ) %>%
  select(year, site, yday, dayl_s, prcp_mm, srad_wm2,
         swe_kgm2, tmax_c, tmin_c, vp_pa)

daymet_long


# 7) Export raw weather data ----


write_csv(daymet_long, "data1/weather_daily.csv")
message("Script 02 complete. Daily weather saved to data1/weather_daily.csv")