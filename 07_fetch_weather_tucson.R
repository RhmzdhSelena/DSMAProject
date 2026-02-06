###############################################################
# Script: 07_fetch_weather_tucson.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Fetch, clean, and enrich Tucson daily weather data using the
#   NOAA GHCN API. Missing base weather observations are imputed
#   using MICE (continuous imputation). Lagged features are then
#   regenerated AFTER imputation. Because lagged variables produce
#   structural NA values at the beginning of the series (not true
#   missing data), these NA values are deterministically filled,
#   ensuring zero missing values in all weather variables.
#
# Notes:
#   - Missing base weather values (PRCP, SNOW, SNWD, TMAX, TMIN, TAVG)
#     are treated as MAR/MCAR and are suitable for MICE.
#   - Lag variables (TMAX_lag1, lag2, lag3, etc.) produce NA only
#     because no earlier day exists → this is not real missingness.
#   - Therefore lag-NAs must NOT be imputed with MICE.
#   - Deterministic lag fill strategy:
#       lag1 NA → replace with same-day series value
#       lag2 NA → replace with lag1
#       lag3 NA → replace with lag2
#     This yields a complete weather feature table without adding
#     statistical noise to structural boundary values.
#
# Inputs:
#   - daily_features_tucson.csv   (to determine the panel date range)
#
# Outputs:
#   - Working ML table (Data/Data): weather_tucson_daily.csv
#   - Figures (appendix):          ./figures/weather/...
#   - Tables (appendix):           ./results/tables/script07_weather_missingness_pre_imputation.csv
###############################################################

###############################################################
# Setup
###############################################################

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
cat("Working directory:", getwd(), "\n")

library(dplyr)
library(readr)
library(lubridate)
library(httr)
library(jsonlite)
library(ggplot2)
library(tidyr)
library(mice)
library(zoo)

## -------------------------------------------------------------
## Output folders (project convention)
## -------------------------------------------------------------
# Weather is a core ML input table, so the final dataset is saved
# in the working data directory (Data/Data). Diagnostic figures and
# descriptive tables for the appendix are stored separately.

FIG_DIR     <- file.path(getwd(), "figures", "weather")
RESULTS_DIR <- file.path(getwd(), "results")
TABLE_DIR   <- file.path(RESULTS_DIR, "tables")

dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

###############################################################
# 1. Load CLEAN behavioral panel (to get date range)
###############################################################

daily_features <- read_csv("daily_features_tucson.csv", show_col_types = FALSE)

start_date <- min(daily_features$date, na.rm = TRUE)
end_date   <- max(daily_features$date, na.rm = TRUE)

cat("Weather extraction range:", start_date, "→", end_date, "\n")

###############################################################
# 2. NOAA station (Tucson International Airport)
###############################################################

station_id <- "USW00023160"
cat("Using NOAA station:", station_id, "\n")

###############################################################
# 3. NOAA API Request
###############################################################

base_url <- "https://www.ncei.noaa.gov/access/services/data/v1"

params <- list(
  dataset = "daily-summaries",
  stations = station_id,
  startDate = as.character(start_date),
  endDate = as.character(end_date),
  dataTypes = "PRCP,SNOW,SNWD,TMAX,TMIN",
  includeStationName = "false",
  includeStationLocation = "false",
  units = "metric",
  format = "json"
)

response <- GET(base_url, query = params)
stop_for_status(response)

weather_raw <- fromJSON(content(response, "text"), flatten = TRUE)
cat("Downloaded", nrow(weather_raw), "daily raw weather rows.\n")

###############################################################
# 4. Clean and sanitize data
###############################################################

weather <- weather_raw %>%
  mutate(
    DATE = as.Date(DATE),
    
    # Handle NOAA "Trace" values (treat as NA for numeric conversion)
    PRCP = na_if(PRCP, "Trace"),
    SNOW = na_if(SNOW, "Trace"),
    SNWD = na_if(SNWD, "Trace"),
    
    # Convert to numeric
    PRCP = suppressWarnings(as.numeric(PRCP)),
    SNOW = suppressWarnings(as.numeric(SNOW)),
    SNWD = suppressWarnings(as.numeric(SNWD)),
    TMAX = suppressWarnings(as.numeric(TMAX)),
    TMIN = suppressWarnings(as.numeric(TMIN))
  ) %>%
  arrange(DATE) %>%
  mutate(
    # Create TAVG (Average Temp)
    TAVG = (TMAX + TMIN) / 2
  )

###############################################################
# 5. Diagnostics & Audit Trail (Pre-Imputation)
###############################################################
# The goal is to document the extent and structure of missingness
# before imputing base weather measurements.

# A. Missingness flags (pre-imputation)
weather <- weather %>%
  mutate(
    miss_TMAX = if_else(is.na(TMAX), 1L, 0L),
    miss_TMIN = if_else(is.na(TMIN), 1L, 0L),
    miss_PRCP = if_else(is.na(PRCP), 1L, 0L),
    miss_TAVG = if_else(is.na(TAVG), 1L, 0L)
  )

# B. Save diagnostic plots (appendix-ready)
p_tmax <- ggplot(weather, aes(x = TMAX)) +
  geom_histogram(bins = 60, fill = "#4D96FF", alpha = 0.8) +
  labs(title = "Distribution of TMAX (Pre-Imputation)", x = "TMAX (°C)", y = "Count") +
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(FIG_DIR, "dist_TMAX_pre_imputation.png"),
  plot = p_tmax,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

p_prcp <- ggplot(weather, aes(x = PRCP)) +
  geom_histogram(bins = 60, fill = "#6BCB77", alpha = 0.8) +
  labs(title = "Distribution of PRCP (Pre-Imputation)", x = "PRCP (mm)", y = "Count") +
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(FIG_DIR, "dist_PRCP_pre_imputation.png"),
  plot = p_prcp,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

p_missing <- weather %>%
  ggplot(aes(x = DATE)) +
  geom_line(aes(y = miss_TMAX), color = "#FF6B6B") +
  geom_line(aes(y = miss_TMIN), color = "#845EC2") +
  geom_line(aes(y = miss_PRCP), color = "#FFC75F") +
  labs(
    title = "Missingness Indicators Over Time (Pre-Imputation)",
    subtitle = "1 = Missing, 0 = Present",
    x = "Date",
    y = "Missingness Flag"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(FIG_DIR, "missingness_time_pre_imputation.png"),
  plot = p_missing,
  width = 8,
  height = 5,
  dpi = 300,
  bg = "white"
)

cat("Diagnostics saved to:", FIG_DIR, "\n")

###############################################################
# 6. MICE imputation for base weather variables
###############################################################
# We impute only the base continuous measures (not dates, not flags,
# and not lagged variables). After imputation, lag features are built
# deterministically from the completed series.

mice_vars <- weather %>%
  select(PRCP, SNOW, SNWD, TMAX, TMIN, TAVG)

# Export a missingness summary table for the appendix (pre-imputation)
miss_tbl <- mice_vars %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(share_missing = n_missing / nrow(mice_vars))

write_csv(miss_tbl, file.path(TABLE_DIR, "script07_weather_missingness_pre_imputation.csv"))

cat("Missing values BEFORE MICE:\n")
print(colSums(is.na(mice_vars)))

set.seed(123)
mice_mod <- mice(mice_vars, m = 5, maxit = 20, method = "pmm", printFlag = TRUE)
mice_complete <- complete(mice_mod, 1)

# Overwrite (impute only where missing; completed dataset contains full series)
weather$PRCP <- mice_complete$PRCP
weather$SNOW <- mice_complete$SNOW
weather$SNWD <- mice_complete$SNWD
weather$TMAX <- mice_complete$TMAX
weather$TMIN <- mice_complete$TMIN
weather$TAVG <- mice_complete$TAVG

cat("Missing values AFTER MICE (base variables):\n")
print(colSums(is.na(weather[, c("PRCP","SNOW","SNWD","TMAX","TMIN","TAVG")])))

###############################################################
# 7. Generate lag features AFTER MICE
###############################################################

weather <- weather %>%
  mutate(
    PRCP_lag1 = lag(PRCP, 1),
    TMAX_lag1 = lag(TMAX, 1),
    TMAX_lag2 = lag(TMAX, 2),
    TMAX_lag3 = lag(TMAX, 3),
    TMIN_lag1 = lag(TMIN, 1),
    TAVG_lag1 = lag(TAVG, 1)
  )

###############################################################
# 8. Fill deterministic NA values in lag variables (start-of-series)
###############################################################
# These are structural boundary NAs (no earlier day exists),
# and are not appropriate for statistical imputation.
# The deterministic fill rule makes the feature table complete.

weather <- weather %>%
  mutate(
    PRCP_lag1 = if_else(is.na(PRCP_lag1), PRCP, PRCP_lag1),
    TMAX_lag1 = if_else(is.na(TMAX_lag1), TMAX, TMAX_lag1),
    TMAX_lag2 = if_else(is.na(TMAX_lag2), TMAX_lag1, TMAX_lag2),
    TMAX_lag3 = if_else(is.na(TMAX_lag3), TMAX_lag2, TMAX_lag3),
    TMIN_lag1 = if_else(is.na(TMIN_lag1), TMIN, TMIN_lag1),
    TAVG_lag1 = if_else(is.na(TAVG_lag1), TAVG, TAVG_lag1)
  )

cat("Remaining missing values AFTER lag fix (Should be 0):\n")
print(colSums(is.na(weather)))

###############################################################
# 9. Weekend + Quarter
###############################################################

weather <- weather %>%
  mutate(
    Weekend = weekdays(DATE) %in% c("Saturday", "Sunday"),
    Quarter = as.factor(quarters(DATE))
  )

###############################################################
# 10. Save final weather dataset (WORKING ML TABLE)
###############################################################
# The weather table is a core input for panel construction and ML,
# therefore it is saved directly in the working data directory.

write_csv(weather, "weather_tucson_daily.csv")
cat("Saved weather_tucson_daily.csv with", nrow(weather), "rows.\n")

###############################################################
# END OF SCRIPT
###############################################################
