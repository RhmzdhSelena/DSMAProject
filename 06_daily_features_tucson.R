###############################################################
# Script: 06_daily_features_tucson.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Aggregate cleaned Tucson Yelp review and check-in data to
#   the restaurant x day level. This script:
#     - Aggregates review-based behavior
#     - Aggregates check-in activity
#     - Engineers lagged and rolling engagement indicators
#     - Produces a unified daily feature table for panel building
#
# Notes:
#   - Sentiment and reviewer metadata are handled separately
#     for descriptive analysis and are NOT part of the panel.
#   - Lagged check-ins capture prior popularity and demand pressure,
#     avoiding simultaneity with daily satisfaction outcomes.
#   - MISSINGNESS HANDLING:
#     Review and check-in counts are "Zero-Filled" (NA -> 0) because
#     a missing record in transactional data implies zero activity.
#     "Cold Start" NAs (first 7 days) are also filled with 0, which
#     implicitly assumes no pre-history before the observation window.
#
# Inputs:
#   - reviews_tucson_clean.csv
#   - checkins_tucson_clean.csv
#
# Output:
#   - ./results/daily_features_tucson.csv
###############################################################

rm(list = ls())

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
cat("Working directory:", getwd(), "\n\n")

library(dplyr)
library(readr)
library(lubridate)
library(zoo)
library(tidyr) 

## -------------------------------------------------------------
## 0. Output folders (project convention)
## -------------------------------------------------------------
RESULTS_DIR <- file.path(getwd(), "results")
TABLE_DIR   <- file.path(RESULTS_DIR, "tables")
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

###############################################################
# 1. Load cleaned review data
###############################################################

reviews <- read_csv("reviews_tucson_clean.csv", show_col_types = FALSE)
cat("Loaded", nrow(reviews), "clean reviews.\n")

reviews <- reviews %>%
  mutate(
    business_id = as.character(business_id),
    date        = as.Date(date)
  )

###############################################################
# 2. Aggregate reviews to restaurant x day
###############################################################

daily_reviews <- reviews %>%
  group_by(business_id, date) %>%
  summarise(
    daily_review_count = n(),
    daily_avg_stars    = mean(stars, na.rm = TRUE),
    daily_useful       = sum(useful, na.rm = TRUE),
    daily_funny        = sum(funny, na.rm = TRUE),
    daily_cool         = sum(cool, na.rm = TRUE),
    .groups = "drop"
  )

cat("Aggregated daily review features:",
    nrow(daily_reviews), "restaurant-day rows.\n")

###############################################################
# 3. Load and aggregate check-in data
###############################################################

checkins <- read_csv("checkins_tucson_clean.csv", show_col_types = FALSE)
cat("Loaded", nrow(checkins), "check-in records.\n")

checkins <- checkins %>%
  mutate(
    business_id  = as.character(business_id),
    checkin_date = as.Date(checkin_date)
  )

# IMPORTANT:
# checkins_tucson_clean.csv is already aggregated to business_id x day
# with a "checkins" count column (number of check-ins that day).
# Therefore, daily_checkins must SUM that column, not count rows.
daily_checkins <- checkins %>%
  group_by(business_id, date = checkin_date) %>%
  summarise(
    daily_checkins = sum(checkins, na.rm = TRUE),
    .groups = "drop"
  )

cat("Aggregated daily check-ins:",
    nrow(daily_checkins), "restaurant-day rows.\n")

###############################################################
# 4. Merge daily review and check-in features
###############################################################

daily_features <- full_join(
  daily_reviews,
  daily_checkins,
  by = c("business_id", "date")
)

# -----------------------------------------------------------
# Zero-Filling Logic (Transactional Missings)
# -----------------------------------------------------------
daily_features <- daily_features %>%
  mutate(
    daily_review_count = replace_na(daily_review_count, 0),
    daily_checkins     = replace_na(daily_checkins, 0),
    daily_useful       = replace_na(daily_useful, 0),
    daily_funny        = replace_na(daily_funny, 0),
    daily_cool         = replace_na(daily_cool, 0)
    # Note: daily_avg_stars remains NA because 0 stars is not meaningful.
  )

cat("Applied zero-filling for missing count variables.\n")

# Basic integrity checks (submission-safe)
stopifnot(!any(is.na(daily_features$business_id)))
stopifnot(!any(is.na(daily_features$date)))

###############################################################
# 5. Time-ordering + lagged features
###############################################################

daily_features <- daily_features %>%
  arrange(business_id, date) %>%
  group_by(business_id) %>%
  mutate(
    # Lagged check-ins (yesterday)
    lag_checkins_1d = lag(daily_checkins, 1),
    
    # Lagged reviews (yesterday)
    lag_reviews_1d  = lag(daily_review_count, 1),
    
    # Rolling popularity (past 7 days, excluding today)
    roll_checkins_7d = rollmean(
      lag(daily_checkins, 1),
      k = 7,
      fill = NA,
      align = "right"
    ),
    
    # Cumulative counts
    cum_reviews  = cumsum(daily_review_count),
    cum_checkins = cumsum(daily_checkins)
  ) %>%
  ungroup()

###############################################################
# 6. Handle "Cold Start" NAs (Explicit Step)
###############################################################
daily_features <- daily_features %>%
  mutate(
    lag_checkins_1d  = replace_na(lag_checkins_1d, 0),
    lag_reviews_1d   = replace_na(lag_reviews_1d, 0),
    roll_checkins_7d = replace_na(roll_checkins_7d, 0)
  )

cat("Fixed 'Cold Start' NAs in lag/roll variables.\n")

cat("Verifying 0 NAs in lags:\n")
print(sum(is.na(daily_features$lag_checkins_1d)))
print(sum(is.na(daily_features$roll_checkins_7d)))

###############################################################
# 7. Save output
###############################################################

write_csv(daily_features, "daily_features_tucson.csv")

cat("Saved daily_features_tucson.csv with",
    nrow(daily_features), "rows.\n")

###############################################################
# END OF SCRIPT
###############################################################
