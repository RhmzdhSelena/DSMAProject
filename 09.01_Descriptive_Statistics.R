###############################################################
# Script: 09.01_Descriptive_Statistics_MLLogic_CountCheckins.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Generate Table 1 (Descriptive Statistics) on the SAME analytical
#   population as the ML/logit models: restaurant-days where
#   satisfaction is observable (i.e., daily_avg_stars is not NA).
#
#   To improve interpretability in Table 1, this script reconstructs
#   TRUE daily check-in COUNTS and a 7-day rolling mean of counts
#   from raw check-in timestamps (checkins_tucson_raw.csv).
#
#   All other variables and filters follow the ML panel logic.
#
# Inputs:
#   - internal_panel_final.csv
#   - checkins_tucson_raw.csv
#
# Output:
#   - Table_1_Descriptive_Statistics_MLLogic_CountCheckins.csv
###############################################################

rm(list = ls())

library(dplyr)
library(readr)
library(zoo)
library(tidyr)  # <-- needed for replace_na()

###############################################################
# 0) Working directory (choose ONE)
###############################################################

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")

###############################################################
# 0A) Output folder (RESULTS -> descriptive_statistics)
###############################################################
RESULTS_BASE <- "C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data/results"
OUT_DESC <- file.path(RESULTS_BASE, "descriptive_statistics")

if (!dir.exists(OUT_DESC)) dir.create(OUT_DESC, recursive = TRUE, showWarnings = FALSE)

###############################################################
# 1) Load base panel (source used to build ML panel)
###############################################################

panel <- read_csv("internal_panel_final.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

###############################################################
# 2) Reconstruct TRUE daily check-in counts from raw check-in file
###############################################################

checkins_raw <- read_csv("checkins_tucson_raw.csv", show_col_types = FALSE) %>%
  mutate(checkin_date = as.Date(checkin_date)) %>%
  filter(!is.na(checkin_date))

daily_checkins_count <- checkins_raw %>%
  group_by(business_id, date = checkin_date) %>%
  summarise(daily_checkins_count = n(), .groups = "drop") %>%
  arrange(business_id, date) %>%
  group_by(business_id) %>%
  mutate(
    # 7-day rolling mean of counts (robust for short histories)
    roll_checkins_7d_count = zoo::rollapply(
      daily_checkins_count,
      width = 7,
      FUN = mean,
      align = "right",
      fill = NA,
      partial = TRUE
    ),
    # count-based cumulative check-ins (consistent with count daily series)
    cum_checkins_count = cumsum(daily_checkins_count)
  ) %>%
  ungroup()

###############################################################
# 3) Merge reconstructed count-based check-ins into the panel
###############################################################

panel <- panel %>%
  left_join(daily_checkins_count, by = c("business_id", "date")) %>%
  mutate(
    daily_checkins_count   = replace_na(daily_checkins_count, 0),
    roll_checkins_7d_count = replace_na(roll_checkins_7d_count, 0),
    cum_checkins_count     = replace_na(cum_checkins_count, 0)
  )

###############################################################
# 4) Build y_target exactly as in ML panel logic
###############################################################

restaurant_means <- panel %>%
  group_by(business_id) %>%
  summarise(mean_stars = mean(daily_avg_stars, na.rm = TRUE), .groups = "drop")

panel <- panel %>%
  left_join(restaurant_means, by = "business_id") %>%
  mutate(
    y_target = ifelse(!is.na(daily_avg_stars) & daily_avg_stars >= mean_stars, 1, 0)
  )

###############################################################
# 5) Recreate controls & transformations (mirror ML build)
###############################################################

panel <- panel %>%
  mutate(
    covid_period = ifelse(date >= as.Date("2020-03-01") & date <= as.Date("2023-05-31"), 1, 0),
    is_raining   = ifelse(PRCP > 0, 1, 0)
  )

###############################################################
# 6) Construct descriptive sample: ONLY days with observable satisfaction
###############################################################
# This is the key correction: we restrict the dataset BEFORE
# computing descriptives, so ALL variables share the same N.

desc_panel <- panel %>%
  filter(!is.na(daily_avg_stars)) %>%    # satisfaction observable (review day)
  transmute(
    daily_avg_stars,
    y_target,
    
    # Engagement (counts for descriptives)
    daily_review_count,
    daily_checkins_count,
    roll_checkins_7d_count,
    
    # Long-run engagement
    cum_reviews,
    cum_checkins_count,
    
    # Internal attributes
    outdoor_seating,
    table_service,
    delivery,
    reservations,
    good_for_groups,
    wifi_binary,
    alcohol_binary,
    noise_high,
    price_high,
    
    # External/context controls
    TMAX_lag1,
    TMAX,
    PRCP,
    Weekend,
    covid_period,
    is_raining
  ) %>%
  na.omit()   # ML-style complete-case requirement for the variables reported

###############################################################
# 7) Descriptive statistics + export
###############################################################

desc <- data.frame(
  n    = sapply(desc_panel, function(x) sum(!is.na(x))),
  mean = sapply(desc_panel, function(x) mean(x, na.rm = TRUE)),
  sd   = sapply(desc_panel, function(x) sd(x, na.rm = TRUE)),
  min  = sapply(desc_panel, function(x) suppressWarnings(min(x, na.rm = TRUE))),
  max  = sapply(desc_panel, function(x) suppressWarnings(max(x, na.rm = TRUE)))
)

desc <- round(desc, 3)

write.csv(
  desc,
  file.path(OUT_DESC, "Table_1_Descriptive_Statistics_MLLogic_CountCheckins.csv"),
  row.names = TRUE
)

cat("✔ Exported: Table_1_Descriptive_Statistics_MLLogic_CountCheckins.csv\n")
cat("Rows in descriptive analytical sample:", nrow(desc_panel), "\n")

###############################################################
# 8) Quick diagnostics (optional)
###############################################################

cat("Max daily_checkins_count:", max(desc_panel$daily_checkins_count, na.rm = TRUE), "\n")
cat("Max roll_checkins_7d_count:", max(desc_panel$roll_checkins_7d_count, na.rm = TRUE), "\n")
ML_Panel_final <- read_csv("ML_Panel_final.csv", show_col_types = FALSE) 
names(ML_Panel_final)
