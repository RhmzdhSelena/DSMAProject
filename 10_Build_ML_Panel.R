###############################################################
# Script: 10_build_ML_panel_final.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Construct the FINAL Machine Learning dataset.
#   - Includes 'log_review_count' (Top Predictor).
#   - Uses 'TAVG_sq' instead of linear Temp.
#   - Includes 'Lit Review Rescues' (Alcohol, Noise, etc.).
#
# Output:
#   ML_Panel_final.csv
###############################################################

rm(list = ls())
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
library(dplyr)
library(readr)
library(tidyr)

# 1. Load Master Panel (Cleaned in Script 08)
#    Note: Script 08 already created wifi_binary, alcohol_binary, noise_high!
data <- read_csv("internal_panel_final.csv", show_col_types = FALSE)

# 2. Feature Engineering 
# ---------------------------------------------------------

# A. Target Construction
restaurant_means <- data %>%
  group_by(business_id) %>%
  summarise(mean_stars = mean(daily_avg_stars, na.rm = TRUE), .groups="drop")

data <- data %>%
  left_join(restaurant_means, by = "business_id") %>%
  mutate(y_target = ifelse(daily_avg_stars >= mean_stars, 1, 0))

# B. Transformations (The "Smart" Variables)
data <- data %>%
  mutate(
    # 1. Covid
    covid_period = ifelse(date >= "2020-03-01" & date <= "2023-05-31", 1, 0),
    
    # 2. Log Review Count (CRITICAL - Top Predictor)
    log_review_count = log(review_count + 1),
    
    # 3. Weather Transforms (Non-Linear)
    TAVG_sq = TAVG^2,
    is_raining = ifelse(PRCP > 0, 1, 0) # Binary Rain
  )

# 3. Final Variable Selection
# ---------------------------------------------------------
# We select ONLY the variables validated in the Pink Graph + Lit Review
ML_Panel_final <- data %>%
  filter(!is.na(y_target)) %>% 
  dplyr::select(
    # --- Target ---
    y_target,
    
    # --- Behavioral (Strongest Signals) ---
    log_review_count,       # The #1 Predictor
    daily_review_count,
    daily_checkins,
    lag_reviews_1d,         # Validated over checkin lag
    roll_checkins_7d,
    cum_reviews,
    cum_checkins,
    
    # --- Attributes (incl. Lit Review Rescues) ---
    alcohol_binary,         # Was missing in old script
    wifi_binary,            # Created in Script 08
    noise_high,             # Matches Lit Review (Atmosphere)
    price_high,             # Matches Lit Review (Price-Quality)
    outdoor_seating,
    table_service,
    reservations,
    delivery,
    good_for_groups,
    
    # --- Categories (AIC Winners) ---
    cat_fast_food,
    cat_dessert,
    cat_dietary_specialty,
    
    # --- Context / Weather ---
    TAVG_sq,                # The "Goldilocks" Variable
    TMAX_lag1,              # The "Planning" Variable
    is_raining,             # The "Clean" Rain Variable
    covid_period,
    Weekend
    # Dropped: TMAX, PRCP (Raw variables replaced by smart ones)
  )

# 4.  Zero-Fill Cold Start NAs
# ---------------------------------------------------------
ML_Panel_final <- ML_Panel_final %>%
  mutate(
    lag_reviews_1d   = replace_na(lag_reviews_1d, 0),
    roll_checkins_7d = replace_na(roll_checkins_7d, 0),
    daily_review_count = replace_na(daily_review_count, 0)
  ) %>%
  na.omit() 

# 5. Save
# ------------------------------------------------------------
cat("========== FINAL ML PANEL ==========\n")
cat("Rows:", nrow(ML_Panel_final), "Cols:", ncol(ML_Panel_final), "\n")
print(table(ML_Panel_final$y_target))

write_csv(ML_Panel_final, "ML_Panel_final.csv")
cat("\nSaved successfully: ML_Panel_final.csv\n")