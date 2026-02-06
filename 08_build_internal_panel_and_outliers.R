###############################################################
# Script: 08_build_internal_panel_and_outliers.R  (ADAPTED)
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Merge static attributes, daily behavior, and weather into a unified
#   INTERNAL-ONLY modeling panel. Detect outliers (IQR rule), create flags,
#   identify long inactivity periods, and prepare the dataset for
#   downstream modeling and external-data enrichment.
#
# Notes:
#   - Behavioral variables are NOT imputed.
#   - Static business attributes already imputed in Script 03.
#   - Outliers flagged, never removed.
#   - Inactivity detection uses a HYBRID, DATA-DRIVEN RULE.
#
# Output:
#   - internal_panel_final.csv
###############################################################

rm(list = ls())

###############################################################
# Setup
###############################################################

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
cat("Working directory:", getwd(), "\n")

library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(data.table)
library(ggplot2)
library(scales)

## -------------------------------------------------------------
## Output folders (project convention)
## -------------------------------------------------------------
FIG_BASE <- "C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data/figures"
TAB_BASE <- "C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data/tables"

FIG_ROOT <- file.path(FIG_BASE, "panel_internal_outliers")
FIG_OUTLIERS <- file.path(FIG_ROOT, "outlier_diagnostics")
FIG_INACTIVITY <- file.path(FIG_ROOT, "inactivity_diagnostics")
FIG_ENGAGEMENT <- file.path(FIG_ROOT, "engagement_distributions")
FIG_APPENDIX <- file.path(FIG_ROOT, "appendix")

TAB_ROOT <- file.path(TAB_BASE, "panel_internal_outliers")

dir.create(FIG_OUTLIERS, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_INACTIVITY, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_ENGAGEMENT, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_APPENDIX, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_ROOT, recursive = TRUE, showWarnings = FALSE)

## -------------------------------------------------------------
## Plot palette (academic blue/grey)
## -------------------------------------------------------------
pal_light <- "#DCE3EA"
pal_mid1  <- "#CBD2DB"
pal_mid2  <- "#B7C4D1"
pal_dark  <- "#00618F"
pal_accent <- "#fab1a0"  # only if high contrast is truly needed

pretty_theme <- theme_minimal(base_size = 14) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "right"
  )

###############################################################
# STEP 1 — Load input datasets
###############################################################

internal <- read_csv("internal_dataset_tucson_imputed.csv", show_col_types = FALSE)
daily    <- read_csv("daily_features_tucson.csv", show_col_types = FALSE)
weather  <- read_csv("weather_tucson_daily.csv", show_col_types = FALSE)

# --- Robustness: enforce key types (from B) ---
daily <- daily %>%
  mutate(
    business_id = as.character(business_id),
    date = as.Date(date)
  )

internal <- internal %>%
  mutate(
    business_id = as.character(business_id)
  )

weather <- weather %>%
  rename(date = DATE) %>%
  mutate(date = as.Date(date))

cat("Loaded:",
    nrow(internal), "internal |",
    nrow(daily), "daily |",
    nrow(weather), "weather rows.\n")

###############################################################
# STEP 2 — Merge datasets
###############################################################

panel <- daily %>%
  left_join(weather, by = "date") %>%
  left_join(internal, by = "business_id")

panel <- panel %>%
  filter(date >= as.Date("2012-01-01"))

cat("Merged & filtered panel rows:", nrow(panel), "\n")

###############################################################
# STEP 2.5 — Category filtering & grouping (STRUCTURAL FEATURES)
###############################################################

# --- Robustness: NA-safe categories (from B) ---
panel <- panel %>%
  mutate(categories_lc = tolower(coalesce(categories, "")))

# Drop NON-DINING business categories
non_dining_pattern <- paste(
  "beauty & spas|day spas|massage",
  "fitness & instruction|yoga|gyms",
  "shopping|gift shops|fashion",
  "health & medical",
  "arts & entertainment|cinema|museums",
  "financial services|tax services|travel services|real estate",
  sep = "|"
)

n_before <- nrow(panel)

panel <- panel %>%
  filter(!grepl(non_dining_pattern, categories_lc))

cat("Dropped non-dining businesses:", n_before - nrow(panel), "rows removed.\n")

# Create interpretable category binaries
panel <- panel %>%
  mutate(
    cat_ethnic = as.integer(grepl(
      "mexican|szechuan|cantonese|ethiopian|syrian|peruvian|pakistani|thai|japanese|korean|vietnamese|indian|turkish|persian|african|lebanese|brazilian|colombian|malaysian|filipino",
      categories_lc
    )),
    cat_american_european = as.integer(grepl(
      "american|british|german|polish|irish|brasserie|bistro|soul food|tuscan|basque",
      categories_lc
    )),
    cat_fast_food = as.integer(grepl(
      "fast food|wraps|donuts|bagels|waffles|pancakes|cheesesteaks|cafeteria",
      categories_lc
    )),
    cat_cafe_breakfast = as.integer(grepl(
      "cafe|coffee|tea rooms|breakfast|brunch|creperies",
      categories_lc
    )),
    cat_dessert = as.integer(grepl(
      "desserts|gelato|shaved ice|cupcakes|macarons|ice cream",
      categories_lc
    )),
    cat_bar_nightlife = as.integer(grepl(
      "bars|beer gardens|brewpubs|whiskey bars|tiki bars|hookah bars|wine tasting|pubs",
      categories_lc
    )),
    cat_seafood_specialty = as.integer(grepl(
      "seafood|fish & chips|poke",
      categories_lc
    )),
    cat_dietary_specialty = as.integer(grepl(
      "halal|kosher|live/raw food|organic|health markets",
      categories_lc
    ))
  )

# Drop raw category string
panel <- panel %>%
  select(-categories, -categories_lc)

cat("Category binaries created and raw 'categories' removed.\n")

###############################################################
# STEP 2.6 — Attribute binaries for descriptives + modeling
###############################################################

panel <- panel %>%
  mutate(
    # Wifi: 1 if free or paid mentioned; 0 otherwise
    wifi_bin = ifelse(!is.na(wifi) & grepl("free|paid", tolower(as.character(wifi))), 1, 0),
    miss_wifi = ifelse(is.na(wifi), 1, 0),
    
    # Alcohol: 1 if full_bar or beer_and_wine; 0 otherwise
    alcohol_bin = ifelse(!is.na(alcohol) & as.character(alcohol) %in% c("full_bar", "beer_and_wine"), 1, 0),
    miss_alcohol = ifelse(is.na(alcohol), 1, 0),
    
    # Noise: quiet as key differentiator
    noise_quiet = ifelse(!is.na(noise_level) & as.character(noise_level) == "quiet", 1, 0),
    miss_noise = ifelse(is.na(noise_level), 1, 0),
    
    # Price (UPDATED): high price = $$$ or $$$$
    price_high = ifelse(!is.na(price_range) & price_range >= 3, 1, 0),
    miss_price = ifelse(is.na(price_range), 1, 0)
  )

# --- Compatibility aliases for Script 10 expectations ---
panel <- panel %>%
  mutate(
    wifi_binary = wifi_bin,
    alcohol_binary = alcohol_bin,
    noise_high = ifelse(!is.na(noise_level) & as.character(noise_level) %in% c("loud", "very_loud"), 1, 0)
  )

###############################################################
# STEP 3 — Outlier Detection (IQR)
###############################################################

outlier_iqr_flag <- function(df, var) {
  x <- df[[var]]
  qs <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  IQRv <- qs[2] - qs[1]
  df[[paste0("outlier_", var)]] <-
    ifelse(x < qs[1] - 1.5 * IQRv | x > qs[2] + 1.5 * IQRv, 1, 0)
  df
}

behavior_vars <- c("daily_review_count", "daily_checkins")

for (v in behavior_vars) {
  panel <- outlier_iqr_flag(panel, v)
}

###############################################################
# PLOTS — Outlier diagnostics
###############################################################

for (v in behavior_vars) {
  
  df <- panel %>% filter(!is.na(.data[[v]]))
  flag <- paste0("outlier_", v)
  
  p <- ggplot(df, aes(x = .data[[v]], fill = factor(.data[[flag]]))) +
    geom_histogram(bins = 80, alpha = 0.85, color = "white") +
    scale_fill_manual(
      values = c("0" = pal_mid1, "1" = pal_dark),
      labels = c("Normal", "Outlier"),
      name = "Observation Type"
    ) +
    scale_x_continuous(limits = c(0, quantile(df[[v]], 0.99, na.rm = TRUE))) +
    pretty_theme +
    labs(
      title = paste("Outlier Detection for", v),
      subtitle = "IQR rule (zoomed)",
      x = v, y = "Frequency"
    )
  
  ggsave(
    filename = file.path(FIG_OUTLIERS, paste0("outliers_", v, "_iqr_zoom.png")),
    plot = p, width = 8, height = 5, dpi = 300, bg = "white"
  )
}

###############################################################
# STEP 4 — Inactivity detection
###############################################################

total_activity <- panel %>%
  group_by(business_id) %>%
  summarise(total_checkins = sum(daily_checkins, na.rm = TRUE), .groups = "drop")

never_active_ids <- total_activity %>%
  filter(total_checkins == 0) %>%
  pull(business_id)

zero_runs <- panel %>%
  filter(!business_id %in% never_active_ids) %>%
  arrange(business_id, date) %>%
  group_by(business_id) %>%
  mutate(
    run_id     = rleid(daily_checkins == 0),
    run_length = ave(daily_checkins == 0, run_id, FUN = length)
  ) %>%
  summarise(max_zero_run = max(run_length, na.rm = TRUE))

Q1 <- quantile(zero_runs$max_zero_run, 0.25, na.rm = TRUE)
Q3 <- quantile(zero_runs$max_zero_run, 0.75, na.rm = TRUE)
IQRv <- Q3 - Q1
final_threshold <- max(Q3 + 1.5 * IQRv, 30)

panel$inactive_flag <- ifelse(
  panel$business_id %in% zero_runs$business_id[zero_runs$max_zero_run >= final_threshold], 1,
  ifelse(panel$business_id %in% never_active_ids, 2, 0)
)

###############################################################
# APPENDIX FIGURES
###############################################################

p1 <- ggplot(panel, aes(x = daily_review_count)) +
  geom_histogram(bins = 50, fill = pal_mid2, color = "white") +
  scale_y_log10(labels = label_comma()) +
  pretty_theme +
  labs(
    title = "Engagement Skewness",
    subtitle = "Daily review counts (log scale)",
    x = "Daily review count", y = "Frequency"
  )

ggsave(
  filename = file.path(FIG_APPENDIX, "App_Fig_1_Skewness.png"),
  plot = p1, width = 8, height = 5, dpi = 300, bg = "white"
)

p2 <- ggplot(zero_runs, aes(x = max_zero_run)) +
  geom_histogram(bins = 60, fill = pal_mid2, color = "white") +
  geom_vline(xintercept = final_threshold, linetype = "dashed", color = pal_dark) +
  pretty_theme +
  labs(
    title = "Maximum Inactivity Periods",
    subtitle = paste("Hybrid threshold =", round(final_threshold)),
    x = "Consecutive zero check-in days",
    y = "Restaurants"
  )

ggsave(
  filename = file.path(FIG_APPENDIX, "App_Fig_2_Inactivity.png"),
  plot = p2, width = 8.5, height = 5, dpi = 300, bg = "white"
)

###############################################################
# STEP 5 — Final cleaning & save
###############################################################

panel <- panel %>%
  mutate(
    across(where(is.character), as.factor),
    weekday = factor(
      weekdays(date),
      levels = c("Monday","Tuesday","Wednesday","Thursday",
                 "Friday","Saturday","Sunday")
    )
  )

write_csv(panel, "internal_panel_final.csv")
cat("Saved internal_panel_final.csv with", nrow(panel), "rows.\n")

cat("Panel columns (first 30):\n")
print(head(colnames(panel), 30))

###############################################################
# END OF SCRIPT
###############################################################
