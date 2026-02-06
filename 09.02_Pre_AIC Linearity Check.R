###############################################################
# Script: 09.02_Linearity_Check.R
# Purpose:
#   Single-figure linearity diagnostics for ML predictors only
###############################################################

rm(list = ls())

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")

library(dplyr)
library(ggplot2)
library(tidyr)

## -------------------------------------------------------------
## Output directory (EXACT path requested)
## -------------------------------------------------------------
FIG_DIR <- "C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data/figures/linearity_diagnostics"
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

## -------------------------------------------------------------
## 1) Load INTERNAL panel (same base as ML)
## -------------------------------------------------------------
df <- read.csv("internal_panel_final.csv")

## -------------------------------------------------------------
## 2) Target construction (MATCH ML SCRIPT EXACTLY)
## -------------------------------------------------------------
restaurant_means <- df %>%
  group_by(business_id) %>%
  summarise(mean_stars = mean(daily_avg_stars, na.rm = TRUE), .groups = "drop")

df <- df %>%
  left_join(restaurant_means, by = "business_id") %>%
  mutate(y_target = as.integer(daily_avg_stars >= mean_stars))

## -------------------------------------------------------------
## 3) Keep ONLY ML predictors (no junk variables)
## -------------------------------------------------------------
ml_vars <- c(
  "log_review_count",
  "daily_review_count",
  "daily_checkins",
  "lag_reviews_1d",
  "roll_checkins_7d",
  "cum_reviews",
  "cum_checkins",
  "alcohol_binary",
  "wifi_binary",
  "noise_high",
  "price_high",
  "outdoor_seating",
  "table_service",
  "delivery",
  "reservations",
  "good_for_groups",
  "cat_fast_food",
  "cat_dessert",
  "cat_dietary_specialty",
  "TAVG_sq",
  "TMAX_lag1",
  "is_raining",
  "covid_period",
  "Weekend"
)

df <- df %>%
  dplyr::select(y_target, any_of(ml_vars))

## -------------------------------------------------------------
## 4) Pretty variable labels (MATCH descriptive table)
## -------------------------------------------------------------
var_labels <- c(
  log_review_count        = "Log daily number of reviews",
  daily_review_count      = "Daily number of reviews",
  daily_checkins          = "Daily check-in count",
  lag_reviews_1d          = "Lagged daily reviews",
  roll_checkins_7d        = "7-day rolling avg. check-ins",
  cum_reviews             = "Cumulative number of reviews",
  cum_checkins            = "Cumulative number of check-ins",
  alcohol_binary          = "Alcohol served",
  wifi_binary             = "WiFi available",
  noise_high              = "High noise level",
  price_high              = "High price level",
  outdoor_seating         = "Outdoor seating available",
  table_service           = "Table service offered",
  delivery                = "Delivery service available",
  reservations            = "Reservations accepted",
  good_for_groups         = "Good for groups",
  cat_fast_food           = "Fast food category",
  cat_dessert             = "Dessert category",
  cat_dietary_specialty   = "Dietary specialty category",
  TAVG_sq                 = "Average temperature (squared)",
  TMAX_lag1               = "Lagged maximum temperature (°C)",
  is_raining              = "Rainy day indicator",
  covid_period            = "COVID-19 period indicator",
  Weekend                 = "Weekend indicator"
)

## -------------------------------------------------------------
## 5) Long format for faceting
## -------------------------------------------------------------
df_long <- df %>%
  pivot_longer(
    cols = -y_target,
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(is.finite(value))

df_long$label <- var_labels[df_long$variable]

## -------------------------------------------------------------
## 6) Sample for plotting (speed + clean visuals)
## -------------------------------------------------------------
set.seed(123)

df_long <- df_long %>%
  group_by(variable) %>%
  group_modify(~ {
    n_take <- min(20000, nrow(.x))
    dplyr::slice_sample(.x, n = n_take)
  }) %>%
  ungroup()

## -------------------------------------------------------------
## 7) Single MULTI-PANEL figure with LOESS Trend (Red) vs. GLM (Blue)
## -------------------------------------------------------------
p <- ggplot(df_long, aes(x = value, y = y_target)) +
  # Keep the raw data points as light background context
  geom_point(alpha = 0.05, color = "grey60", size = 0.5) + 
  
  # THE BLUE LINE: The Linear Assumption (Binomial GLM)
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial()),
    se = FALSE,
    color = "#00618F",
    linewidth = 1
  ) +
  
  # THE RED LINE: The "Actual Trend" (LOESS)
  # This shows where the data deviates from linearity
  geom_smooth(
    method = "loess", 
    se = FALSE, 
    color = "#D63384", # Using a pink/red consistent with your AUC plots
    linetype = "dashed",
    linewidth = 0.8
  ) +
  
  facet_wrap(~ label, scales = "free_x", ncol = 3) +
  labs(
    title = "Logit Linearity Diagnostics",
    subtitle = "Blue Solid = Linear Assumption | Red Dashed = Non-Linear Trend (LOESS)",
    x = NULL,
    y = "Pr(Above-average daily satisfaction)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10, color = "grey30")
  )

## -------------------------------------------------------------
## 8) Save (SINGLE IMAGE)
## -------------------------------------------------------------
out_file <- file.path(FIG_DIR, "App_Fig_Linearity_Diagnostics.png")

ggsave(
  filename = out_file,
  plot = p,
  width = 9,
  height = 14,   # LONGER, not wider
  dpi = 300,
  bg = "white"
)

cat("\nSaved linearity diagnostics figure to:\n", out_file, "\n")
