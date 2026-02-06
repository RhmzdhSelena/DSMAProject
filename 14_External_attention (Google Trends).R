###############################################################
# Script: 13A_Trends_vs_Engagement_OptionA.R
# Purpose:
#   Descriptive co-movement of Google Trends "attention" and
#   Yelp engagement (check-ins / reviews), with lag diagnostics.
#
# Key idea (Option A):
#   - Convert restaurant-level Trends (0-100, not comparable across restaurants)
#     into a scale-free "spike" indicator within each restaurant.
#   - Aggregate to week: share of restaurants experiencing a spike.
#   - Compare to aggregate weekly engagement (total / avg check-ins).
#
# Outputs:
#   - Figure: trends_attention_vs_checkins_timeseries.png
#   - Console: correlations at lag 0 and lag 1 week
###############################################################

rm(list = ls())

# ==============================
# 0) Libraries
# ==============================
library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(tidyr)

# ==============================
# 1) Paths (EDIT THESE)
# ==============================
DATA_DIR <- "C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data"
FIG_DIR  <- file.path(DATA_DIR, "figures_appendix")  # or "figures"

trends_file <- file.path(DATA_DIR, "google_trends_restaurants_final.csv")

# Yelp panel file (choose what you actually have on disk)
# Priority: internal_panel_final.csv (daily) -> best for check-ins
yelp_file_candidates <- c(
  file.path(DATA_DIR, "internal_panel_final.csv"),
  file.path(DATA_DIR, "ML_Panel_final.csv"),
  file.path(DATA_DIR, "ML_Panel_final.csv") # keep placeholder if you rename later
)

yelp_file <- yelp_file_candidates[file.exists(yelp_file_candidates)][1]
if (is.na(yelp_file)) stop("No Yelp panel found. Put internal_panel_final.csv or ML_Panel_final.csv into DATA_DIR.")

dir.create(FIG_DIR, showWarnings = FALSE, recursive = TRUE)

cat("Using Yelp panel:", yelp_file, "\n")
cat("Using Trends file:", trends_file, "\n")

# ==============================
# 2) Load Google Trends (restaurant-week level)
# ==============================
trends <- read_csv(trends_file, show_col_types = FALSE) %>%
  mutate(date = as.Date(date))

if ("isPartial" %in% colnames(trends)) {
  trends <- trends %>%
    filter(is.na(isPartial) | isPartial == FALSE)
}

# Expect columns like:
# date, search_interest, search_term, business_id
stopifnot(all(c("date","search_interest","business_id") %in% names(trends)))

# IMPORTANT:
# Trends 0–100 is typically scaled within each query/restaurant over time,
# so do NOT average raw 0–100 across restaurants.
# We make a within-restaurant spike indicator instead.

trends_spikes_weekly <- trends %>%
  group_by(business_id) %>%
  mutate(
    # robust threshold: top quartile weeks for THIS restaurant
    q75 = quantile(search_interest, 0.75, na.rm = TRUE),
    spike_week = ifelse(search_interest >= q75 & !is.na(search_interest), 1, 0),
    # optional: z-score within restaurant (also scale-free)
    z_interest = as.numeric(scale(search_interest))
  ) %>%
  ungroup() %>%
  group_by(date) %>%   # date already is week start from Trends export
  summarise(
    n_restaurants = n_distinct(business_id),
    share_spike   = mean(spike_week, na.rm = TRUE),
    mean_z        = mean(z_interest, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  rename(week = date)

# ==============================
# 3) Load Yelp panel (daily) and aggregate to matching week start (Sunday)
# ==============================
yelp <- read_csv(yelp_file, show_col_types = FALSE)

# We need a daily date column and engagement vars
# common in your pipeline: date, daily_checkins, daily_review_count
if (!("date" %in% names(yelp))) stop("Yelp panel must contain a 'date' column.")

yelp <- yelp %>%
  mutate(date = as.Date(date))

# Use whatever exists (check-ins are key; reviews are optional)
if (!("daily_checkins" %in% names(yelp))) stop("Yelp panel must contain 'daily_checkins'.")
if (!("daily_review_count" %in% names(yelp))) {
  cat("Note: daily_review_count not found; proceeding with check-ins only.\n")
}

# Align weeks to Trends: Trends uses Sunday week-start.
# floor_date(..., week_start = 7) sets Sunday as start of week.
yelp_weekly <- yelp %>%
  mutate(week = floor_date(date, unit = "week", week_start = 7)) %>%
  group_by(week) %>%
  summarise(
    total_checkins = sum(daily_checkins, na.rm = TRUE),
    avg_checkins   = mean(daily_checkins, na.rm = TRUE),
    total_reviews  = if ("daily_review_count" %in% names(yelp)) sum(daily_review_count, na.rm = TRUE) else NA_real_,
    avg_reviews    = if ("daily_review_count" %in% names(yelp)) mean(daily_review_count, na.rm = TRUE) else NA_real_,
    .groups = "drop"
  )

# ==============================
# 4) Merge & build standardized series for clean plotting
# ==============================
df <- trends_spikes_weekly %>%
  inner_join(yelp_weekly, by = "week") %>%
  arrange(week) %>%
  mutate(
    # standardize for comparable plotting (no dual axis)
    z_share_spike   = as.numeric(scale(share_spike)),
    z_total_checkins= as.numeric(scale(total_checkins)),
    z_total_reviews = if (!all(is.na(total_reviews))) as.numeric(scale(total_reviews)) else NA_real_,
    # lagged engagement (next week)
    z_total_checkins_lead1 = dplyr::lead(z_total_checkins, 1)
  )

# ==============================
# 5) Diagnostics: lag 0 and lag +1 correlations
# ==============================
cor_lag0 <- cor(df$z_share_spike, df$z_total_checkins, use = "complete.obs")
cor_lag1 <- cor(df$z_share_spike, df$z_total_checkins_lead1, use = "complete.obs")

cat("\n--- Google Trends attention (share_spike) vs total check-ins ---\n")
cat("Correlation (same week, lag 0): ", round(cor_lag0, 3), "\n")
cat("Correlation (next week, lag +1):", round(cor_lag1, 3), "\n\n")

# ==============================
# 6) Plot: co-movement time series (standardized)
# ==============================
plot_df <- df %>%
  select(week, z_share_spike, z_total_checkins) %>%
  pivot_longer(cols = c(z_share_spike, z_total_checkins),
               names_to = "series", values_to = "z_value") %>%
  mutate(series = recode(series,
                         z_share_spike    = "External attention (Trends spike share, z)",
                         z_total_checkins = "Engagement (Total check-ins, z)"))

p <- ggplot(plot_df, aes(x = week, y = z_value, linetype = series)) +
  geom_line(linewidth = 0.9) +
  labs(
    x = NULL,
    y = "Standardized deviation (z-score)",
    linetype = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

out_path <- file.path(FIG_DIR, "trends_attention_vs_checkins_timeseries.png")
ggsave(out_path, p, width = 9, height = 4.8, dpi = 300)
cat("Saved figure:", out_path, "\n")
