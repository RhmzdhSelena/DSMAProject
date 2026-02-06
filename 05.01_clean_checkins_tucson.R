###############################################################
# Script: 05.01_clean_checkins_tucson.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Clean, validate, and structure the Tucson Yelp check-in dataset
#   extracted in Script 05. Specifically, this script:
#     - Parses and validates the check-in date field
#     - Removes duplicate (business_id, day) records
#     - Aggregates check-ins to business-day counts
#     - Flags burst outliers using the IQR rule (flagged, not removed)
#     - Produces diagnostics (missingness + distribution + examples)
#
# Notes (submission rules):
#   - No imputation for behavioral variables; missingness is flagged, not “fixed”.
#   - Outliers are flagged, not removed, to preserve raw behavioral signals.
#
# Inputs:
#   - checkins_tucson_raw.csv
#
# Outputs (project-structured):
#   - Clean dataset:      checkins_tucson_clean.csv
#   - Figures (appendix): ./figures/script05_01/...
#   - Tables/diagnostics: ./results/tables/...
###############################################################

library(dplyr)
library(readr)
library(lubridate)
library(ggplot2)
library(VIM)

## -------------------------------------------------------------
## 0. Project output folders
## -------------------------------------------------------------
# All outputs are written to folders inside the current working directory.
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
PROJECT_DIR <- getwd()

FIG_DIR     <- file.path(PROJECT_DIR, "figures", "checkins")
RESULTS_DIR <- file.path(PROJECT_DIR, "results")
TABLE_DIR   <- file.path(RESULTS_DIR, "tables")

dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(RESULTS_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

## -------------------------------------------------------------
## 1. Load raw extracted dataset
## -------------------------------------------------------------
raw <- read_csv("checkins_tucson_raw.csv", show_col_types = FALSE)
cat("Loaded", nrow(raw), "raw check-in rows for Tucson.\n")

## -------------------------------------------------------------
## 2. Convert datatypes & validate dates
## -------------------------------------------------------------
# We parse the check-in date and create flags for missing / invalid entries.
# For "invalid" we treat:
#   - dates before year 2000 as implausible platform data
#   - dates later than the *observed end date* of the dataset as invalid
#     (computed from the data after removing obviously implausible early years)

checkins <- raw %>%
  mutate(
    checkin_date = as.Date(checkin_date),
    missing_date = if_else(is.na(checkin_date), 1L, 0L)
  )

# Determine the dataset end date from the observed data (after removing implausibly early years).
# This makes the validation rule data-driven (your Yelp sample ends in 2022).
tmp_dates <- checkins %>%
  filter(!is.na(checkin_date), year(checkin_date) >= 2000)

dataset_end_date <- max(tmp_dates$checkin_date, na.rm = TRUE)
dataset_end_year <- year(dataset_end_date)

cat("Observed dataset end date:", format(dataset_end_date), "(year", dataset_end_year, ")\n")

# Flag invalid years and set those dates to NA (flagged, not removed).
checkins <- checkins %>%
  mutate(
    invalid_year = if_else(
      !is.na(checkin_date) &
        (year(checkin_date) < 2000 | checkin_date > dataset_end_date),
      1L, 0L
    ),
    checkin_date = if_else(invalid_year == 1L, as.Date(NA), checkin_date)
  )

cat("Date conversion and validation completed.\n")

## -------------------------------------------------------------
## 3. Remove duplicates
## -------------------------------------------------------------
# For daily aggregation, duplicates are defined as repeated rows for the same (business_id, checkin_date).
dup_count <- checkins %>%
  count(business_id, checkin_date) %>%
  filter(n > 1) %>%
  nrow()

if (dup_count > 0) {
  cat("Removing", dup_count, "duplicate check-in rows.\n")
}

checkins <- checkins %>% distinct(business_id, checkin_date, .keep_all = TRUE)

## -------------------------------------------------------------
## 4. Aggregate check-in counts per day
## -------------------------------------------------------------
# The modelling pipeline uses business-day panels, therefore we aggregate to one row per business per day.
checkins_daily <- checkins %>%
  filter(!is.na(checkin_date)) %>%   # keep valid dates only
  group_by(business_id, checkin_date) %>%
  summarise(
    checkins = n(),
    .groups = "drop"
  )

cat("Created daily check-in counts:", nrow(checkins_daily), "rows.\n")

## -------------------------------------------------------------
## 5. Detect burst outliers using IQR rule
## -------------------------------------------------------------
# Outliers are flagged (not removed). The rule uses the distribution of non-zero daily check-ins.
nonzero <- checkins_daily$checkins[checkins_daily$checkins > 0]

Q1 <- quantile(nonzero, 0.25, na.rm = TRUE)
Q3 <- quantile(nonzero, 0.75, na.rm = TRUE)
IQR_val <- Q3 - Q1
upper <- Q3 + 1.5 * IQR_val

checkins_daily <- checkins_daily %>%
  mutate(outlier_checkins = if_else(checkins > upper, 1L, 0L))

cat("Outlier detection completed. Upper threshold =", upper, "\n")

## -------------------------------------------------------------
## 6. Missingness summary & heatmap (diagnostics)
## -------------------------------------------------------------
# Missingness is exported as a compact table that can be referenced in the appendix if needed.
miss_summary <- tibble(
  variable = names(checkins_daily),
  n_missing = colSums(is.na(checkins_daily))
)

write_csv(miss_summary, file.path(TABLE_DIR, "script05_01_missingness_summary.csv"))

png(
  filename = file.path(FIG_DIR, "missingness_heatmap.png"),
  width = 900,
  height = 600,
  res = 150
)
VIM::aggr(checkins_daily, numbers = TRUE, sortVars = TRUE)
dev.off()

## -------------------------------------------------------------
## 7. Diagnostic plots
## -------------------------------------------------------------
# 7A — Distribution (log scale, zoomed to 99th percentile for readability)
p1 <- ggplot(checkins_daily, aes(x = checkins)) +
  geom_histogram(bins = 80, fill = "#4D96FF", alpha = 0.8, color = "white") +
  scale_x_continuous(limits = c(0, quantile(checkins_daily$checkins, 0.99, na.rm = TRUE))) +
  scale_y_log10() +
  labs(
    title = "Distribution of Daily Check-ins (Log Scale, Zoomed)",
    x = "Daily Check-ins",
    y = "Frequency (log)"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(FIG_DIR, "checkins_distribution_log.png"),
  plot = p1,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

# 7B — Outlier proportion
p2 <- ggplot(checkins_daily, aes(x = factor(outlier_checkins))) +
  geom_bar(fill = "#FF6B6B") +
  labs(
    title = "Proportion of Check-in Burst Outliers",
    x = "Outlier Flag (1 = burst)",
    y = "Count"
  ) +
  theme_minimal(base_size = 14)

ggsave(
  filename = file.path(FIG_DIR, "checkin_outlier_proportion.png"),
  plot = p2,
  width = 7,
  height = 5,
  dpi = 300,
  bg = "white"
)

# 7C — Time series preview for random restaurants
# This provides a quick sanity check that daily aggregation behaves as expected.
set.seed(123)
sample_ids <- sample(unique(checkins_daily$business_id), 6)

p3 <- ggplot(
  checkins_daily %>% filter(business_id %in% sample_ids),
  aes(x = checkin_date, y = checkins)
) +
  geom_line(color = "#845EC2") +
  facet_wrap(~ business_id, scales = "free_y") +
  labs(
    title = "Sample Check-in Time Series (6 Random Restaurants)",
    x = "Date",
    y = "Daily Check-ins"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  filename = file.path(FIG_DIR, "sample_time_series.png"),
  plot = p3,
  width = 10,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("Diagnostic plots saved to:", FIG_DIR, "\n")

## -------------------------------------------------------------
## 8. Save cleaned check-in dataset
## -------------------------------------------------------------
write_csv(checkins_daily, "checkins_tucson_clean.csv")

cat("Saved cleaned dataset: checkins_tucson_clean.csv with",
    nrow(checkins_daily), "rows.\n")

