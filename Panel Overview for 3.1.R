# ============================================================
# Table for Section 3.1: Panel overview (documentation only)
# ============================================================

library(dplyr)
library(readr)

# ---- 1) Load the final panel you already export
# Adjust filename if yours differs (e.g., "internal_panel_final.csv" or "ML_Panel_final.csv")
final_path <- "ML_Panel_final.csv"
df <- read_csv(final_path, show_col_types = FALSE)

# ---- 2) Ensure date column is Date
# Adjust if your date column name differs (common: "date")
if (!inherits(df$date, "Date")) df <- df %>% mutate(date = as.Date(date))

# ---- 3) Build overview table
panel_overview <- df %>%
  summarise(
    restaurants = n_distinct(business_id),
    restaurant_days = n(),
    start_date = min(date, na.rm = TRUE),
    end_date = max(date, na.rm = TRUE),
    pct_days_with_review = mean(daily_review_count > 0, na.rm = TRUE) * 100,
    pct_days_with_checkin = mean(daily_checkins > 0, na.rm = TRUE) * 100
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# ---- 4) Save to your tables folder
# If you already use TABLE_BASE / TABLE_* folders, replace out_path accordingly.
out_path <- file.path("tables", "table_3_1_panel_overview.csv")
write_csv(panel_overview, out_path)
