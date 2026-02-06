###############################################################
# Script: 04.02_sentiment_kpi_analysis.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Create a descriptive-only enriched review dataset by:
#     (1) Loading the cleaned Tucson review table (no sentiment)
#     (2) Computing review-level sentiment measures and a standardized
#         sentiment index for descriptive analysis
#     (3) Linking daily textual sentiment to the customer satisfaction
#         KPI at the restaurant-day level to contextualize KPI variation
#
# Notes:
#   - This output is used exclusively for descriptive analysis and
#     interpretation. Sentiment variables are not included in the
#     supervised ML feature set.
#   - The KPI link is descriptive (association-based). No predictive
#     modeling is conducted in this script.
#   - User metadata enrichment is optional and does not affect the
#     KPI–sentiment analysis.
#
# Inputs:
#   - reviews_tucson_clean.csv
#   - (Optional) yelp_academic_dataset_user.json
#
# Outputs:
#   - reviews_tucson_enriched_descriptive.csv
#   - tables/reviews_sentiment_enrichment_audit.csv
#   - tables/reviews_sentiment_scores_summary.csv
#   - tables/sentiment_by_kpi_summary.csv
#   - tables/sentiment_kpi_restaurant_day.csv
#   - tables/sentiment_kpi_ttest.csv
#   - tables/sentiment_stars_correlation_review_level.csv
#   - tables/sentiment_stars_regression_review_level.csv
#   - figures/sentiment/dist_sentiment_final.png
#   - figures/sentiment/sentiment_final_vs_stars.png
#   - figures/sentiment/sentiment_by_kpi_boxplot.png
#   - figures/sentiment/daily_sentiment_vs_daily_stars.png
###############################################################

rm(list = ls())

library(dplyr)
library(readr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)

library(tidytext)
library(broom)
library(jsonlite)

library(furrr)
library(future)
library(vader)
library(utf8)

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
cat("Working directory:", getwd(), "\n\n")

dir.create("tables", showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("figures", "sentiment"), showWarnings = FALSE, recursive = TRUE)

###############################################################
# 1) Load cleaned reviews (no sentiment)
###############################################################

reviews <- read_csv("reviews_tucson_clean.csv", show_col_types = FALSE)
cat("Loaded", nrow(reviews), "clean reviews.\n")

reviews <- reviews %>%
  mutate(
    review_id = as.character(review_id),
    business_id = as.character(business_id),
    user_id = as.character(user_id),
    date = as.Date(date),
    text_clean = as.character(text_clean)
  )

###############################################################
# 2) Optional: join user metadata for descriptive enrichment
###############################################################

user_path <- "yelp_academic_dataset_user.json"

if (file.exists(user_path)) {
  
  users <- jsonlite::stream_in(file(user_path), verbose = FALSE) %>%
    as_tibble() %>%
    transmute(
      user_id = as.character(user_id),
      user_fans = as.numeric(fans),
      friends_count = ifelse(is.na(friends) | friends == "", 0,
                             str_count(friends, ",") + 1)
    )
  
  reviews <- reviews %>%
    left_join(users, by = "user_id") %>%
    mutate(
      miss_user_fans = ifelse(is.na(user_fans), 1, 0),
      miss_friends_count = ifelse(is.na(friends_count), 1, 0),
      user_fans = ifelse(is.na(user_fans), 0, user_fans),
      friends_count = ifelse(is.na(friends_count), 0, friends_count)
    )
  
  join_audit <- reviews %>%
    summarise(
      n_reviews = n(),
      share_missing_user_fans = mean(miss_user_fans == 1, na.rm = TRUE),
      share_missing_friends_count = mean(miss_friends_count == 1, na.rm = TRUE),
      mean_user_fans = mean(user_fans, na.rm = TRUE),
      mean_friends_count = mean(friends_count, na.rm = TRUE)
    )
  
  write_csv(join_audit, file.path("tables", "reviews_sentiment_enrichment_audit.csv"))
  
} else {
  
  join_audit <- tibble(
    note = "User metadata file not found; user enrichment skipped.",
    expected_path = user_path
  )
  
  write_csv(join_audit, file.path("tables", "reviews_sentiment_enrichment_audit.csv"))
}

###############################################################
# 3) Compute sentiment (review-level; descriptive-only)
###############################################################
# Sentiment is computed using the established multi-method approach:
#   - VADER compound score (parallelized in chunks)
#   - AFINN lexicon sentiment (token-based sum)
#   - Custom domain dictionary sentiment (token-based sum)
# The final hybrid index sentiment_final is a weighted combination of
# standardized components, consistent with the descriptive pipeline.

# Ensure text_clean exists and is UTF-8 valid
if (!("text_clean" %in% names(reviews))) {
  reviews <- reviews %>%
    mutate(
      text_clean = text %>%
        str_replace_all("[\r\n]", " ") %>%
        str_squish()
    )
}
invalid_idx <- which(!utf8_valid(reviews$text_clean))
if (length(invalid_idx) > 0) {
  reviews$text_clean[invalid_idx] <- iconv(reviews$text_clean[invalid_idx], from = "", to = "UTF-8")
}

# Parallel backend for VADER
plan(multisession, workers = 12)
cat("Parallel backend started with 12 workers.\n")

# 3.1 VADER sentiment (chunked + parallel)
chunk_size <- 2000
n <- nrow(reviews)
chunk_ids <- split(seq_len(n), ceiling(seq_len(n) / chunk_size))

compute_vader <- function(idx, df) {
  res <- vader_df(df$text_clean[idx])
  res$idx <- idx
  res
}

cat("Running VADER sentiment in", length(chunk_ids), "parallel batches...\n")

sentiment_list <- future_map(
  chunk_ids,
  compute_vader,
  df = reviews,
  .progress = TRUE
)

sentiment_df <- bind_rows(sentiment_list) %>%
  arrange(idx) %>%
  select(-idx)

reviews <- bind_cols(reviews, sentiment_df)

# 3.2 AFINN sentiment (token-based sum)
afinn <- tidytext::get_sentiments("afinn")

afinn_scores <- reviews %>%
  select(review_id, text_clean) %>%
  filter(!is.na(text_clean), text_clean != "") %>%
  mutate(text_clean = str_to_lower(text_clean)) %>%
  unnest_tokens(word, text_clean) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  inner_join(afinn, by = "word") %>%
  group_by(review_id) %>%
  summarise(sent_afinn = sum(value, na.rm = TRUE), .groups = "drop")

reviews <- reviews %>%
  left_join(afinn_scores, by = "review_id") %>%
  mutate(sent_afinn = replace_na(sent_afinn, 0))

# 3.3 Custom dictionary sentiment (token-based sum)
custom_dict <- tibble(
  word = c(
    "mediocre","bland","soggy","burnt","undercooked","overcooked",
    "greasy","dry","rubbery","stale","cold","dismissive","slow",
    "inattentive","unhelpful","overpriced","ripoff","scammy",
    "tasty","savory","flavorful","juicy","crunchy","cozy","trendy",
    "instagrammable","attentive","accommodating","top-notch","worth-it"
  ),
  score = c(
    -1.5,-1.2,-1.5,-1.7,-1.6,-1.6,-1.3,-0.8,-1.3,-1.4,-0.5,-1.5,-0.8,
    -1.0,-1.2,-1.8,-2.0,-2.0,
    2.0,1.8,2.2,1.7,1.5,1.8,1.2,1.5,2.0,1.8,2.5,1.7
  )
)

custom_scores <- reviews %>%
  select(review_id, text_clean) %>%
  filter(!is.na(text_clean), text_clean != "") %>%
  mutate(text_clean = str_to_lower(text_clean)) %>%
  unnest_tokens(word, text_clean) %>%
  filter(str_detect(word, "^[a-z]+$")) %>%
  inner_join(custom_dict, by = "word") %>%
  group_by(review_id) %>%
  summarise(sent_custom = sum(score, na.rm = TRUE), .groups = "drop")

reviews <- reviews %>%
  left_join(custom_scores, by = "review_id") %>%
  mutate(sent_custom = replace_na(sent_custom, 0))

# 3.4 Hybrid sentiment_final (same weighting)
norm <- function(x) {
  if (sd(x, na.rm = TRUE) == 0) return(rep(0, length(x)))
  as.numeric(scale(x))
}

reviews <- reviews %>%
  mutate(
    sentiment_final =
      norm(compound)   * 0.45 +
      norm(sent_afinn) * 0.25 +
      norm(sent_custom)* 0.30
  )

sent_summary <- reviews %>%
  summarise(
    mean_sentiment_final = mean(sentiment_final, na.rm = TRUE),
    sd_sentiment_final   = sd(sentiment_final, na.rm = TRUE),
    share_missing_compound = mean(is.na(compound), na.rm = TRUE),
    share_zero_afinn = mean(sent_afinn == 0, na.rm = TRUE),
    share_zero_custom = mean(sent_custom == 0, na.rm = TRUE)
  )

write_csv(sent_summary, file.path("tables", "reviews_sentiment_scores_summary.csv"))

###############################################################
# 4) Descriptive relationship: sentiment vs satisfaction (review level)
###############################################################

corr_review <- reviews %>%
  summarise(
    corr_sentiment_stars = cor(sentiment_final, stars, use = "complete.obs")
  )

write_csv(
  corr_review,
  file.path("tables", "sentiment_stars_correlation_review_level.csv")
)

reg_review <- lm(stars ~ sentiment_final, data = reviews)

reg_summary <- broom::tidy(reg_review)

write_csv(
  reg_summary,
  file.path("tables", "sentiment_stars_regression_review_level.csv")
)

###############################################################
# 5) Restaurant-day KPI construction + sentiment link (daily level)
###############################################################

reviews_daily <- reviews %>%
  group_by(business_id, date) %>%
  summarise(
    daily_avg_stars = mean(stars, na.rm = TRUE),
    daily_review_count = n(),
    daily_sentiment = mean(sentiment_final, na.rm = TRUE),
    .groups = "drop"
  )

kpi_threshold <- mean(reviews_daily$daily_avg_stars, na.rm = TRUE)

reviews_daily <- reviews_daily %>%
  mutate(
    kpi_above_avg = as.integer(daily_avg_stars > kpi_threshold)
  )

sent_by_kpi <- reviews_daily %>%
  group_by(kpi_above_avg) %>%
  summarise(
    n_restaurant_days = n(),
    mean_daily_sentiment = mean(daily_sentiment, na.rm = TRUE),
    median_daily_sentiment = median(daily_sentiment, na.rm = TRUE),
    mean_daily_avg_stars = mean(daily_avg_stars, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(sent_by_kpi, file.path("tables", "sentiment_by_kpi_summary.csv"))
write_csv(reviews_daily, file.path("tables", "sentiment_kpi_restaurant_day.csv"))

tt <- try(t.test(daily_sentiment ~ kpi_above_avg, data = reviews_daily), silent = TRUE)

tt_out <- if (!inherits(tt, "try-error")) {
  tibble(
    statistic = unname(tt$statistic),
    p_value = tt$p.value,
    conf_low = tt$conf.int[1],
    conf_high = tt$conf.int[2],
    mean_group0 = unname(tt$estimate[1]),
    mean_group1 = unname(tt$estimate[2])
  )
} else {
  tibble(note = "t-test could not be computed (insufficient variation or missing sentiment).")
}

write_csv(tt_out, file.path("tables", "sentiment_kpi_ttest.csv"))

###############################################################
# 6) Analytical figures 
###############################################################

# Color palette
pal_light <- "#DCE3EA"
pal_mid1  <- "#CBD2DB"
pal_mid2  <- "#B7C4D1"
pal_dark  <- "#00618F"

# -------------------------------------------------------------
# Distribution of sentiment (overall)
# -------------------------------------------------------------

p1 <- ggplot(reviews, aes(x = sentiment_final)) +
  geom_histogram(bins = 50, fill = pal_mid1, color = "white") +
  theme_minimal(base_size = 13) +
  labs(
    x = "Sentiment (hybrid index)",
    y = "Count"
  )

ggsave(
  file.path("figures", "sentiment", "dist_sentiment_final.png"),
  plot = p1, width = 8, height = 5, dpi = 300
)

# -------------------------------------------------------------
# Review-level sentiment vs stars
# -------------------------------------------------------------

p2 <- ggplot(reviews, aes(x = sentiment_final, y = stars)) +
  geom_point(alpha = 0.15, color = pal_mid2) +
  geom_smooth(method = "lm", se = TRUE, color = pal_dark) +
  theme_minimal(base_size = 13) +
  labs(
    x = "Sentiment (hybrid index)",
    y = "Stars"
  )

ggsave(
  file.path("figures", "sentiment", "sentiment_final_vs_stars.png"),
  plot = p2, width = 8, height = 5, dpi = 300
)

# -------------------------------------------------------------
# Sentiment by KPI (main KPI figure)
# -------------------------------------------------------------

p3 <- reviews_daily %>%
  mutate(
    kpi_above_avg = factor(
      kpi_above_avg,
      levels = c(0, 1),
      labels = c("Low satisfaction days", "High satisfaction days")
    )
  ) %>%
  ggplot(aes(x = kpi_above_avg, y = daily_sentiment, fill = kpi_above_avg)) +
  geom_boxplot(
    width = 0.5,
    outlier.alpha = 0.15,
    color = pal_dark,      # dark outline
    size = 0.8
  ) +
  scale_fill_manual(
    values = c(
      "Low satisfaction days"  = pal_light,
      "High satisfaction days" = pal_mid2
    )
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank()
  ) +
  labs(
    x = NULL,
    y = "Daily sentiment (hybrid index)"
  )

ggsave(
  file.path("figures", "sentiment", "sentiment_by_kpi_boxplot.png"),
  plot = p3,
  width = 8,
  height = 5,
  dpi = 300
)
###############################################################
# 7) Save final enriched review dataset (descriptive-only)
###############################################################

write_csv(reviews, "reviews_tucson_enriched_descriptive.csv")
cat("Saved reviews_tucson_enriched_descriptive.csv with", nrow(reviews), "rows.\n")


