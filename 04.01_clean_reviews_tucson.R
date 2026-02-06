###############################################################
# Script: 04.01_clean_reviews_tucson.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Clean and validate Tucson Yelp reviews WITHOUT computing
#   sentiment. This clean review table is used as the input for
#   the restaurant-day panel pipeline
#
# Performs:
#   - Type conversion
#   - Duplicate removal
#   - Text cleaning & UTF-8 validation
#   - Word count (textual richness)
#   - Outlier flags (stars/useful/funny/cool)
#
# Notes:
#   - Behavioral variables are NEVER imputed
#   - Sentiment is handled separately for descriptive analysis
#
# Input:
#   - reviews_tucson_raw.csv  (from Script 04.00)
#
# Output:
#   - reviews_tucson_clean.csv
###############################################################

###############################################################
# 0. Setup
###############################################################

rm(list = ls())

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")


cat("Working directory:", getwd(), "\n\n")

library(dplyr)
library(readr)
library(stringr)
library(utf8)

###############################################################
# 1. Load RAW reviews
###############################################################

reviews <- read_csv("reviews_tucson_raw.csv", show_col_types = FALSE)
cat("Loaded", nrow(reviews), "raw reviews.\n")

reviews <- reviews %>%
  mutate(
    review_id = as.character(review_id),
    user_id   = as.character(user_id),
    date      = as.Date(date)
  ) %>%
  distinct(review_id, .keep_all = TRUE)

###############################################################
# 2. Text cleaning & UTF-8 fix
###############################################################

reviews <- reviews %>%
  mutate(
    text_clean = text %>%
      str_replace_all("[\r\n]", " ") %>%
      str_squish()
  )

invalid_idx <- which(!utf8_valid(reviews$text_clean))
if (length(invalid_idx) > 0) {
  reviews$text_clean[invalid_idx] <- iconv(
    reviews$text_clean[invalid_idx],
    from = "",
    to   = "UTF-8"
  )
}

###############################################################
# 2.1 Textual richness
###############################################################

reviews <- reviews %>%
  mutate(
    word_count = str_count(text_clean, "\\S+")
  )

###############################################################
# 3. Outlier flags (IQR rule)
###############################################################

iqr_flag <- function(df, var) {
  x <- df[[var]]
  qs <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
  IQR <- qs[2] - qs[1]
  df[[paste0("outlier_", var)]] <-
    ifelse(x < qs[1] - 1.5 * IQR | x > qs[2] + 1.5 * IQR, 1, 0)
  df
}

for (v in c("stars","useful","funny","cool")) {
  reviews <- iqr_flag(reviews, v)
}

###############################################################
# 4. Save final clean review dataset
###############################################################

write_csv(reviews, "reviews_tucson_clean.csv")

cat("Saved reviews_tucson_clean.csv with", nrow(reviews), "rows.\n")

###############################################################
# END OF SCRIPT
###############################################################