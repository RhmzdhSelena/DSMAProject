###############################################################
# Script: 04_extract_reviews_tucson.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Extract all Yelp reviews belonging to restaurants in Tucson.
#   The script loads the Tucson business_id list (created in 
#   Script 01), queries the PostgreSQL review table in chunks 
#   to avoid memory and SQL limits, and saves a structured raw 
#   review dataset for further cleaning and feature engineering.
#
# Notes:
#   - Input: restaurants_tucson_raw.csv (Tucson-only restaurants)
#   - Only reviews belonging to these business_ids are extracted.
#   - Chunking avoids RAM overload and PostgreSQL IN() limits.
#   - All missing values are intentionally preserved.
#
# Output:
#   reviews_tucson_raw.csv
###############################################################

library(DBI)
library(RPostgres)
library(dplyr)
library(readr)

###############################################################
# Load Tucson restaurant IDs
###############################################################

restaurants <- read_csv("restaurants_tucson_clean.csv", show_col_types = FALSE)

business_ids <- restaurants$business_id
cat("Loaded", length(business_ids), "Tucson business IDs.\n")

###############################################################
# Extract reviews in chunks (to avoid SQL IN() limits)
###############################################################

chunk_size <- 500
chunks <- split(business_ids, ceiling(seq_along(business_ids) / chunk_size))

all_reviews <- list()

for (i in seq_along(chunks)) {
  
  ids <- paste0("'", chunks[[i]], "'", collapse = ",")
  
  sql <- sprintf("
        SELECT
            j->>'review_id'      AS review_id,
            j->>'user_id'        AS user_id,
            j->>'business_id'    AS business_id,
            (j->>'stars')::float AS stars,
            j->>'date'           AS date,
            j->>'text'           AS text,
            (j->>'useful')::int  AS useful,
            (j->>'funny')::int   AS funny,
            (j->>'cool')::int    AS cool
        FROM review
        WHERE j->>'business_id' IN (%s);
    ", ids)
  
  cat("Extracting chunk", i, "of", length(chunks), "\n")
  
  res <- dbGetQuery(con, sql)
  all_reviews[[length(all_reviews) + 1]] <- res
}

reviews_tucson <- bind_rows(all_reviews)

###############################################################
# Save results
###############################################################

write_csv(reviews_tucson, "reviews_tucson_raw.csv")
cat("Saved reviews_tucson_raw.csv with", nrow(reviews_tucson), "rows.\n")

dbDisconnect(con)
cat("Database connection closed.\n")

raws <- read_csv("reviews_tucson_raw.csv", show_col_types = FALSE)
