###############################################################
# Script: 05_extract_checkins_tucson.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Extract all Yelp check-ins for Tucson restaurant businesses.
#   Yelp stores check-ins inside a JSON column ("j") where the 
#   "date" field is a comma-separated string of dates. This script
#   loads Tucson business_ids, queries the checkin table, flattens 
#   the date arrays, and outputs a structured dataset for later 
#   daily aggregation
#
# Notes:
#   - Input: restaurants_tucson_raw.csv
#   - Uses JSON extraction: j->>'business_id', j->>'date'
#   - UNNEST(string_to_array()) converts date strings into rows.
#   - Missing or empty date fields are preserved.
#
# Output:
#   checkins_tucson_raw.csv
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

cat("Loaded", length(business_ids), "Tucson restaurant IDs.\n")

###############################################################
# Connect to PostgreSQL
###############################################################

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "remoteuser",
  host = "localhost",
  port = 5432,
  user = "dsma_student",
  password = "DSMA_Stud23"
)

cat("Connected to database.\n")

###############################################################
# Chunking for large IN() query
###############################################################

chunk_size <- 500
chunks <- split(business_ids, ceiling(seq_along(business_ids) / chunk_size))

check_list <- list()

###############################################################
# Extract check-ins
###############################################################

for (i in seq_along(chunks)) {
  
  ids <- paste0("'", chunks[[i]], "'", collapse = ",")
  
  sql <- sprintf("
        SELECT
            j->>'business_id' AS business_id,
            unnest(string_to_array(j->>'date', ','))::date AS checkin_date
        FROM checkin
        WHERE j->>'business_id' IN (%s);
    ", ids)
  
  cat("Extracting chunk", i, "of", length(chunks), "\n")
  
  res <- dbGetQuery(con, sql)
  check_list[[length(check_list) + 1]] <- res
}

checkins_tucson <- bind_rows(check_list)

###############################################################
# Save output
###############################################################

write_csv(checkins_tucson, "checkins_tucson_raw.csv")

cat("Saved checkins_tucson_raw.csv with", 
    nrow(checkins_tucson), "rows and",
    length(unique(checkins_tucson$business_id)), "unique business_ids.\n")

###############################################################
# Disconnect
###############################################################

dbDisconnect(con)
cat("Database connection closed.\n")

checkins <- read_csv("checkins_tucson_raw.csv")
length(unique(checkins$business_id))
setdiff(unique(checkins$business_id), restaurants$business_id)
