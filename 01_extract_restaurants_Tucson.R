###############################################################
# Script: 01_extract_business_tucson.R
# Author: Selena Rahimzadeh
#
# Purpose:
#   Extract all restaurant-like businesses located in Tucson, AZ
#   from the PostgreSQL Yelp database. This script provides the
#   foundational business dataset used for cleaning, merging with
#   review and check-in data, and building the daily analysis panel.
#
#   - Relational database extraction (SQL)
#   - Data acquisition, storage, and management
#   - Table selection & filtering
#   - Preparing raw marketing data for further analysis
#
# Output:
#   - restaurants_tucson.csv: all Tucson restaurant-like businesses
###############################################################

library(DBI)
library(RPostgres)
library(dplyr)
library(readr)

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
# Define restaurant-related category keywords
###############################################################

restaurant_categories <- c(
  "Restaurants", "Breakfast & Brunch", "Fast Food", "Sandwiches", "Diners",
  "Pizza", "Burgers", "Seafood", "Steakhouses",
  "Sushi Bars", "Tacos", "Soup", "Soul Food", "Bars"
)

###############################################################
# Extract Tucson business table
###############################################################

sql_business_tucson <- "
SELECT
    j->>'business_id' AS business_id,
    j->>'name' AS name,
    j->>'city' AS city,
    j->>'state' AS state,
    j->>'categories' AS categories,
    (j->>'stars')::float AS stars,
    (j->>'review_count')::int AS review_count,
    (j->>'is_open')::int AS is_open,
    (j->>'latitude')::float AS latitude,
    (j->>'longitude')::float AS longitude,
    -- Business attributes
    (j->'attributes'->>'WiFi') AS wifi,
    (j->'attributes'->>'Alcohol') AS alcohol,
    (j->'attributes'->>'NoiseLevel') AS noise_level,
    (j->'attributes'->>'RestaurantsPriceRange2') AS price_range,
    (j->'attributes'->>'OutdoorSeating') AS outdoor_seating,
    (j->'attributes'->>'RestaurantsDelivery') AS restaurants_delivery,
    (j->'attributes'->>'RestaurantsTakeOut') AS restaurants_take_out,
    (j->'attributes'->>'RestaurantsReservations') AS restaurants_reservations,
    (j->'attributes'->>'RestaurantsGoodForGroups') AS restaurants_good_for_groups,
    (j->'attributes'->>'RestaurantsTableService') AS restaurants_table_service,
    (j->'attributes'->>'BusinessAcceptsCreditCards') AS business_accepts_credit_cards
FROM business
WHERE j->>'city' = 'Tucson';
"

business_raw <- dbGetQuery(con, sql_business_tucson)

cat("Fetched", nrow(business_raw), "total Tucson businesses.\n")

###############################################################
# Filter for restaurant-like businesses only
###############################################################

restaurants_tucson <- business_raw %>%
  filter(!is.na(categories)) %>%
  filter(
    Reduce(
      `|`,
      lapply(restaurant_categories, \(k) grepl(k, categories, ignore.case = TRUE))
    )
  ) %>%
  filter(!grepl("Street Vendors", categories, ignore.case = TRUE))

cat("Identified", nrow(restaurants_tucson),
    "restaurant-like businesses in Tucson.\n")

###############################################################
# Export dataset
###############################################################

write_csv(restaurants_tucson, "restaurants_tucson.csv")
cat("Saved: restaurants_tucson.csv\n")

###############################################################
# Disconnect
###############################################################

dbDisconnect(con)
cat("Connection closed.\n")
