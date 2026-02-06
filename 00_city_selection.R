###############################################################
# Script: 00_full_city_selection.R
# Author: Selena-Leila Rahimzadeh
#
## Purpose:
#   This script prepares the foundation for the empirical analysis by identifying
#   the most suitable city to focus on. It evaluates all available U.S. cities
#   in the Yelp dataset based on restaurant sample size, attribute completeness,
#   and review density. The resulting ranking guides the selection of the target
#   city for all subsequent data extraction, cleaning, and modeling steps.
#
# Notes:
#   The ranking in this script is based solely on business- and review-level
#   data quality (sample size, attribute completeness, and review density).
#   The final selection of the analysis city also considers external criteria,
#   including the availability, granularity, and temporal stability of local
#   weather data. These additional factors are evaluated separately to ensure
#   that the chosen city supports a reliable weather-based modeling framework.

###############################################################

library(DBI)
library(RPostgres)
library(dplyr)
library(readr)
library(tidyr)

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
# Extract all business records from DB
###############################################################

sql_business <- "
SELECT
    j->>'business_id' AS business_id,
    j->>'name' AS name,
    j->>'city' AS city,
    j->>'state' AS state,
    j->>'categories' AS categories,
    (j->>'stars')::float AS stars,
    (j->>'review_count')::int AS review_count
FROM business;
"
business_raw <- dbGetQuery(con, sql_business)

###############################################################
# Check Keywords we might need
###############################################################
sql_all_categories <- "
SELECT DISTINCT TRIM(cat) AS category
FROM (
    SELECT unnest(string_to_array(j->>'categories', ',')) AS cat
    FROM business
) AS t
WHERE cat IS NOT NULL AND cat <> '';
"

all_categories <- dbGetQuery(con, sql_all_categories)

View(all_categories)

###############################################################
# Identify restaurant-like businesses (restaurants_only)
# using hand-picked Keywords
###############################################################
keywords <- c(
  "Restaurants", "Breakfast & Brunch", "Cafes", "Cafe",
  "Coffee & Tea", "Fast Food", "Sandwiches", "Diners",
  "Pizza", "Burgers", "Seafood", "Steakhouses",
  "Sushi Bars", "Tapas Bars", "Tapas/Small Plates",
  "Tex-Mex", "Soup", "Soul Food"
)

restaurants_only <- business_raw %>%
  filter(!is.na(categories)) %>%
  filter(
    Reduce(
      `|`,
      lapply(keywords, \(k) grepl(k, categories, ignore.case = TRUE))
    )
  )

###############################################################
# Build review_features (business-level review metadata)
###############################################################

review_features <- restaurants_only %>%
  select(
    business_id,
    city,
    review_count
  )

###############################################################
#  Extract business attributes → internal_dataset
# We will need the attributes in order to evaluate the amount of missings in our data
###############################################################

sql_attributes <- "
SELECT
    j->>'business_id' AS business_id,
    j->>'city' AS city,
    (j->'attributes'->>'WiFi') AS wifi,
    (j->'attributes'->>'Alcohol') AS alcohol,
    (j->'attributes'->>'NoiseLevel') AS noise_level,
    (j->'attributes'->>'Smoking') AS smoking,
    (j->'attributes'->>'Caters') AS caters,
    (j->'attributes'->>'HasTV') AS has_tv,
    (j->'attributes'->>'DogsAllowed') AS dogs_allowed,
    (j->'attributes'->>'OutdoorSeating') AS outdoor_seating,
    (j->'attributes'->>'RestaurantsAttire') AS restaurants_attire,
    (j->'attributes'->>'RestaurantsTakeOut') AS restaurants_take_out,
    (j->'attributes'->>'RestaurantsDelivery') AS restaurants_delivery,
    (j->'attributes'->>'RestaurantsReservations') AS restaurants_reservations,
    (j->'attributes'->>'RestaurantsGoodForGroups') AS restaurants_good_for_groups,
    (j->'attributes'->>'RestaurantsTableService') AS restaurants_table_service,
    (j->'attributes'->>'PriceRange') AS price_range,
    (j->'attributes'->>'WheelchairAccessible') AS wheelchair_accessible,
    (j->'attributes'->>'BusinessAcceptsCreditCards') AS business_accepts_credit_cards
FROM business;
"

internal_raw <- dbGetQuery(con, sql_attributes)

internal_dataset <- internal_raw %>%
  filter(business_id %in% restaurants_only$business_id)

###############################################################
# City-ranking 
###############################################################

# Only consider cities with >= 200 restaurants #statistically appropriate 
city_size <- restaurants_only %>%
  group_by(city) %>%
  summarise(n_restaurants = n()) %>%
  filter(n_restaurants >= 200)

cities_to_keep <- city_size$city

internal_f <- internal_dataset %>% filter(city %in% cities_to_keep)
restaurants_f <- restaurants_only %>% filter(city %in% cities_to_keep)
reviews_f <- review_features %>% filter(business_id %in% restaurants_f$business_id)

# Variables needed for modeling
vars_needed <- c(
  "wifi", "alcohol", "noise_level", "smoking", "caters",
  "has_tv", "dogs_allowed", "outdoor_seating", "drive_thru",
  "happy_hour", "restaurants_attire", "restaurants_take_out",
  "restaurants_delivery", "restaurants_reservations",
  "restaurants_table_service", "restaurants_good_for_groups",
  "price_range", "wheelchair_accessible",
  "business_accepts_credit_cards"
)

vars_needed <- vars_needed[vars_needed %in% colnames(internal_f)]

# Missingness ranking
missingness <- internal_f %>%
  select(city, all_of(vars_needed)) %>%
  group_by(city) %>%
  summarise(missing_share = mean(is.na(across(all_of(vars_needed))))) %>%
  arrange(missing_share)

# Review density
review_density <- reviews_f %>%
  select(business_id, review_count) %>%
  inner_join(
    restaurants_f %>% select(business_id, city),
    by = "business_id"
  ) %>%
  group_by(city) %>%
  summarise(avg_reviews_per_restaurant = mean(review_count, na.rm = TRUE)) %>%
  arrange(desc(avg_reviews_per_restaurant))

# Merge & rank
city_scores <- city_size %>%
  left_join(missingness, by = "city") %>%
  left_join(review_density, by = "city") %>%
  mutate(
    size_rank    = rank(-n_restaurants),
    missing_rank = rank(missing_share),
    review_rank  = rank(-avg_reviews_per_restaurant),
    final_score  = size_rank + missing_rank + review_rank
  ) %>%
  arrange(final_score)

print(city_scores)

###############################################################
# Disconnect
###############################################################
dbDisconnect(con)

