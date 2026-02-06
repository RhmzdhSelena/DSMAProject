###############################################################
# Script: 02_clean_business_attributes.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Clean and standardize business attributes for all restaurant 
#   businesses in Tucson. This includes transforming inconsistent 
#   attribute formats, coding categorical variables, detecting
#   missing values, and preparing the dataset for subsequent 
#   integration with reviews, check-ins, and weather data.
#
# Notes:
# - The input file restaurants_tucson.csv is created in Script 01
#     after filtering all valid restaurant-like businesses.
# - Yelp attributes often contain inconsistent or unstructured text 
#     ("free", "TRUE", "u'paid'"). This script harmonizes them into 
#     binary or factor variables.
# - This script cleans and standardizes all static restaurant 
#   attributes in the Tucson dataset. 
#
# - A universal text-cleaning function removes unicode prefixes, 
#   stray quotation marks, and whitespace, and converts all values 
#   to a consistent lowercase format. Domain-specific cleaning 
#   functions then map attributes like WiFi availability, alcohol 
#   service, and noise level into clearly defined categories.
#
# - Boolean attributes (e.g., delivery, takeout, reservations) 
#   are converted into binary 0/1 indicators. Price range values 
#   are converted into numeric form.
#
# - The script intentionally keeps only the cleaned variables and 
#   removes the original raw Yelp attribute columns to avoid 
#   confusion and ensure that all downstream scripts work 
#   exclusively with harmonized attributes.
#
# - Missing values are preserved at this stage. No imputation is 
#   performed here. Instead, missingness is handled explicitly in 
#   Script 03 using MICE and missing-data indicators. This separation 
#   ensures full methodological transparency and prevents hidden or 
#   uncontrolled data modifications.
# Output:
#   restaurants_tucson_clean.csv
#   restaurants_tucson_missing_summary.csv
###############################################################

library(dplyr)
library(readr)
library(stringr)
library(tidyr)

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")

###############################################################
# 1. Load raw restaurant data
###############################################################

raw <- read_csv("restaurants_tucson.csv", show_col_types = FALSE)
cat("Loaded raw dataset with", nrow(raw), "restaurants.\n")


###############################################################
# 2. Universal safe string cleaner
###############################################################

clean_string <- function(x) {
  x <- tolower(as.character(x))
  x <- str_replace_all(x, "u'", "")
  x <- str_replace_all(x, "'", "")
  x <- str_trim(x)
  
  # Only true missing markers → NA
  x <- ifelse(x %in% c("", "na", "null", "none_selected"), NA, x)
  
  return(x)
}


###############################################################
# 3. Specific attribute cleaners
###############################################################

clean_wifi <- function(x) {
  x <- clean_string(x)
  case_when(
    str_detect(x, "free") ~ "free",
    str_detect(x, "paid") ~ "paid",
    x == "no" ~ "no",
    TRUE ~ NA_character_
  )
}

clean_alcohol <- function(x) {
  x <- clean_string(x)
  case_when(
    x == "none" ~ "none",
    str_detect(x, "wine") ~ "beer_and_wine",
    str_detect(x, "full") ~ "full_bar",
    TRUE ~ NA_character_
  )
}

clean_noise <- function(x) {
  x <- clean_string(x)
  case_when(
    str_detect(x, "quiet") ~ "quiet",
    str_detect(x, "average") ~ "average",
    x == "loud" ~ "loud",
    str_detect(x, "very") ~ "very_loud",
    TRUE ~ NA_character_
  )
}

clean_bool <- function(x) {
  x <- clean_string(x)
  case_when(
    x %in% c("true", "yes", "1", "t") ~ 1,
    x %in% c("false", "no", "0", "f") ~ 0,
    TRUE ~ NA_real_
  )
}


###############################################################
# 4. Apply cleaners to dataset
###############################################################

clean <- raw %>%
  mutate(
    wifi          = clean_wifi(wifi),
    alcohol       = clean_alcohol(alcohol),
    noise_level   = clean_noise(noise_level),
    
    outdoor_seating = clean_bool(outdoor_seating),
    delivery        = clean_bool(restaurants_delivery),
    takeout         = clean_bool(restaurants_take_out),
    reservations    = clean_bool(restaurants_reservations),
    good_for_groups = clean_bool(restaurants_good_for_groups),
    table_service   = clean_bool(restaurants_table_service),
    credit_cards    = clean_bool(business_accepts_credit_cards),
    
    price_range = suppressWarnings(as.numeric(price_range))
  )


###############################################################
# 5. Add missingness flags (needed for Script 03 MNAR handling)
###############################################################

clean <- clean %>%
  mutate(
    miss_wifi      = ifelse(is.na(wifi), 1, 0),
    miss_alcohol   = ifelse(is.na(alcohol), 1, 0),
    miss_noise     = ifelse(is.na(noise_level), 1, 0)
  )

###############################################################
# 6. Select final cleaned variables (drop all raw messy columns)
###############################################################

restaurants_clean <- clean %>%
  select(
    business_id, name, city, state, categories,
    stars, review_count, latitude, longitude, is_open,
    
    wifi, alcohol, noise_level,
    outdoor_seating, delivery, takeout, reservations,
    good_for_groups, table_service, credit_cards,
    price_range,
    
    miss_wifi, miss_alcohol, miss_noise
  )


###############################################################
# 7. Save results
###############################################################

write_csv(restaurants_clean, "restaurants_tucson_clean.csv")
cat("Saved final clean table: restaurants_tucson_clean.csv\n")

# ------------------------------------------------------------
# Missingness summary (diagnostic output for documentation)
# ------------------------------------------------------------
dir.create(file.path("tables"), showWarnings = FALSE, recursive = TRUE)

missing_summary <- restaurants_clean %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "share_missing") %>%
  arrange(desc(share_missing))

write_csv(
  missing_summary,
  file.path("tables", "missing_summary_restaurants_tucson_clean.csv")
)
