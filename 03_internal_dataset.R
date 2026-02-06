# -------------------------------------
# Script: 03_internal_dataset.R
# Author: Selena-Leila Rahimzadeh
# Purpose:Prepare the internal (static) dataset for all restaurants 
#   in Tucson. This script selects, renames, and organizes all 
#   cleaned restaurant-level attributes that do not vary over 
#   time. These structural characteristics will later be merged 
#   with daily review, check-in, and weather features.
# Notes: 
# Notes:
#   - Input file restaurants_tucson_clean.csv is produced in
#     Script 02 and contains cleaned restaurant attributes.
#
#   - This script includes only static structural characteristics.
#     Time-varying behavioral features (reviews, check-ins, and
#     weather) are constructed in later scripts and merged at
#     the restaurant-day level.
#
#   - Variables with structurally meaningful missingness
#     (wifi, alcohol, noise_level) are treated as MNAR:
#         → Missingness indicators are explicitly added.
#         → Missing values are recoded into a separate "missing"
#           category.
#         → No imputation is performed for these variables.
#
#   - Service and pricing attributes (outdoor_seating, delivery,
#     takeout, reservations, good_for_groups, table_service,
#     credit_cards, price_range) are treated as MAR/MCAR.
#     Missing values in these variables are imputed using
#     Multiple Imputation by Chained Equations (MICE) to
#     preserve sample size and avoid listwise deletion.
#
#   - Imputation is applied exclusively to static business
#     attributes. Behavioral and daily variables are not handled
#     in this script.
#
#   - Both unimputed and imputed versions of the internal dataset
#     are saved to ensure full methodological transparency and
#     reproducibility.

library(dplyr)
library(readr)
library(mice)

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")

###############################################################
# 1. Load cleaned dataset
###############################################################

clean <- read_csv("restaurants_tucson_clean.csv", show_col_types = FALSE)
cat("Loaded cleaned dataset with", nrow(clean), "restaurants.\n")


###############################################################
# 2. Select static structural variables
###############################################################

internal <- clean %>%
  select(
    business_id, name, city, state, categories,
    stars, review_count, latitude, longitude, is_open,
    
    # MNAR — do NOT impute
    wifi,
    alcohol,
    noise_level,
    
    # MAR/MCAR — will be imputed
    outdoor_seating,
    delivery,
    takeout,
    reservations,
    good_for_groups,
    table_service,
    credit_cards,
    price_range
  )

write_csv(internal, "internal_dataset_tucson_unimputed.csv")


###############################################################
# 3. MNAR handling:
#    Add missingness flag + convert NA → "missing"
###############################################################

internal <- internal %>%
  mutate(
    miss_wifi        = ifelse(is.na(wifi), 1, 0),
    miss_alcohol     = ifelse(is.na(alcohol), 1, 0),
    miss_noise       = ifelse(is.na(noise_level), 1, 0),
    
    wifi        = ifelse(is.na(wifi),        "missing", wifi),
    alcohol     = ifelse(is.na(alcohol),     "missing", alcohol),
    noise_level = ifelse(is.na(noise_level), "missing", noise_level)
  )


###############################################################
# 4. Prepare MAR variables for MICE imputation
###############################################################

mice_vars <- c(
  "outdoor_seating", "delivery", "takeout",
  "reservations", "good_for_groups",
  "table_service", "credit_cards", "price_range"
)

mice_block <- internal[mice_vars]

cat("Missing values before MICE:\n")
print(colSums(is.na(mice_block)))
dir.create("tables", showWarnings = FALSE, recursive = TRUE)

missing_before <- internal %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing") %>%
  mutate(share_missing = n_missing / nrow(internal)) %>%
  arrange(desc(share_missing))

write_csv(missing_before, file.path("tables", "internal_missingness_before.csv"))

###############################################################
# 5. Run MICE imputation
###############################################################

if (any(is.na(mice_block))) {
  
  set.seed(123)
  
  mice_mod <- mice(
    mice_block,
    m = 5,
    maxit = 20,
    printFlag = TRUE
  )
  
  mice_complete <- complete(mice_mod, 1)
  
  internal[mice_vars] <- mice_complete
}
missing_after <- internal %>%
  summarise(across(all_of(mice_vars), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing_after")

write_csv(missing_after, file.path("tables", "internal_missingness_after_mice.csv"))

###############################################################
# 6. Save final internal dataset
###############################################################

write_csv(internal, "internal_dataset_tucson_imputed.csv")
cat("Saved final internal dataset: internal_dataset_tucson_imputed.csv\n")
