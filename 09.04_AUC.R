###############################################################
# Script: 09.03_AUC.R
# Purpose: 
#   Validate the Final Variable Set (AIC + Theory) using 
#   Random Forest Permutation Importance.
###############################################################

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(ranger)
library(pROC)
library(parallel)
library(ggplot2)
library(scales)
library(stringr)

# ------------------------------------------------------------
# 1. Setup & Load Data
# ------------------------------------------------------------
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
set.seed(123)

# ---------- output folders (ONLY change: directories) ----------
FIG_DIR <- file.path(getwd(), "figures", "variable_importance_final")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

TABLE_DIR <- file.path(getwd(), "results", "tables", "permutation_importance_final")
dir.create(TABLE_DIR, recursive = TRUE, showWarnings = FALSE)

# ---------- palette (ONLY change: colors) ----------
pal_light <- "#DCE3EA"
pal_mid1  <- "#CBD2DB"
pal_mid2  <- "#B7C4D1"
pal_dark  <- "#00618F"

# Load the CLEANED internal panel from Script 08
internal_panel <- read_csv("internal_panel_final.csv", show_col_types = FALSE)
cat("Loaded rows:", nrow(internal_panel), " | cols:", ncol(internal_panel), "\n")

# ------------------------------------------------------------
# 2. Feature Engineering (Recreating Transformations)
# ------------------------------------------------------------

# A. Construct Target
restaurant_means <- internal_panel %>%
  group_by(business_id) %>%
  summarise(mean_stars = mean(daily_avg_stars, na.rm = TRUE), .groups = "drop")

data <- internal_panel %>%
  left_join(restaurant_means, by = "business_id") %>%
  mutate(y_target = ifelse(daily_avg_stars >= mean_stars, 1, 0))

# B. Transformations (Must match AIC script logic)
data <- data %>%
  mutate(
    # 1. Covid
    covid_period = ifelse(
      as.Date(date) >= as.Date("2020-03-01") & 
        as.Date(date) <= as.Date("2023-05-31"),
      1, 0
    ),
    
    # 2. Log Review Count
    log_review_count = log(review_count + 1),
    
    # 3. Weather Transforms
    TAVG_sq = TAVG^2,
    is_raining = as.factor(ifelse(PRCP > 0, 1, 0))
  )

# ------------------------------------------------------------
# 3. Variable Selection (AIC Winners + Theory Rescues)
# ------------------------------------------------------------

final_vars <- c(
  # --- 1. AIC WINNERS (Statistical Selection) ---
  "daily_review_count", 
  "daily_checkins", 
  "lag_reviews_1d", 
  "roll_checkins_7d", 
  "cum_reviews", 
  "cum_checkins", 
  "log_review_count", 
  
  # Context / Weather 
  "TMAX_lag1", "TMIN_lag1", #AIC kept both lag1 and lag2, however, they were very similar.
  "Weekend", 
  "covid_period",
  "TAVG_sq",   
  "is_raining",
  "is_open",
  
  # Attributes (AIC Kept these)
  "delivery", "takeout", "good_for_groups", "alcohol_binary",
  
  # Categories (AIC Kept these)
  "cat_fast_food", "cat_dessert", "cat_dietary_specialty",
  
  # --- 2. LITERATURE REVIEW RESCUES (Theory Selection) ---
  # Rescued based on sections 2.1.1 and 2.1.3 of your paper
  "price_high",       # "Price-Quality Relationship"
  "outdoor_seating",  # "Physical Environment"
  "table_service",    # "Internal Determinants"
  "noise_high",       # "Atmosphere" (Crucial for Servicescape)
  "reservations",      # Linked to Table Service
  "wifi_binary"       # "Atmosphere / Service Convenience"
)

# NOTE: Removed 'restaurant_mean_stars' (Leakage)
# NOTE: Removed 'is_raining1' (Code uses 'is_raining')

# Check for missing columns (Safety Check)
missing_vars <- setdiff(final_vars, names(data))
if(length(missing_vars) > 0) {
  warning("Warning! These variables are missing: ", paste(missing_vars, collapse=", "))
}

# Select Data (old behavior preserved: drop_na() here)
df <- data %>%
  dplyr::select(y_target, any_of(final_vars)) %>%
  drop_na() %>%
  mutate(y_target = factor(y_target, levels = c(0, 1)))

cat("\nFinal Modeling Data:\n")
cat("Rows:", nrow(df), "| Columns:", ncol(df), "\n")
print(names(df))

# ------------------------------------------------------------
# 4. Train / Test Split
# ------------------------------------------------------------
set.seed(123)
idx <- sample.int(nrow(df), size = floor(0.70 * nrow(df)))
train <- df[idx, ]
test  <- df[-idx, ]

# Subsample test set for speed (optional but recommended for Permutation)
MAX_TEST_N <- 50000
test_imp <- if (nrow(test) > MAX_TEST_N) slice_sample(test, n = MAX_TEST_N) else test

# ------------------------------------------------------------
# 5. Fit Random Forest
# ------------------------------------------------------------
cat("\nTraining Random Forest... (Please wait)\n")
rf_imp <- ranger(
  y_target ~ .,
  data = train,
  probability = TRUE,
  num.trees = 300,
  importance = "permutation", 
  mtry = floor(sqrt(ncol(train) - 1)),
  respect.unordered.factors = "order"
)

# ------------------------------------------------------------
# 6. Permutation Importance (AUC Drop)
# ------------------------------------------------------------
# Baseline (old behavior preserved: predictions[, 2])
probs <- predict(rf_imp, data = test_imp)$predictions[, 2]
baseline_auc <- as.numeric(auc(roc(test_imp$y_target, probs, quiet = TRUE)))
cat("Baseline Model AUC:", round(baseline_auc, 4), "\n")

# Importance Loop
run_perm_importance <- function(model, test_data, baseline, features, repeats=3) { 
  results <- data.frame(variable = character(), mean_drop = numeric(), sd_drop = numeric())
  cat("Calculating Permutation Importance for", length(features), "features...\n")
  
  # Parallel setup
  n_cores <- max(1, parallel::detectCores() - 1)
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, { library(pROC); library(ranger) })
  clusterExport(cl, c("model", "test_data", "baseline", "repeats"), envir = environment())
  
  worker <- function(var) {
    drops <- numeric(repeats)
    for (i in 1:repeats) {
      temp_data <- test_data
      temp_data[[var]] <- sample(temp_data[[var]]) 
      preds <- predict(model, data = temp_data)$predictions[, 2]
      new_auc <- as.numeric(auc(roc(temp_data$y_target, preds, quiet = TRUE)))
      drops[i] <- baseline - new_auc
    }
    return(c(mean(drops), sd(drops)))
  }
  
  raw_results <- parSapply(cl, features, worker)
  stopCluster(cl)
  
  results <- data.frame(
    variable = colnames(raw_results),
    mean_auc_drop = raw_results[1, ],
    sd_auc_drop = raw_results[2, ]
  ) %>% arrange(desc(mean_auc_drop))
  
  return(results)
}

features_to_test <- setdiff(names(test_imp), "y_target")
imp_results <- run_perm_importance(rf_imp, test_imp, baseline_auc, features_to_test)

# ---------- table export (ONLY change: directory) ----------
write_csv(imp_results, file.path(TABLE_DIR, "permutation_importance_FINAL.csv"))

# ------------------------------------------------------------
# 7. Plot Results (ONLY change: colors)
# ------------------------------------------------------------
imp_plot <- imp_results %>%
  mutate(
    variable = str_replace_all(variable, "_", " "),
    variable = factor(variable, levels = rev(variable))
  )

p <- ggplot(imp_plot, aes(x = variable, y = mean_auc_drop)) +
  geom_col(aes(fill = mean_auc_drop), color = "white") +
  coord_flip() +
  
  # Academic blue/grey scale (replaces pink; nothing else changed)
  scale_fill_gradient(
    low = pal_mid1,
    high = pal_dark
  ) +
  
  labs(
    title = "Variable Importance (AUC Drop)",
    subtitle = paste("AIC Winners + Lit Review Rescues | Baseline AUC:", round(baseline_auc, 3)),
    x = NULL,
    y = "Drop in AUC when variable is shuffled"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank()
  )

print(p)

# ---------- figure export (ONLY change: directory) ----------
ggsave(
  file.path(FIG_DIR, "App_Fig_VarImportance_FINAL.png"),
  p, width = 10, height = 8, bg = "white"
)

cat("\nDone!\n")
cat("Saved table to:", file.path("results", "tables", "permutation_importance_final", "permutation_importance_FINAL.csv"), "\n")
cat("Saved plot  to:", file.path("figures", "variable_importance_final", "App_Fig_VarImportance_FINAL.png"), "\n")
