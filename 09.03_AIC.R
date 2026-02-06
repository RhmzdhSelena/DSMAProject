###############################################################
# Script: 09.01_AIC.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Exploratory AIC-based variable screening using
#   the RAW internal panel (internal_panel_final.csv).
#
#   This script:
#   1. Loads the master panel.
#   2. Constructs y_target and covid_period on the fly.
#   3. Removes IDs and Leakage (daily_avg_stars).
#   4. Runs AIC on ALL remaining variables to see what sticks.
#
###############################################################

# ------------------------------------------------------------
# 1. Set working directory
# ------------------------------------------------------------
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
cat("Working directory:", getwd(), "\n\n")

# ------------------------------------------------------------
# 2. Load libraries
# ------------------------------------------------------------
library(dplyr)
library(MASS)   # stepAIC
library(pROC)   # For AUC calculation at the end

# ------------------------------------------------------------
# 2B. Output folder (one figure per folder rule)
# ------------------------------------------------------------
FIG_DIR <- file.path(getwd(), "figures", "correlation_matrix_aic")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2C. Palette 
# ------------------------------------------------------------
pal_light <- "#DCE3EA"
pal_mid1  <- "#CBD2DB"
pal_mid2  <- "#B7C4D1"
pal_dark  <- "#00618F"
# pal_accent <- "#fab1a0"  # not used here

# ------------------------------------------------------------
# 3. Load MASTER panel
# ------------------------------------------------------------
data <- read.csv("internal_panel_final.csv")

cat("Loaded FULL MASTER panel:",
    nrow(data), "rows |",
    ncol(data), "columns\n\n")

# ------------------------------------------------------------
# 4. Feature Engineering
# ------------------------------------------------------------

# A. Construct Target (y_target)
restaurant_means <- data %>%
  group_by(business_id) %>%
  summarise(
    restaurant_mean_stars = mean(daily_avg_stars, na.rm = TRUE),
    .groups = "drop"
  )

data <- data %>%
  left_join(restaurant_means, by = "business_id") %>%
  mutate(
    y_target = ifelse(daily_avg_stars >= restaurant_mean_stars, 1, 0)
  )

# B. Construct Covid Dummy
data <- data %>%
  mutate(
    covid_period = ifelse(
      as.Date(date) >= as.Date("2020-03-01") &
        as.Date(date) <= as.Date("2023-05-31"),
      1, 0
    )
  )

# C. NON-LINEAR TRANSFORMATIONS (ADDED)
# Based on diagnostic plots, we improve how the model sees data.

# 1. Log-Transform 'review_count' (Diminishing returns)
#    We add 1 to avoid log(0) errors.
data$log_review_count <- log(data$review_count + 1)

# 2. Quadratic Terms for Temperature (U-Shaped relationships)
#    We create these so AIC can choose: "Linear only" OR "Linear + Squared"
data$TAVG_sq <- data$TAVG^2
data$TMAX_sq <- data$TMAX^2
data$TMIN_sq <- data$TMIN^2

# 3. Rain Dummy (Noise reduction)
#    Raw PRCP was noisy; a simple "Is it raining?" flag is often better.
data$is_raining <- as.factor(ifelse(data$PRCP > 0, 1, 0))

# ------------------------------------------------------------
# 5. Prepare Data for AIC
# ------------------------------------------------------------
aic_data <- data %>%
  filter(!is.na(y_target))

# List of columns to REMOVE
drop_cols <- c(
  # --- IDs & Metadata ---
  "business_id", "date", "name", "city", "state",
  "latitude", "longitude", "STATION", "Quarter",
  
  # --- Leakage (Must Remove) ---
  "daily_avg_stars", "mean_stars", "stars",
  
  # --- Raw Variables (Replaced by Transforms) ---
  "review_count",   # Replaced by log_review_count
  "PRCP",           # Replaced by is_raining
  "TMAX", "TMIN",   # We focus on TAVG and TAVG_sq
  
  # --- Audit Flags / Noise ---
  "miss_TMAX", "miss_TMIN", "miss_PRCP", "miss_TAVG",
  "miss_wifi", "miss_alcohol", "miss_noise",
  "outlier_daily_review_count", "outlier_daily_checkins",
  "inactive_flag",
  
  # --- Raw Attributes (Safety check - likely already removed in clean panel) ---
  "categories", "wifi", "alcohol", "noise_level", "price_range"
)

# Robust Selection using any_of()
aic_data <- aic_data %>%
  dplyr::select(-any_of(drop_cols))

# Convert characters/logicals to factors
aic_data <- aic_data %>%
  mutate(across(where(is.character), as.factor)) %>%
  mutate(across(where(is.logical), as.factor))

cat("Candidate predictors remaining:", ncol(aic_data) - 1, "\n")
cat("Predictors list (passed to AIC):\n")
print(names(aic_data))
cat("\n")

# ------------------------------------------------------------
# 6. Fixed Sample (Remove NAs)
# ------------------------------------------------------------
aic_data_fixed <- na.omit(aic_data)

cat("Rows before NA removal:", nrow(aic_data), "\n")
cat("Rows after NA removal:", nrow(aic_data_fixed), "\n\n")

# ------------------------------------------------------------
# 7. Train/Test Split (Speed Optimization)
# ------------------------------------------------------------
set.seed(123)
n_total <- nrow(aic_data_fixed)

# Use 70% sample
train_index <- sample(seq_len(n_total), size = floor(0.7 * n_total))
aic_train <- aic_data_fixed[train_index, ]
aic_test  <- aic_data_fixed[-train_index, ] # Validation set for later

cat("AIC training sample size:", nrow(aic_train), "\n\n")

# ------------------------------------------------------------
# 8. Run Full Model
# ------------------------------------------------------------
# "y_target ~ ." means "Predict y_target using ALL other columns"
full_formula <- as.formula("y_target ~ .")

full_model <- glm(
  full_formula,
  data = aic_train,
  family = binomial(link = "logit")
)

cat("========== FULL MODEL AIC ==========\n")
print(AIC(full_model))
cat("\n")

# ------------------------------------------------------------
# 9. Stepwise AIC Selection
# ------------------------------------------------------------
cat("Starting Stepwise Selection... (This may take a few minutes)\n")

aic_model <- stepAIC(
  full_model,
  direction = "both",
  trace = FALSE
)

cat("\n========== AIC-SELECTED MODEL SUMMARY ==========\n")
print(summary(aic_model))

# ------------------------------------------------------------
# 10. Output Selected Variables
# ------------------------------------------------------------
kept_vars <- names(coef(aic_model))[-1]

cat("\n========== VARIABLES KEPT BY AIC ==========\n")
print(kept_vars)

###############################################################
# 11. VISUALIZATION: Correlation Heatmap of "Winning" Variables
###############################################################
library(ggplot2)
library(reshape2)

# Check if aic_model exists (Steps 1-10 must be run first!)
if (!exists("aic_model")) stop("You must run the AIC Stepwise Selection (Step 9) first!")

# 1. Extract the raw variable names from the final AIC model
final_vars <- all.vars(formula(aic_model))

# Remove the target variable (we only want predictors)
plot_vars <- setdiff(final_vars, "y_target")

cat("Plotting correlation for:", length(plot_vars), "variables.\n")

# 2. Prepare Data for Plotting
#    Select only the winning variables and convert everything to numeric
corr_data <- aic_data_fixed %>%
  dplyr::select(all_of(plot_vars)) %>%
  mutate(across(everything(), as.numeric))

# 3. Calculate Correlation Matrix
cormat <- round(cor(corr_data, use = "complete.obs"), 2)

# 4. Melt for ggplot
melted_cormat <- melt(cormat)

# 5. Add rounded labels (as in your old script)
melted_cormat <- melted_cormat %>%
  mutate(value_lab = sprintf("%.2f", value))

# 6. Generate Heatmap 
p_corr <- ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = pal_mid2, mid = "white", high = pal_dark,
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 11),
    legend.title = element_text(size = 11),
    legend.text = element_text(size = 9),
    plot.margin = margin(10, 10, 10, 10)
  ) +
  geom_text(aes(label = value_lab), color = "black", size = 2.4) +
  labs(
    title = "Correlation Matrix of AIC-Selected Predictors",
    subtitle = "Checking for Multicollinearity among the entire panel"
  )

# Save (ONLY folder changed; width/height/dpi unchanged from your old script)
ggsave(
  filename = file.path(FIG_DIR, "App_Fig_Correlation_Matrix_AIC.png"),
  plot = p_corr,
  width = 14, height = 12, units = "in",
  dpi = 300,
  bg = "white"
)

cat("Saved graphic to:", file.path("figures", "correlation_matrix_aic", "App_Fig_Correlation_Matrix_AIC.png"), "\n")

# ------------------------------------------------------------
# 12. VALIDATION: Calculate AUC of the AIC-Selected Model
# ------------------------------------------------------------
cat("\n========== VALIDATION: AUC CALCULATION ==========\n")

pred_probs <- predict(aic_model, newdata = aic_test, type = "response")

roc_obj <- roc(aic_test$y_target, pred_probs, quiet = TRUE)
auc_value <- auc(roc_obj)

cat("AIC-Selected Model AUC:", round(auc_value, 4), "\n")
cat("Interpretation: 0.5 = Random Guessing | 1.0 = Perfect Prediction\n")

setdiff(names(data), all.vars(formula(aic_model)))
