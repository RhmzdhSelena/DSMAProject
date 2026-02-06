###############################################################
# Script: 11_Logistic_Regression_Final.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Logistic regression benchmark model
#   Predicting above-average customer satisfaction (y_target)
#
# Dataset:
#   ML_Panel_final.csv
#
# Evaluation outputs (TEST set):
#   - Confusion Matrix
#   - Hit Rate (Accuracy)
#   - F1 Score
#   - AUC & Gini
#   - Top Decile Lift (TDL)
###############################################################

rm(list = ls())

# ------------------------------------------------------------
# 1. Set working directory
# ------------------------------------------------------------
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
cat("Working directory:", getwd(), "\n\n")

# ------------------------------------------------------------
# OUTPUT: save tables under results/tables (requested)
# ------------------------------------------------------------
RESULTS_BASE <- "C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data/results"
TABLE_LOGIT  <- file.path(RESULTS_BASE, "tables")
if (!dir.exists(TABLE_LOGIT)) dir.create(TABLE_LOGIT, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 2. Load libraries
# ------------------------------------------------------------
library(dplyr)
library(caret)
library(pROC)

# ------------------------------------------------------------
# 3. Load FINAL ML panel
# ------------------------------------------------------------
ML_Panel_final <- read.csv("ML_Panel_final.csv")

cat("Loaded ML_Panel_final with",
    nrow(ML_Panel_final), "rows and",
    ncol(ML_Panel_final), "columns.\n\n")

# ------------------------------------------------------------
# 4. Ensure correct data types
# ------------------------------------------------------------
ML_Panel_final$Weekend <- as.factor(ML_Panel_final$Weekend)
ML_Panel_final$covid_period <- as.factor(ML_Panel_final$covid_period)

# ------------------------------------------------------------
# 5. Remove remaining missings (LOGIT requirement)
# ------------------------------------------------------------
logit_data <- na.omit(ML_Panel_final)

cat("Usable observations after NA removal:",
    nrow(logit_data), "\n\n")

# ------------------------------------------------------------
# 6. Train / Test split
# ------------------------------------------------------------
set.seed(123)

n_total <- nrow(logit_data)
train_index <- sample(seq_len(n_total),
                      size = floor(0.7 * n_total))

train_data <- logit_data[train_index, ]
test_data  <- logit_data[-train_index, ]

cat("========== DATA SPLIT ==========\n")
cat("Total observations:", n_total, "\n")
cat("Train sample size:", nrow(train_data), "\n")
cat("Test sample size:", nrow(test_data), "\n")
cat("Train/Test split: 70/30\n")
cat("Random seed: 123\n\n")

# ------------------------------------------------------------
# 7. Estimate logistic regression (TRAIN data)
# ------------------------------------------------------------
logit_model <- glm(
  y_target ~ .,
  data = train_data,
  family = binomial(link = "logit")
)

cat("========== LOGISTIC REGRESSION SUMMARY ==========\n")
print(summary(logit_model))
cat("\n")

# ------------------------------------------------------------
# 8. Predict probabilities on TEST data
# ------------------------------------------------------------
test_data$pred_prob <- predict(
  logit_model,
  newdata = test_data,
  type = "response"
)

# Classification threshold
threshold <- 0.5
test_data$pred_class <- ifelse(test_data$pred_prob >= threshold, 1, 0)

# ------------------------------------------------------------
# 9. Confusion Matrix & Hit Rate
# ------------------------------------------------------------
conf_mat <- table(
  Predicted = test_data$pred_class,
  Actual   = test_data$y_target
)

cat("========== CONFUSION MATRIX ==========\n")
print(conf_mat)
cat("\n")

hit_rate <- mean(
  test_data$pred_class == test_data$y_target,
  na.rm = TRUE
)

cat("Hit Rate (Accuracy):", round(hit_rate, 4), "\n\n")

# ------------------------------------------------------------
# 10. F1 Score
# ------------------------------------------------------------
conf_mat_caret <- confusionMatrix(
  factor(test_data$pred_class, levels = c(0,1)),
  factor(test_data$y_target,   levels = c(0,1))
)

f1_score <- conf_mat_caret$byClass["F1"]
cat("F1 Score:", round(f1_score, 4), "\n\n")

# ------------------------------------------------------------
# 11. AUC & Gini (PRIMARY METRIC)
# ------------------------------------------------------------
roc_obj <- roc(
  response  = test_data$y_target,
  predictor = test_data$pred_prob
)

auc_value  <- as.numeric(auc(roc_obj))
gini_value <- 2 * auc_value - 1

cat("========== RANKING PERFORMANCE ==========\n")
cat("AUC:", round(auc_value, 4), "\n")
cat("Gini Coefficient:", round(gini_value, 4), "\n\n")

# ------------------------------------------------------------
# 12. Lift & Top Decile Lift (SECONDARY)
# ------------------------------------------------------------
test_data <- test_data %>%
  arrange(desc(pred_prob)) %>%
  mutate(rank = row_number())

decile_cut <- floor(0.1 * nrow(test_data))
top_decile <- test_data[1:decile_cut, ]

baseline_rate   <- mean(test_data$y_target)
top_decile_rate <- mean(top_decile$y_target)

TDL <- top_decile_rate / baseline_rate

cat("========== LIFT METRICS ==========\n")
cat("Baseline positive rate:", round(baseline_rate, 4), "\n")
cat("Top Decile positive rate:", round(top_decile_rate, 4), "\n")
cat("Top Decile Lift (TDL):", round(TDL, 4), "\n\n")

###############################################################
# END OF SCRIPT
###############################################################
# ------------------------------------------------------------
# Export Logistic Regression Results as Table
# ------------------------------------------------------------

library(dplyr)

# Extract coefficient table
logit_results <- summary(logit_model)$coefficients

# Convert to data frame
logit_table <- as.data.frame(logit_results)

# Add variable names as column
logit_table$Variable <- rownames(logit_table)

# Reorder columns (EXPLICIT dplyr::select)
logit_table <- logit_table %>%
  dplyr::select(
    Variable,
    Estimate,
    `Std. Error`,
    `z value`,
    `Pr(>|z|)`
  )

# Add Odds Ratios (recommended)
logit_table$Odds_Ratio <- exp(logit_table$Estimate)

# Round values for readability
logit_table <- logit_table %>%
  mutate(
    Estimate     = round(Estimate, 3),
    `Std. Error` = round(`Std. Error`, 3),
    `z value`    = round(`z value`, 3),
    `Pr(>|z|)`   = round(`Pr(>|z|)`, 4),
    Odds_Ratio   = round(Odds_Ratio, 3)
  )

# Export to CSV (UPDATED PATH)
write.csv(
  logit_table,
  file.path(TABLE_LOGIT, "Table_Logistic_Regression_Results.csv"),
  row.names = FALSE
)

cat("✔ Logistic regression results exported successfully.\n")
cat("Saved to:", file.path(TABLE_LOGIT, "Table_Logistic_Regression_Results.csv"), "\n")
