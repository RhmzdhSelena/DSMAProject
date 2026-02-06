###############################################################
# Script: 12_RF_SHAP_From_ML_Comparison.R
# Author: Selena-Leila Rahimzadeh (extended with SHAP)
#
# Purpose:
#   Stand-alone: reproduce your RF from 12_ML_Comparison.R
#   and compute SHAP values (direction + magnitude) on the TEST set.
#
# Outputs (same project structure):
#   - results/tables/rf_shap_importance_direction.csv
#   - results/tables/rf_shap_direction_only.csv
#   - figures/variable_importance/rf_shap_beeswarm_top20.png
#   - results/tables/model_rf_caret.rds  (so you can reload later)
#
# IMPORTANT:
#   Uses the same working directory and ML_Panel_final.csv as your ML script.
###############################################################

rm(list = ls())

# -------------------------------------------------------------
# 0) Setup (match your ML script)
# -------------------------------------------------------------
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
set.seed(123)

# Packages (minimal required for RF + SHAP + export)
pkgs <- c("caret", "pROC", "randomForest", "dplyr", "tidyr", "ggplot2",
          "scales", "readr", "forcats", "fastshap")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install, dependencies = TRUE)

library(caret)
library(pROC)
library(randomForest)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readr)
library(forcats)
library(fastshap)
library(doParallel)

n_cores <- max(1, parallel::detectCores() - 1)
cl <- parallel::makeCluster(n_cores)
doParallel::registerDoParallel(cl)
# -------------------------------------------------------------
# 0A) Output directories (exactly like your ML script)
# -------------------------------------------------------------
PROJECT_DIR <- getwd()

FIG_DIR     <- file.path(PROJECT_DIR, "figures")
RESULTS_DIR <- file.path(PROJECT_DIR, "results")
TABLE_DIR   <- file.path(RESULTS_DIR, "tables")

FIG_VI_DIR  <- file.path(FIG_DIR, "variable_importance")

for (d in c(FIG_DIR, RESULTS_DIR, TABLE_DIR, FIG_VI_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# -------------------------------------------------------------
# 1) Load ML-ready panel data (exact file name)
# -------------------------------------------------------------
if (!file.exists("ML_Panel_final.csv")) {
  stop("ML_Panel_final.csv not found in: ", getwd())
}
data <- read.csv("ML_Panel_final.csv")

# -------------------------------------------------------------
# 2) Train / Test Split (70 / 30) stratified (exactly your script)
# -------------------------------------------------------------
train_index <- createDataPartition(
  y = data$y_target,
  p = 0.7,
  list = FALSE
)

train_data <- data[train_index, ]
test_data  <- data[-train_index, ]

X_train <- train_data[, !(names(train_data) %in% "y_target")]
X_test  <- test_data[,  !(names(test_data)  %in% "y_target")]

y_train <- train_data$y_target
y_test  <- test_data$y_target

# -------------------------------------------------------------
# 3) caret outcome (Yes/No) (exactly your script)
# -------------------------------------------------------------
train_data$y_target_caret <- factor(
  ifelse(train_data$y_target == 1, "Yes", "No"),
  levels = c("No", "Yes")
)
test_data$y_target_caret <- factor(
  ifelse(test_data$y_target == 1, "Yes", "No"),
  levels = c("No", "Yes")
)

# ensure Weekend numeric if present (exactly your script)
if ("Weekend" %in% names(X_train)) {
  X_train$Weekend <- as.numeric(X_train$Weekend)
  X_test$Weekend  <- as.numeric(X_test$Weekend)
}

# -------------------------------------------------------------
# 4) trainControl (exactly your script)
# -------------------------------------------------------------
ctrl <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = FALSE
)

# -------------------------------------------------------------
# 5) Random Forest (exactly your script)
# -------------------------------------------------------------
set.seed(123)

rf_grid <- expand.grid(mtry = c(2, 4, 6, 8))

model_rf <- train(
  x = X_train,
  y = train_data$y_target_caret,
  method = "rf",
  metric = "ROC",
  tuneGrid = rf_grid,
  trControl = ctrl,
  ntree = 500,
  importance = TRUE
)

# Save model so you don't have to retrain later
saveRDS(model_rf, file.path(TABLE_DIR, "model_rf_caret.rds"))

# Predict probabilities on TEST (P(Yes))
rf_prob <- predict(model_rf, X_test, type = "prob")[, "Yes"]

cat("✅ RF trained. Best mtry:\n")
print(model_rf$bestTune)

# -------------------------------------------------------------
# 6) SHAP (fastshap) on TEST set
#    - Background sample from TRAIN set (stabilizes estimates)
# -------------------------------------------------------------

# Wrapper returns P(Yes)
pred_wrapper <- function(object, newdata) {
  predict(object, newdata = newdata, type = "prob")[, "Yes"]
}

set.seed(123)

# Background sample size (adjust if runtime/memory is an issue)
bg_n <- min(2000, nrow(X_train))
X_bg <- X_train[sample(seq_len(nrow(X_train)), bg_n), , drop = FALSE]

# Explain full test set (if too slow, uncomment subsample)
X_explain <- X_test
# explain_n <- min(8000, nrow(X_test))
# X_explain <- X_test[sample(seq_len(nrow(X_test)), explain_n), , drop = FALSE]

nsim <- 80  # increase to 120–200 if you want smoother SHAP (slower)

cat("⏳ Computing SHAP (this can take a bit)...\n")

shap_mat <- fastshap::explain(
  object       = model_rf,
  X            = X_bg,
  newdata      = X_explain,
  pred_wrapper = pred_wrapper,
  nsim         = nsim,
  adjust       = TRUE
)

parallel::stopCluster(cl)
doParallel::registerDoSEQ()

shap_df <- as.data.frame(shap_mat)
colnames(shap_df) <- colnames(X_explain)

cat("✅ SHAP computed: ", nrow(shap_df), " x ", ncol(shap_df), "\n", sep = "")

# -------------------------------------------------------------
# 7) SHAP tables: importance + average direction
# -------------------------------------------------------------
shap_summary <- tibble(
  variable      = colnames(shap_df),
  mean_abs_shap = sapply(shap_df, function(x) mean(abs(x), na.rm = TRUE)),
  mean_shap     = sapply(shap_df, function(x) mean(x, na.rm = TRUE))
) %>%
  arrange(desc(mean_abs_shap)) %>%
  mutate(direction = ifelse(mean_shap >= 0, "↑ increases P(Yes)", "↓ decreases P(Yes)"))

write_csv(shap_summary, file.path(TABLE_DIR, "rf_shap_importance_direction.csv"))

shap_direction_only <- shap_summary %>%
  transmute(variable, mean_shap, direction)

write_csv(shap_direction_only, file.path(TABLE_DIR, "rf_shap_direction_only.csv"))

cat("✅ SHAP tables saved to: ", TABLE_DIR, "\n", sep = "")

# -------------------------------------------------------------
# 8) SHAP beeswarm plot (top 20)
# -------------------------------------------------------------
plot_df <- shap_df %>%
  mutate(row_id = row_number()) %>%
  pivot_longer(cols = -row_id, names_to = "variable", values_to = "shap_value") %>%
  left_join(
    X_explain %>%
      mutate(row_id = row_number()) %>%
      pivot_longer(cols = -row_id, names_to = "variable", values_to = "feature_value"),
    by = c("row_id", "variable")
  )

top_k <- 20
top_vars <- shap_summary$variable[1:min(top_k, nrow(shap_summary))]

plot_df_top <- plot_df %>%
  filter(variable %in% top_vars) %>%
  mutate(variable = forcats::fct_reorder(variable, abs(shap_value), .fun = median, .desc = TRUE))

p_shap <- ggplot(plot_df_top, aes(x = shap_value, y = variable, color = feature_value)) +
  geom_point(alpha = 0.6, size = 1.6,
             position = position_jitter(height = 0.18, width = 0)) +
  geom_vline(xintercept = 0, linewidth = 0.4) +
  labs(
    title = "SHAP Summary (Random Forest)",
    subtitle = "Positive SHAP increases predicted P(above-average satisfaction = Yes)",
    x = "SHAP value (impact on model output)",
    y = NULL,
    color = "Feature value"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"))

ggsave(
  filename = file.path(FIG_VI_DIR, "rf_shap_beeswarm_top20.png"),
  plot = p_shap,
  width = 10, height = 6, dpi = 300, bg = "white"
)

cat("✅ SHAP plot saved to: ", file.path(FIG_VI_DIR, "rf_shap_beeswarm_top20.png"), "\n", sep = "")

cat("\nDONE.\n",
    "Outputs:\n",
    " - ", file.path(TABLE_DIR, "model_rf_caret.rds"), "\n",
    " - ", file.path(TABLE_DIR, "rf_shap_importance_direction.csv"), "\n",
    " - ", file.path(TABLE_DIR, "rf_shap_direction_only.csv"), "\n",
    " - ", file.path(FIG_VI_DIR, "rf_shap_beeswarm_top20.png"), "\n\n",
    sep = "")
