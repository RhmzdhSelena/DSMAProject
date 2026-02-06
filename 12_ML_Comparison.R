###############################################################
# Script: 12_ML_Comparison.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Estimate and compare supervised machine learning models
#   for predicting above-average daily customer satisfaction.
#
#   All models are trained on a common 70% training sample and
#   evaluated on an identical 30% held-out test set.
#
#   Evaluated models:
#     - Decision Tree (cp grid; 3-fold CV)
#     - Bagging (nbagg small grid; 3-fold CV)
#     - Random Forest (mtry small grid; 3-fold CV; ntree fixed)
#     - Boosting (GBM small grid; 3-fold CV)
#     - SVM Radial (C/sigma small grid; 3-fold CV; subsampled train)
#     - kNN (k grid; 3-fold CV; centered/scaled)
#     - Naive Bayes (Top-12 filter feature selection; default NB)
#     - Neural Network (size/decay small grid; 3-fold CV; centered/scaled)
#
# Output:
#   - ML_Model_Performance_Table.csv
#   - Figures
#   - Results tables 
###############################################################

## -------------------------------------------------------------
## 0. Setup
## -------------------------------------------------------------

setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")
set.seed(123)

library(caret)
library(pROC)
library(rpart)
library(ipred)
library(randomForest)
library(gbm)
library(e1071)
library(class)
library(nnet)
library(kernlab)

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)


## -------------------------------------------------------------
## 0A. Output directories 
## -------------------------------------------------------------
# For a clean project structure, all figures and result tables are written
# to dedicated folders inside the current working directory (getwd()).
# This makes the script reproducible and keeps the project root tidy.

PROJECT_DIR <- getwd()

# Main output folders
FIG_DIR     <- file.path(PROJECT_DIR, "figures")
RESULTS_DIR <- file.path(PROJECT_DIR, "results")
TABLE_DIR   <- file.path(RESULTS_DIR, "tables")

# Optional figure subfolders (keeps appendix outputs organized)
FIG_ROC_DIR   <- file.path(FIG_DIR, "roc")
FIG_BENCH_DIR <- file.path(FIG_DIR, "benchmark")
FIG_LIFT_DIR  <- file.path(FIG_DIR, "lift")
FIG_CM_DIR    <- file.path(FIG_DIR, "confusion_matrices")
FIG_VI_DIR    <- file.path(FIG_DIR, "variable_importance")

# Create folders if they do not yet exist (recursive ensures nested folders work)
for (d in c(FIG_DIR, RESULTS_DIR, TABLE_DIR, FIG_ROC_DIR, FIG_BENCH_DIR, FIG_LIFT_DIR, FIG_CM_DIR, FIG_VI_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}


## -------------------------------------------------------------
## 1. Load ML-ready panel data
## -------------------------------------------------------------
data <- read.csv("ML_Panel_final.csv")

## -------------------------------------------------------------
## 2. Train / Test Split (70 / 30)
# We create a stratified split to preserve the class balance (share of y_target==1)
# in both training and test data. All model fitting uses ONLY the training set; the
# test set is held out for the final out-of-sample evaluation.
## -------------------------------------------------------------
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

## -------------------------------------------------------------
## 3. Factor version of target
## -------------------------------------------------------------
train_data$y_target_factor <- factor(train_data$y_target, levels = c(0, 1))
test_data$y_target_factor  <- factor(test_data$y_target,  levels = c(0, 1))

## -------------------------------------------------------------
## 4. Unified Evaluation Function
## -------------------------------------------------------------
# Helper: evaluate probabilistic predictions against the true binary outcomes.
# Inputs:
#   - y_true: numeric vector (0/1) of true outcomes
#   - y_prob: numeric vector (0..1) of predicted probabilities for class 1 ("Yes")
#   - threshold: classification cutoff for confusion-matrix metrics (default 0.5)
# Output:
#   A one-row data.frame with standard classification metrics used in the paper.
evaluate_model <- function(y_true, y_prob, threshold = 0.5) {
  y_pred <- ifelse(y_prob >= threshold, 1, 0)
  cm <- confusionMatrix(
    factor(y_pred, levels = c(0, 1)),
    factor(y_true, levels = c(0, 1))
  )
  roc_obj <- roc(y_true, y_prob)
  auc <- as.numeric(auc(roc_obj))
  gini <- 2 * auc - 1
  decile_cutoff <- quantile(y_prob, 0.9)
  top_decile_rate <- mean(y_true[y_prob >= decile_cutoff])
  overall_rate <- mean(y_true)
  tdl <- top_decile_rate / overall_rate
  
  data.frame(
    Accuracy = cm$overall["Accuracy"],
    Balanced_Accuracy = cm$byClass["Balanced Accuracy"],
    Precision = cm$byClass["Precision"],
    Recall = cm$byClass["Recall"],
    F1 = cm$byClass["F1"],
    AUC = auc,
    Gini = gini,
    Top_Decile_Lift = tdl
  )
}

## -------------------------------------------------------------
## 5. Model Estimation and Evaluation
## -------------------------------------------------------------
# 5.0 Logistic Regression (Benchmark)
# Fit on TRAIN only, predict probabilities on TEST only (no leakage)

# Build modeling frames (ensure same columns, drop caret outcome from predictors)
logit_train <- data.frame(X_train)
logit_train$y_target <- y_train

logit_test <- data.frame(X_test)

# Fit logit (binomial) like Script 11
model_logit <- glm(
  y_target ~ .,
  data = logit_train,
  family = binomial(link = "logit")
)

# Predict test probabilities for class 1
logit_prob <- predict(model_logit, newdata = logit_test, type = "response")

# Evaluate
logit_perf <- evaluate_model(y_test, logit_prob) 

# 5.1 Decision Tree (cp grid; 3-fold CV)  [NO LEAKAGE]
# Create caret-style outcome once (Yes/No)
train_data$y_target_caret <- factor(ifelse(train_data$y_target == 1, "Yes", "No"),
                                    levels = c("No", "Yes"))
test_data$y_target_caret  <- factor(ifelse(test_data$y_target == 1, "Yes", "No"),
                                    levels = c("No", "Yes"))

# ensure Weekend is numeric in X matrices if present
if ("Weekend" %in% names(X_train)) {
  X_train$Weekend <- as.numeric(X_train$Weekend)
  X_test$Weekend  <- as.numeric(X_test$Weekend)
}

# Common cross-validation control:
# - 3-fold CV (kept small for runtime)
# - ROC/AUC as the tuning metric (probability-based, threshold-free)
# - classProbs=TRUE is required for ROC computation in caret.
ctrl <- trainControl(
  method = "cv",
  number = 3,
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  verboseIter = FALSE
)

tree_grid <- expand.grid(cp = c(0.001, 0.005, 0.01, 0.02))
model_tree <- train(
  x = X_train,
  y = train_data$y_target_caret,
  method = "rpart",
  metric = "ROC",
  tuneGrid = tree_grid,
  trControl = ctrl
)
tree_prob <- predict(model_tree, X_test, type = "prob")[, "Yes"]
tree_perf <- evaluate_model(y_test, tree_prob)

# 5.2 Bagging (nbagg small grid; 3-fold CV)  
# Manual CV over nbagg (keeps ipred::bagging, but tunes nbagg)
set.seed(123)
folds <- createFolds(train_data$y_target_caret, k = 3, returnTrain = FALSE)
nbagg_grid <- c(25, 50, 75)

# Build a leakage-safe training frame for bagging:
bag_train_full <- data.frame(X_train)
bag_train_full$y_target_caret <- train_data$y_target_caret

bag_cv_auc <- sapply(nbagg_grid, function(B) {
  fold_auc <- sapply(folds, function(idx_val) {
    
    bag_tr  <- bag_train_full[-idx_val, , drop = FALSE]
    bag_val <- bag_train_full[idx_val,  , drop = FALSE]
    
    m <- ipred::bagging(y_target_caret ~ ., data = bag_tr, nbagg = B)
    
    # predict probabilities for "Yes"
    p <- predict(m, newdata = bag_val, type = "prob")[, "Yes"]
    
    y_val_num <- y_train[idx_val]  # numeric 0/1 for AUC computation
    as.numeric(pROC::auc(pROC::roc(y_val_num, p)))
  })
  mean(fold_auc, na.rm = TRUE)
})

best_nbagg <- nbagg_grid[which.max(bag_cv_auc)]

# Fit final bagging model on full training data
model_bag <- ipred::bagging(y_target_caret ~ ., data = bag_train_full, nbagg = best_nbagg)

# Predict on leakage-safe test frame
bag_test_full <- data.frame(X_test)
bag_test_full$y_target_caret <- test_data$y_target_caret  # not used by predict, but harmless/consistent

bag_prob <- predict(model_bag, newdata = bag_test_full, type = "prob")[, "Yes"]
bag_perf <- evaluate_model(y_test, bag_prob)


# -------------------------------------------------------------
# 5.3 Random Forest (mtry small grid; 3-fold CV; ntree fixed) 
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

rf_prob <- predict(model_rf, X_test, type = "prob")[, "Yes"]
rf_perf <- evaluate_model(y_test, rf_prob)

# 5.4 Boosting (GBM small grid; 3-fold CV)  
gbm_grid <- expand.grid(
  n.trees = c(150, 300),
  interaction.depth = c(2, 3, 4),
  shrinkage = c(0.03, 0.05),
  n.minobsinnode = c(10, 20)
)

model_gbm <- train(
  x = X_train,
  y = train_data$y_target_caret,
  method = "gbm",
  metric = "ROC",
  tuneGrid = gbm_grid,
  trControl = ctrl,
  verbose = FALSE
)

gbm_prob <- predict(model_gbm, X_test, type = "prob")[, "Yes"]
gbm_perf <- evaluate_model(y_test, gbm_prob)

# -------------------------------------------------------------
# 5.5 SVM (RBF Kernel) — C/sigma small grid; 3-fold CV; subsampled train
# -------------------------------------------------------------

# Scaling (unchanged)
X_train_scaled <- scale(X_train)
X_test_scaled <- scale(
  X_test,
  center = attr(X_train_scaled, "scaled:center"),
  scale  = attr(X_train_scaled, "scaled:scale")
)

# stratified 30% subsample for SVM only
set.seed(123)
svm_idx <- createDataPartition(
  train_data$y_target,
  p = 0.3,
  list = FALSE
)

X_train_svm <- X_train_scaled[svm_idx, ]
y_train_svm <- train_data$y_target_caret[svm_idx]

# Reduced grid (as requested)
svm_grid <- expand.grid(
  sigma = c(0.005, 0.01),
  C = c(0.5, 1)
)

cat("Starting SVM (RBF) training on subsample...\n")
start_time <- Sys.time()

model_svm_rbf <- train(
  x = X_train_svm,
  y = y_train_svm,
  method = "svmRadial",
  metric = "ROC",
  tuneGrid = svm_grid,
  trControl = ctrl
)

end_time <- Sys.time()
cat("SVM training done.\n")
cat("Time elapsed:",
    round(difftime(end_time, start_time, units = "mins"), 2),
    "minutes\n"
)

# Predict on FULL test set
svm_rbf_prob <- predict(
  model_svm_rbf,
  newdata = X_test_scaled,
  type = "prob"
)[, "Yes"]

svm_perf <- evaluate_model(y_test, svm_rbf_prob)

# 5.6 kNN (k grid; 3-fold CV; centered/scaled)
knn_grid <- expand.grid(k = c(5, 10, 15, 20, 25))

model_knn <- train(
  x = X_train,
  y = train_data$y_target_caret,
  method = "knn",
  metric = "ROC",
  tuneGrid = knn_grid,
  trControl = ctrl,
  preProcess = c("center", "scale")
)

knn_prob <- predict(model_knn, X_test, type = "prob")[, "Yes"]
knn_perf <- evaluate_model(y_test, knn_prob)

# 5.7 Naive Bayes (Top-12 filter feature selection; default NB)
# Filter: caret univariate filter importance (Top 12)
var_imp <- caret::filterVarImp(X_train, train_data$y_target_caret)
# pick the column that exists (usually "Yes")
imp_col <- intersect(colnames(var_imp), c("Yes", "yes", "1", "X1"))
if (length(imp_col) == 0) imp_col <- colnames(var_imp)[1]
top_vars <- rownames(var_imp)[order(var_imp[[imp_col[1]]], decreasing = TRUE)][1:min(12, nrow(var_imp))]

X_train_nb <- X_train[, top_vars, drop = FALSE]
X_test_nb  <- X_test[,  top_vars, drop = FALSE]

model_nb <- naiveBayes(x = X_train_nb, y = train_data$y_target_caret)
nb_prob <- predict(model_nb, X_test_nb, type = "raw")[, "Yes"]
nb_perf <- evaluate_model(y_test, nb_prob)

# 5.8 Neural Network (size/decay small grid; 3-fold CV; centered/scaled)
nn_grid <- expand.grid(
  size = c(3, 5, 7),
  decay = c(0, 0.001, 0.01)
)

model_nn <- train(
  x = X_train,
  y = train_data$y_target_caret,
  method = "nnet",
  metric = "ROC",
  tuneGrid = nn_grid,
  trControl = ctrl,
  preProcess = c("center", "scale"),
  trace = FALSE,
  maxit = 300
)

nn_prob <- predict(model_nn, X_test, type = "prob")[, "Yes"]
nn_perf <- evaluate_model(y_test, nn_prob)

## -------------------------------------------------------------
## 6. Performance Comparison Table
## -------------------------------------------------------------
results <- rbind(
  Logit = logit_perf,
  Decision_Tree = tree_perf,
  Bagging = bag_perf,
  Random_Forest = rf_perf,
  Boosting = gbm_perf,
  SVM = svm_perf,
  kNN = knn_perf,
  Naive_Bayes = nb_perf,
  Neural_Network = nn_perf
)

results <- round(results, 3)
print(results)
write.csv(results, file.path(TABLE_DIR, "ML_Model_Performance_Table.csv"))

## =============================================================
## APPENDIX FIGURES
## =============================================================
## -------------------------------------------------------------
## Pretty variable names (for Appendix figure only)
## -------------------------------------------------------------
vip_labels <- c(
  # Engagement dynamics
  log_review_count    = "Log daily number of reviews",
  daily_review_count  = "Daily number of reviews",
  lag_reviews_1d      = "Lagged daily reviews (t−1)",
  cum_reviews         = "Cumulative reviews",
  daily_checkins      = "Daily check-ins",
  roll_checkins_7d    = "7-day rolling avg. check-ins",
  cum_checkins        = "Cumulative check-ins",
  
  # Restaurant attributes
  wifi_binary         = "WiFi available",
  alcohol_binary      = "Alcohol served",
  noise_high          = "High noise level",
  outdoor_seating     = "Outdoor seating",
  delivery            = "Delivery available",
  reservations        = "Reservations accepted",
  table_service       = "Table service",
  good_for_groups     = "Good for groups",
  price_high          = "High price level",
  
  # Categories
  cat_fast_food       = "Fast food category",
  cat_dessert         = "Dessert category",
  cat_dietary_specialty = "Dietary specialty category",
  
  # Temporal / weather controls
  Weekend             = "Weekend",
  covid_period        = "COVID-19 period",
  is_raining          = "Rainy day",
  TAVG_sq             = "Average temperature (squared)",
  TMAX_lag1           = "Lagged maximum temperature"
)
## -------------------------------------------------------------
## A1. ROC Curves — ALL models in one graphic 
## -------------------------------------------------------------
roc_list <- list(
  "Bagging"             = roc(y_test, bag_prob,  quiet = TRUE),
  "Decision Tree"       = roc(y_test, tree_prob, quiet = TRUE),
  "GBM"                 = roc(y_test, gbm_prob,  quiet = TRUE),
  "kNN"                 = roc(y_test, knn_prob,  quiet = TRUE),
  "Logit"               = roc(y_test, logit_prob,quiet = TRUE),
  "Naive Bayes"         = roc(y_test, nb_prob,   quiet = TRUE),
  "Neural Net"          = roc(y_test, nn_prob,   quiet = TRUE),
  "Random Forest"       = roc(y_test, rf_prob,   quiet = TRUE),
  "SVM (Radial, subset)"= roc(y_test, svm_rbf_prob,  quiet = TRUE)
)

p_roc_all <- ggroc(roc_list, linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 1, linetype = "dashed", color = "grey40") +
  labs(
    title = "ROC Curves — Model Comparison",
    x = "False Positive Rate",
    y = "True Positive Rate",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 18),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

ggsave(file.path(FIG_ROC_DIR, "App_Fig_ROC_Comparison_AllModels.png"),
       p_roc_all, width = 12, height = 8, dpi = 300, bg = "white")

## -------------------------------------------------------------
## A2. Gini + TDL Barplot 
## -------------------------------------------------------------
uni_blue <- "#00618F"
uni_grey <- "#CBD2DB"

perf_plot <- as.data.frame(results)
perf_plot$Algorithm <- rownames(perf_plot)

# Nice labels
perf_plot <- perf_plot %>%
  mutate(
    Algorithm = recode(Algorithm,
                       "Decision_Tree"   = "Decision Tree",
                       "Random_Forest"   = "Random Forest",
                       "Neural_Network"  = "Neural Net",
                       "Naive_Bayes"     = "Naive Bayes"
    )
  )

# Order (you can change if you want)
algo_order <- c(
  "Logit",
  "Naive Bayes",
  "kNN",
  "Decision Tree",
  "Bagging",
  "Random Forest",
  "Boosting",
  "SVM",
  "Neural Net"
)
perf_plot$Algorithm <- factor(perf_plot$Algorithm, levels = algo_order)

plot_long <- perf_plot %>%
  select(Algorithm, Gini, Top_Decile_Lift) %>%
  pivot_longer(cols = c(Gini, Top_Decile_Lift),
               names_to = "Metric", values_to = "Score") %>%
  mutate(Metric = recode(Metric,
                         "Gini" = "Gini",
                         "Top_Decile_Lift" = "TDL"))

p_gini_tdl <- ggplot(plot_long, aes(x = Algorithm, y = Score, fill = Metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.7, color = "white") +
  geom_text(aes(label = round(Score, 2)),
            position = position_dodge(width = 0.75),
            vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("Gini" = uni_grey, "TDL" = uni_blue), name = "Metric") +
  labs(
    title = "9-Model Benchmark (Gini & TDL)",
    subtitle = "Comparative Analysis of Predictive Power",
    x = "Algorithm",
    y = "Score"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  ) +
  coord_cartesian(clip = "off")

ggsave(file.path(FIG_BENCH_DIR, "App_Fig_Benchmark_Gini_TDL.png"),
       p_gini_tdl, width = 13, height = 7, dpi = 300, bg = "white")

## -------------------------------------------------------------
## A3. Lift Curves — ALL models 
## -------------------------------------------------------------
make_lift_df <- function(y_true, y_prob, model_name) {
  # Robust 0/1 conversion for y
  if (is.factor(y_true)) {
    lv <- levels(y_true)
    if (all(c("No", "Yes") %in% lv)) {
      y <- as.integer(y_true == "Yes")
    } else {
      y <- as.integer(as.character(y_true))
    }
  } else if (is.character(y_true)) {
    y <- as.integer(y_true %in% c("1", "Yes", "YES", "yes", "TRUE", "True", "true"))
  } else {
    y <- as.integer(y_true)
  }
  
  p <- as.numeric(y_prob)
  
  df <- data.frame(y = y, p = p) %>%
    dplyr::filter(is.finite(y), is.finite(p)) %>%
    dplyr::arrange(dplyr::desc(p))
  
  if (nrow(df) == 0) stop(paste0("Lift data empty for: ", model_name))
  total_pos <- sum(df$y == 1, na.rm = TRUE)
  if (total_pos == 0) stop(paste0("No positives in y for: ", model_name))
  
  df$frac_obs <- seq_len(nrow(df)) / nrow(df)
  df$cum_pos  <- cumsum(df$y == 1)
  df$cum_gain <- df$cum_pos / total_pos
  df$model    <- model_name
  df
}

lift_df <- bind_rows(
  make_lift_df(y_test, logit_prob, "Logit"),
  make_lift_df(y_test, tree_prob,  "Decision Tree"),
  make_lift_df(y_test, bag_prob,   "Bagging"),
  make_lift_df(y_test, rf_prob,    "Random Forest"),
  make_lift_df(y_test, gbm_prob,   "GBM"),
  make_lift_df(y_test, svm_rbf_prob,   "SVM (Radial, subset)"),
  make_lift_df(y_test, knn_prob,   "kNN"),
  make_lift_df(y_test, nb_prob,    "Naive Bayes"),
  make_lift_df(y_test, nn_prob,    "Neural Net")
)

baseline_df <- data.frame(
  frac_obs = seq(0, 1, length.out = 250),
  cum_gain = seq(0, 1, length.out = 250),
  model = "Random"
)

p_lift_all <- ggplot() +
  geom_line(data = lift_df, aes(x = frac_obs, y = cum_gain, color = model), linewidth = 1.05) +
  geom_line(data = baseline_df, aes(x = frac_obs, y = cum_gain),
            linewidth = 1.0, linetype = "dashed", color = "grey45") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Lift Curves — Concentration of High-Satisfaction Days",
    x = "Cumulative Share of Observations",
    y = "Cumulative Share of Positives",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

ggsave(file.path(FIG_LIFT_DIR, "App_Fig_Lift_Curves_AllModels.png"),
       p_lift_all, width = 12, height = 8, dpi = 300, bg = "white")

## -------------------------------------------------------------
## A4. Confusion Matrices 
## -------------------------------------------------------------
create_cm_oldstyle_newcolors <- function(y_true, y_prob, title_label, threshold = 0.5) {
  
  y_true_f <- factor(y_true, levels = c(0, 1))
  y_pred_f <- factor(ifelse(y_prob >= threshold, 1, 0), levels = c(0, 1))
  
  TN <- sum(y_pred_f == 0 & y_true_f == 0, na.rm = TRUE)
  FP <- sum(y_pred_f == 1 & y_true_f == 0, na.rm = TRUE)
  FN <- sum(y_pred_f == 0 & y_true_f == 1, na.rm = TRUE)
  TP <- sum(y_pred_f == 1 & y_true_f == 1, na.rm = TRUE)
  
  df_cm <- data.frame(
    Actual = factor(c(0, 1, 0, 1), levels = c(0, 1)),
    Predicted = factor(c(0, 0, 1, 1), levels = c(0, 1)),
    Cell = c("TN", "FN", "FP", "TP"),
    Value = c(TN, FN, FP, TP)
  )
  
  fill_map <- c(
    "TN" = "#DCE3EA",
    "FP" = "#CBD2DB",
    "FN" = "#B7C4D1",
    "TP" = "#00618F"
  )
  
  ggplot(df_cm, aes(x = Actual, y = Predicted, fill = Cell)) +
    geom_tile(color = "white", linewidth = 1.5) +
    scale_fill_manual(values = fill_map) +
    annotate("text", x = c(1,2,1,2), y = c(1,1,2,2),
             label = c("TN", "FN", "FP", "TP"),
             size = 5.5, fontface = "bold", vjust = -0.6) +
    annotate("text", x = c(1,2,1,2), y = c(1,1,2,2),
             label = format(c(TN, FN, FP, TP), big.mark=","),
             size = 6.5, fontface = "bold", vjust = 1.25) +
    labs(
      title = title_label,
      x = "Actual Satisfaction",
      y = "Predicted Satisfaction"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "none",
      panel.grid = element_blank(),
      plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
      axis.text = element_text(face = "bold")
    )
}

p_cm_rf  <- create_cm_oldstyle_newcolors(y_test, rf_prob,  "CM: Random Forest (Balanced Ranking)")
p_cm_svm <- create_cm_oldstyle_newcolors(y_test, svm_rbf_prob, "CM: RBF SVM (Subsampled)")

ggsave(file.path(FIG_CM_DIR, "App_Fig_CM_RF_Section442_Final.png"),
       p_cm_rf, width = 6, height = 5.5, dpi = 300, bg = "white")
ggsave(file.path(FIG_CM_DIR, "App_Fig_CM_SVM_Section442_Final.png"),
       p_cm_svm, width = 6, height = 5.5, dpi = 300, bg = "white")

## -------------------------------------------------------------
## A5. Variable Importance (Random Forest) 
## -------------------------------------------------------------
vip <- varImp(model_rf, scale = FALSE)$importance
vip$Variable <- rownames(vip)

# safety: remove any target-looking fields if they appear
vip <- vip %>% filter(!Variable %in% c("y_target","y_target_caret","y_target_factor"))

imp_col <- if ("Overall" %in% names(vip)) "Overall" else setdiff(names(vip), "Variable")[1]

vip_data <- vip %>%
  arrange(desc(.data[[imp_col]])) %>%
  head(30) %>%
  mutate(
    Pretty_Variable = ifelse(
      Variable %in% names(vip_labels),
      vip_labels[Variable],
      Variable
    )
  )

p_vip <- ggplot(vip_data, aes(x = reorder(Pretty_Variable, .data[[imp_col]]), y = .data[[imp_col]])) +
  geom_bar(stat = "identity", aes(fill = .data[[imp_col]]), color = "white") +
  coord_flip() +
  scale_fill_gradient(low = "#CBD2DB", high = "#00618F") +
  labs(
    title = "Variable Importance Ranking (Random Forest)",
    x = "Predictor Variable",
    y = "Relative variable importance"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", size = 16, margin = ggplot2::margin(b = 10)),
    plot.subtitle = element_text(size = 11, color = "grey40", margin = ggplot2::margin(b = 15)),
    axis.title = element_text(face = "bold", color = "grey20"),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.margin = ggplot2::margin(20, 30, 20, 60)
  )

ggsave(file.path(FIG_VI_DIR, "App_Fig_Variable_Importance_RF.png"),
       plot = p_vip, width = 9, height = 7, dpi = 300, bg = "white")

cat("Done. Figures saved under: ", FIG_DIR, "\n", sep = "")
cat("Done. Results table saved under: ", TABLE_DIR, "\n", sep = "")


