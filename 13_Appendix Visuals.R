###############################################################
# Script: 11_Appendix_Visuals_FULL.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Generate the complete set of descriptive appendix figures used to
#   document data characteristics, validation checks, and textual patterns
#   underlying the empirical analyses.
#
#   Includes:
#     - Engagement distribution (Skewness)
#     - Inactivity structure (Threshold)
#     - Feature correlations (Full & Top 15 Predictors)
#     - Sentiment dispersion by satisfaction tier
#     - Unigram and Bigram word clouds
###############################################################

rm(list = ls())

# ==============================
# 0. Paths and Output Handling
# ==============================
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")

FIG_BASE <- "C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data/figures"

# Meaningful root folder for THIS appendix script
out_dir <- file.path(FIG_BASE, "appendix_visuals_full")

# Optional subfolders (keeps things tidy)
OUT_DESCRIPTIVES <- file.path(out_dir, "descriptives")
OUT_CORR         <- file.path(out_dir, "correlations")
OUT_SENTIMENT    <- file.path(out_dir, "sentiment")
OUT_CLOUDS       <- file.path(out_dir, "wordclouds")

for (d in c(out_dir, OUT_DESCRIPTIVES, OUT_CORR, OUT_SENTIMENT, OUT_CLOUDS)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)}
# ==============================
# 1. Libraries
# ==============================
library(dplyr)
library(readr)
library(ggplot2)
library(reshape2)
library(grid)
library(stringdist)
library(scales)
library(stringr)
library(tidytext)
library(tidyr)
library(wordcloud)
library(rlang)

# ==============================
# 2. Data Loading & Prep
# ==============================
raw_panel <- read_csv("internal_panel_final.csv", show_col_types = FALSE)
ml_panel  <- read_csv("ML_Panel_final.csv", show_col_types = FALSE)
reviews_clean <- read_csv("reviews_tucson_clean.csv", show_col_types = FALSE)
reviews_enriched <- read_csv("reviews_tucson_enriched_descriptive.csv", show_col_types = FALSE)

# Date Normalization
if ("date" %in% names(raw_panel)) raw_panel <- raw_panel %>% mutate(date = as.Date(date))
reviews_clean    <- reviews_clean %>% mutate(date = as.Date(date))
reviews_enriched <- reviews_enriched %>% mutate(date = as.Date(date))

# ------------------------------------------------------------------------------
# SAFETY CHECK: Ensure Derived Variables Exist
# ------------------------------------------------------------------------------
if (!"TAVG_sq" %in% names(ml_panel) && "TAVG" %in% names(ml_panel)) {
  ml_panel$TAVG_sq <- ml_panel$TAVG^2
}
if (!"log_review_count" %in% names(ml_panel) && "daily_review_count" %in% names(ml_panel)) {
  ml_panel$log_review_count <- log(ml_panel$daily_review_count + 1)
}
if (!"is_raining" %in% names(ml_panel) && "PRCP" %in% names(ml_panel)) {
  ml_panel$is_raining <- ifelse(ml_panel$PRCP > 0, 1, 0)
}

# =============================================================
# FIG 1: ENGAGEMENT SKEWNESS
# =============================================================
p1 <- ggplot(raw_panel, aes(x = daily_review_count)) +
  geom_histogram(bins = 50, fill = "#ffc0cb", color = "#CC79A7") +
  scale_y_log10(labels = label_comma()) +
  labs(
    title = "Figure 1: Engagement Skewness",
    subtitle = "Daily review counts (Log Scale)",
    x = "Daily Reviews",
    y = "Frequency (Log)"
  ) +
  theme_minimal()

ggsave(file.path(OUT_DESCRIPTIVES, "Fig_1_Skewness.png"), p1, width = 8, height = 5)

# =============================================================
# FIG 2: INACTIVITY THRESHOLD
# =============================================================
zero_runs <- raw_panel %>%
  group_by(business_id) %>%
  summarise(max_zero_run = max(rle(daily_checkins == 0)$lengths), .groups = "drop")

p2 <- ggplot(zero_runs, aes(x = max_zero_run)) +
  geom_histogram(bins = 60, fill = "#CC79A7", color = "white") +
  geom_vline(xintercept = 5140, linetype = "dashed", color = "#0984e3", size = 1) +
  labs(
    title = "Figure 2: Inactivity Threshold",
    subtitle = "Hybrid Rule Cutoff (Blue Line)",
    x = "Max Consecutive Inactive Days",
    y = "Count"
  ) +
  theme_minimal()

ggsave(file.path(OUT_DESCRIPTIVES, "Fig_2_Inactivity.png"), p2, width = 8, height = 5)

# =============================================================
# CORRELATION ANALYSIS
# =============================================================

# ------------------------------------------------------------------------------
# PART 1: FULL CORRELATION MATRIX (Specific 25 Variables)
# ------------------------------------------------------------------------------
full_vars_list <- c(
  "y_target", "log_review_count", "daily_review_count",
  "daily_checkins", "lag_reviews_1d", "roll_checkins_7d",
  "cum_reviews", "cum_checkins", "alcohol_binary",
  "wifi_binary", "noise_high", "price_high",
  "outdoor_seating", "table_service", "reservations",
  "delivery", "good_for_groups", "cat_fast_food",
  "cat_dessert", "cat_dietary_specialty", "TAVG_sq",
  "TMAX_lag1", "is_raining", "covid_period",
  "Weekend"
)

# Select these variables
full_data <- ml_panel %>%
  dplyr::select(all_of(full_vars_list)) %>%
  mutate(across(everything(), as.numeric))

cormat_full <- round(cor(full_data, use = "complete.obs"), 2)
melted_full <- melt(cormat_full)

p_full <- ggplot(data = melted_full, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "#FF0000", mid = "white", high = "#0072B2", 
    midpoint = 0, limit = c(-1,1), space = "Lab", 
    name="Correlation"
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, size = 8, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(face="bold", hjust=0.5)
  ) +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2) +
  labs(title = "Full Correlation Matrix", subtitle = " ")

ggsave(file.path(OUT_CORR, "App_Fig_Corr_Full_ML_Panel.png"), p_full, width = 14, height = 12, bg="white")

# ------------------------------------------------------------------------------
# PART 2: MAIN PREDICTORS (Top 15 Triangle)
# ------------------------------------------------------------------------------
# Updated List: Top 15 Predictors from Variable Importance
rf_vars_top15 <- c(
  "cum_checkins",       # 1
  "cum_reviews",        # 2
  "TAVG_sq",            # 3
  "TMAX_lag1",          # 4
  "log_review_count",   # 5
  "roll_checkins_7d",   # 6
  "lag_reviews_1d",     # 7
  "Weekend",            # 8
  "wifi_binary",        # 9
  "is_raining",         # 10
  "delivery",           # 11
  "reservations",       # 12
  "outdoor_seating",    # 13
  "table_service",      # 14
  "daily_review_count"  # 15
)

# Prepare Data
focus_data <- ml_panel %>%
  dplyr::select(all_of(rf_vars_top15)) %>%
  mutate(across(everything(), as.numeric))

cormat <- round(cor(focus_data, use = "complete.obs"), 2)
cor_long <- reshape2::melt(cormat, varnames = c("Var1", "Var2"), value.name = "Correlation")

cor_long$Var1 <- factor(cor_long$Var1, levels = rf_vars_top15)
cor_long$Var2 <- factor(cor_long$Var2, levels = rf_vars_top15)
cor_long <- cor_long %>% dplyr::filter(as.integer(Var1) <= as.integer(Var2))

# Diagonal Labels Logic
n <- length(rf_vars_top15)
diag_labels <- data.frame(
  x = seq_len(n) - 0.55,
  y = seq_len(n),
  label = rf_vars_top15,
  stringsAsFactors = FALSE
)
diag_labels$y_rev <- (n - diag_labels$y + 1)

# Plot Triangle
p_corr_triangle <- ggplot(cor_long, aes(x = Var2, y = Var1, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Correlation)), size = 2.8, color = "black") + 
  scale_fill_gradient2(
    low = "#FF0000", mid = "white", high = "#0072B2", 
    midpoint = 0, limits = c(-1, 1), name = NULL
  ) +
  scale_x_discrete(position = "top", expand = c(0, 0)) +
  scale_y_discrete(limits = rev(rf_vars_top15), expand = c(0, 0)) +
  coord_fixed(clip = "off") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    
    # Increased left margin to 50 to prevent cut-off of the first label
    plot.margin = ggplot2::margin(t = 5, r = 18, b = 5, l = 50),
    
    legend.position = "right",
    legend.key.height = unit(12, "cm"),
    legend.key.width  = unit(0.55, "cm")
  ) +
  guides(
    fill = guide_colorbar(
      barheight = unit(12, "cm"), 
      barwidth  = unit(0.55, "cm"),
      ticks.colour = "black", 
      frame.colour = "black"
    )
  ) +
  annotate("text", x = diag_labels$x, y = diag_labels$y_rev, label = diag_labels$label, hjust = 1, size = 3.6, fontface = "plain")

ggsave(file.path(OUT_CORR, "Fig_Correlation_Triangle_RF_Top15.png"), p_corr_triangle, width = 11, height = 9, dpi = 300, bg = "white")
# =============================================================
# FIG X: SENTIMENT DISPERSION
# =============================================================
daily_sentiment_tbl <- reviews_enriched %>%
  filter(!is.na(sentiment_final)) %>%
  group_by(business_id, date) %>%
  summarise(daily_sentiment = mean(sentiment_final, na.rm = TRUE), .groups = "drop")

restaurant_means <- raw_panel %>%
  group_by(business_id) %>%
  summarise(restaurant_mean_stars = mean(daily_avg_stars, na.rm = TRUE), .groups = "drop")

panel_for_disp <- raw_panel %>%
  left_join(restaurant_means, by = "business_id") %>%
  mutate(
    y_target = ifelse(daily_avg_stars >= restaurant_mean_stars, 1, 0),
    y_label  = factor(y_target, levels = c(0, 1), labels = c("Below-Average\nSatisfaction", "Above-Average\nSatisfaction"))
  ) %>%
  left_join(daily_sentiment_tbl, by = c("business_id", "date"))

plot_data_disp <- panel_for_disp %>%
  filter(!is.na(daily_sentiment), !is.na(y_label))

pX <- ggplot(plot_data_disp, aes(x = y_label, y = daily_sentiment, fill = y_label, color = y_label)) +
  geom_violin(alpha = 0.2, color = NA) +
  geom_boxplot(width = 0.3, outlier.alpha = 0.1, size = 0.7) +
  scale_fill_manual(values = c("#fab1a0", "#81ecec")) +
  scale_color_manual(values = c("#CC79A7", "#0984e3")) +
  labs(
    title = "Sentiment Dispersion",
    subtitle = "Daily sentiment distributions by satisfaction tier.",
    x = NULL, y = "Daily Sentiment Score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "grey40", margin = ggplot2::margin(b = 15))
  )

ggsave(file.path(OUT_SENTIMENT, "Fig_X_Sentiment_Dispersion_Pretty.png"), pX, width = 7, height = 5, dpi = 300)

# =============================================================
# BIGRAM PASTEL SENTIMENT CLOUDS 
# =============================================================
cat("Step: Generating Bigram Pastel Clouds...\n")
high_thresh <- quantile(reviews_enriched$sentiment_final, 0.90, na.rm = TRUE)
low_thresh  <- quantile(reviews_enriched$sentiment_final, 0.10, na.rm = TRUE)

# 10-Color Palettes
good_palette <- c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1", "#c6dbef", "#00b894", "#55efc4", "#81ecec", "#00cec9")
bad_palette <- c("#7E0080", "#9851B4", "#C20232", "#EF224B", "#ff7675", "#fd79a8", "#fab1a0", "#e84393", "#6c5ce7", "#a29bfe")

generate_vibrant_pastel_cloud <- function(data, filter_expr, color_palette, filename) {
  bigram_data <- data %>%
    dplyr::filter(!!rlang::parse_expr(filter_expr) & !is.na(text_clean)) %>%
    unnest_tokens(bigram, text_clean, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word[!(stop_words$word %in% c("not", "no", "never"))]) %>%
    filter(!word2 %in% stop_words$word) %>%
    filter(!word1 %in% c("restaurant", "food", "place", "ordered", "tucson")) %>%
    filter(!word2 %in% c("restaurant", "food", "place", "ordered", "tucson")) %>%
    mutate(bigram = paste(word1, word2, sep = " ")) %>%
    count(bigram, sort = TRUE)
  
  png(filename, width = 2400, height = 2400, res = 300)
  wordcloud(words = bigram_data$bigram, freq = bigram_data$n, max.words = 150, random.order = FALSE, colors = color_palette, scale = c(3.8, 0.8))
  dev.off()
}

# Generate and Save (Renamed to remove "_v2")
generate_vibrant_pastel_cloud(reviews_enriched, "sentiment_final >= high_thresh", good_palette, file.path(OUT_CLOUDS, "App_Fig_Cloud_HighSentiment_Pastel.png"))
generate_vibrant_pastel_cloud(reviews_enriched, "sentiment_final <= low_thresh", bad_palette, file.path(OUT_CLOUDS, "App_Fig_Cloud_LowSentiment_Pastel.png"))

