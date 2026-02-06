###############################################################
# Script: 09_Clustering_Analysis.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Perform Unsupervised Learning (K-Means Clustering) to identify
#   latent restaurant segments based on behavioral engagement.
#
#   This analysis replicates the specific feature set used in the
#   exploratory phase, clustering restaurants on four dimensions
#   of volume and consistency:
#     1. Average Daily Reviews (Volume)
#     2. Average Daily Check-ins (Volume)
#     3. Review Frequency (Consistency: % of days with a review)
#     4. Check-in Frequency (Consistency: % of days with a check-in)
#
# Methodology:
#   - Data aggregated to the business entity level.
#   - Invariant features removed to prevent singularity.
#   - Z-score scaling applied to normalize feature magnitudes.
#   - Labels (High/Low) assigned based on check-in volume centroids.
#
# Outputs 
#   - figures/elbow_k_selection/Fig_4a_Elbow.png
#   - figures/silhouette_k_selection/Fig_4b_Silhouette.png
#   - figures/pca_cluster_projection/App_Fig_PCA_Clusters.png (Final Visual)
#   - restaurant_clusters_k2.csv (Cluster assignments)
###############################################################

rm(list = ls())
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")

library(dplyr)
library(readr)
library(cluster)
library(factoextra)
library(ggplot2)
library(scales)
library(gt)

## -------------------------------------------------------------
## Output folders (STRICT: one fig per folder)
## -------------------------------------------------------------
FIG_ELBOW <- file.path(getwd(), "figures", "elbow_k_selection")
FIG_SIL   <- file.path(getwd(), "figures", "silhouette_k_selection")
FIG_PCA   <- file.path(getwd(), "figures", "pca_cluster_projection")
TAB_DIR   <- file.path(getwd(), "tables", "clustering")

dir.create(FIG_ELBOW, recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_SIL,   recursive = TRUE, showWarnings = FALSE)
dir.create(FIG_PCA,   recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR,   recursive = TRUE, showWarnings = FALSE)

## -------------------------------------------------------------
## Palette (academic blue/grey)
## -------------------------------------------------------------
pal_light <- "#DCE3EA"
pal_mid1  <- "#CBD2DB"
pal_mid2  <- "#B7C4D1"
pal_dark  <- "#00618F"

# ------------------------------------------------------------
# 1) Data Loading and Aggregation (restaurant level)
# ------------------------------------------------------------
panel <- read_csv("internal_panel_final.csv", show_col_types = FALSE)

restaurant_features <- panel %>%
  group_by(business_id) %>%
  summarise(
    mean_daily_reviews  = mean(daily_review_count, na.rm = TRUE),
    mean_daily_checkins = mean(daily_checkins, na.rm = TRUE),
    share_days_review   = mean(daily_review_count > 0, na.rm = TRUE),
    share_days_checkin  = mean(daily_checkins > 0, na.rm = TRUE),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 2) Preprocessing and Scaling
# ------------------------------------------------------------
data_for_scale <- restaurant_features %>% select(-business_id)

variances <- sapply(data_for_scale, var, na.rm = TRUE)
constant_cols <- names(variances)[variances == 0]
if (length(constant_cols) > 0) {
  data_for_scale <- data_for_scale %>% select(-all_of(constant_cols))
}

data_scaled <- scale(data_for_scale)

# ------------------------------------------------------------
# 3) K-means clustering (k=2)
# ------------------------------------------------------------
set.seed(123)
km_res <- kmeans(data_scaled, centers = 2, nstart = 25)
restaurant_features$cluster_raw <- km_res$cluster

# ------------------------------------------------------------
# 4) RELABEL CLUSTERS to match your interpretation
#    "Review-centric" = higher review activity & review-day share
#    "Traffic-centric" = higher check-in activity & check-in-day share
# ------------------------------------------------------------
cluster_centers <- restaurant_features %>%
  group_by(cluster_raw) %>%
  summarise(
    avg_daily_reviews   = mean(mean_daily_reviews,  na.rm = TRUE),
    avg_daily_checkins  = mean(mean_daily_checkins, na.rm = TRUE),
    active_days_reviews = mean(share_days_review,   na.rm = TRUE),
    active_days_checkin = mean(share_days_checkin,  na.rm = TRUE),
    n_restaurants       = n(),
    .groups = "drop"
  )

# Decide which cluster is "review-centric" vs "traffic-centric"
# Robust rule: compare standardized dominance (reviews vs check-ins).
cluster_centers <- cluster_centers %>%
  mutate(
    review_score  = scale(avg_daily_reviews)[,1] + scale(active_days_reviews)[,1],
    traffic_score = scale(avg_daily_checkins)[,1] + scale(active_days_checkin)[,1]
  )

review_cluster_id  <- cluster_centers$cluster_raw[which.max(cluster_centers$review_score)]
traffic_cluster_id <- cluster_centers$cluster_raw[which.max(cluster_centers$traffic_score)]

# Safety check: if both maxes are same (rare), fall back to avg reviews vs avg check-ins
if (review_cluster_id == traffic_cluster_id) {
  review_cluster_id  <- cluster_centers$cluster_raw[which.max(cluster_centers$avg_daily_reviews)]
  traffic_cluster_id <- cluster_centers$cluster_raw[which.max(cluster_centers$avg_daily_checkins)]
}

restaurant_features <- restaurant_features %>%
  mutate(
    cluster_label = case_when(
      cluster_raw == review_cluster_id  ~ "Review-centric",
      cluster_raw == traffic_cluster_id ~ "Traffic-centric",
      TRUE ~ NA_character_
    )
  )

# Save assignments
write_csv(
  restaurant_features %>% select(business_id, cluster_label),
  file.path(TAB_DIR, "restaurant_clusters_k2_labels.csv")
)

# ------------------------------------------------------------
# 5) TABLE 2 — Cluster Centers + Engagement Profiles (export-ready)
# ------------------------------------------------------------
table2 <- restaurant_features %>%
  group_by(cluster_label) %>%
  summarise(
    `Avg. Daily Reviews`        = mean(mean_daily_reviews,  na.rm = TRUE),
    `Avg. Daily Check-ins`      = mean(mean_daily_checkins, na.rm = TRUE),
    `Active Days (Reviews %)`   = mean(share_days_review,   na.rm = TRUE),
    `Active Days (Check-ins %)` = mean(share_days_checkin,  na.rm = TRUE),
    `Number of Restaurants`     = n(),
    .groups = "drop"
  ) %>%
  # Order rows: Review-centric first (matches your narrative)
  mutate(cluster_label = factor(cluster_label, levels = c("Review-centric","Traffic-centric"))) %>%
  arrange(cluster_label) %>%
  rename(Cluster = cluster_label)

# Save as CSV (clean, for LaTeX/Word import)
write_csv(table2, file.path(TAB_DIR, "Table2_Cluster_Centers.csv"))

# Save a nice formatted table (PNG + HTML) via gt
tab_gt <- table2 %>%
  gt() %>%
  tab_header(
    title = md("**Table 2: Cluster Centers and Engagement Profiles of Tucson Restaurants**")
  ) %>%
  fmt_number(
    columns = c(`Avg. Daily Reviews`, `Avg. Daily Check-ins`),
    decimals = 3
  ) %>%
  fmt_percent(
    columns = c(`Active Days (Reviews %)`, `Active Days (Check-ins %)`),
    decimals = 2
  ) %>%
  cols_align(align = "left", columns = Cluster) %>%
  cols_align(align = "right", columns = -Cluster)

gtsave(tab_gt, file.path(TAB_DIR, "Table2_Cluster_Centers.html"))
gtsave(tab_gt, file.path(TAB_DIR, "Table2_Cluster_Centers.png"))

# ------------------------------------------------------------
# 6) Elbow + Silhouette (Appendix Figure 5)
# ------------------------------------------------------------
p_elbow <- fviz_nbclust(data_scaled, kmeans, method = "wss", linecolor = pal_dark) +
  geom_vline(xintercept = 2, linetype = "dashed", color = pal_mid2, linewidth = 0.9) +
  labs(title = NULL, subtitle = NULL, x = "Number of clusters (k)", y = "Within-cluster sum of squares") +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(FIG_ELBOW, "App_Fig5a_Elbow.png"),
  plot = p_elbow, width = 6, height = 4, bg = "white"
)

p_sil <- fviz_nbclust(data_scaled, kmeans, method = "silhouette", linecolor = pal_dark) +
  labs(title = NULL, subtitle = NULL, x = "Number of clusters (k)", y = "Average silhouette width") +
  theme_minimal(base_size = 13)

ggsave(
  filename = file.path(FIG_SIL, "App_Fig5b_Silhouette.png"),
  plot = p_sil, width = 6, height = 4, bg = "white"
)

# ------------------------------------------------------------
# 7) PCA Projection Plot (Figure 2) — relabeled legend
# ------------------------------------------------------------
png(
  filename = file.path(FIG_PCA, "Fig2_PCA_Clusters.png"),
  width = 1400, height = 900, res = 200
)

pca_plot <- fviz_cluster(
  list(data = data_scaled, cluster = restaurant_features$cluster_label),
  geom = "point",
  pointsize = 1.4,
  alpha = 0.70,
  ellipse = TRUE,
  ellipse.type = "confidence",
  ellipse.level = 0.80,
  ellipse.alpha = 0.10,
  ellipse.border.remove = FALSE,
  ellipse.size = 0.9,
  show.clust.cent = TRUE,
  clust.cent.size = 4,
  palette = c(pal_dark, pal_mid2),  # map in legend order below
  ggtheme = theme_minimal(base_size = 14)
) +
  labs(
    title = NULL,
    x = "Overall Engagement (PC1)",
    y = "Engagement Structure (PC2)",
    color = "Engagement profile",
    shape = "Engagement profile"
  ) +
  guides(fill = "none")

print(pca_plot)
dev.off()