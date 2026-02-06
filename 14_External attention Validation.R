###############################################################
# Script: 13_External_Attention_Bridge.R
# Author: Selena-Leila Rahimzadeh
#
# Purpose:
#   Explanatory analysis of external digital attention mechanisms
#   using a cross-platform bridge between historical Yelp data
#   and contemporary Google Places information.
#
# Notes:
#   - Analysis is cross-sectional and descriptive
#   - Results are interpreted as associations, not causal effects
#   - Temporal mismatch between platforms is explicitly acknowledged
###############################################################

# ==============================
# 0. Working Directory
# ==============================
setwd("C:/Users/selii/OneDrive/Goethe/1. Semester/DSMA/Data/Data")

# ==============================
# 1. Libraries
# ==============================
library(dplyr)
library(ggplot2)
library(stringdist)
library(readr)

# ==============================
# 2. Data Preparation: Bridge Strategy
# ==============================

# Load internal Yelp panel
internal_panel_final <- read_csv("internal_panel_final.csv", show_col_types = FALSE)

# Aggregate internal Yelp panel to business level
internal_biz_level <- internal_panel_final %>%
  group_by(business_id) %>%
  summarise(
    yelp_name      = first(name),
    yelp_stars     = mean(stars, na.rm = TRUE),
    total_checkins = sum(daily_checkins, na.rm = TRUE),
    price_range    = first(price_range),
    .groups = "drop"
  )

# Load external Google Places data
google_places_full <- read_csv("google_places_full.csv", show_col_types = FALSE)

# Merge internal and external data
analysis_master <- internal_biz_level %>%
  inner_join(google_places_full, by = "business_id") %>%
  mutate(
    name_dist = stringdist(yelp_name, google_name, method = "jw"),
    visibility_score =
      scale(google_user_ratings_total) +
      scale(google_photo_count)
  )

# ==============================
# 3. Bridge Validation Summary
# ==============================

cat("\n--- CROSS-PLATFORM BRIDGE VALIDATION (JARO-WINKLER) ---\n")

validation_summary <- analysis_master %>%
  mutate(
    match_quality = ifelse(name_dist < 0.15, "Reliable", "Suspect")
  )

print(table(validation_summary$match_quality))

analysis_filtered <- analysis_master %>%
  filter(name_dist < 0.25)

# ==============================
# 4. Variable Construction for Analysis
# ==============================

analysis_final <- analysis_filtered %>%
  mutate(
    Quality = ifelse(yelp_stars >= 4, "High Quality", "Low/Medium Quality"),
    Attention = ifelse(
      visibility_score > median(visibility_score, na.rm = TRUE),
      "High Attention",
      "Low Attention"
    )
  )

# ==============================
# 5. Graphic 1: Bridge Validation
# ==============================

p_validation <- ggplot(
  analysis_final,
  aes(x = yelp_stars, y = google_rating)
) +
  geom_jitter(alpha = 0.3, color = "#0984e3", size = 2) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "#CC79A7", size = 1
  ) +
  labs(
    x = "Historical Yelp Rating (Internal)",
    y = "Current Google Rating (External)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(
      color = "grey40",
      margin = ggplot2::margin(b = 15)
    ),
    axis.title = element_text(face = "bold")
  )

# ==============================
# 6. Graphic 2: Attention Amplifier
# ==============================

plot_data_clean <- analysis_final %>%
  filter(!is.na(Attention))

p_attention <- ggplot(
  plot_data_clean,
  aes(
    x = Quality,
    y = log(total_checkins + 1),
    fill = Attention,
    color = Attention
  )
) +
  geom_violin(alpha = 0.2, color = NA,
              position = position_dodge(width = 0.8)) +
  geom_boxplot(
    width = 0.2,
    outlier.alpha = 0.1,
    size = 0.7,
    position = position_dodge(width = 0.8)
  ) +
  scale_fill_manual(
    name = "External Attention",
    values = c("#81ecec", "#fab1a0")
  ) +
  scale_color_manual(
    name = "External Attention",
    values = c("#0984e3", "#CC79A7")
  ) +
  labs(
    x = "Internal Quality Tier (Yelp Stars)",
    y = "Log of Cumulative Check-ins"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(
      color = "grey40",
      margin = ggplot2::margin(b = 15)
    ),
    axis.text.x = element_text(face = "bold")
  )

# ==============================
# 7. Regression Models
# ==============================

model_validation <- lm(
  google_rating ~ yelp_stars,
  data = analysis_final
)

model_attention <- lm(
  log(total_checkins + 1) ~ yelp_stars + visibility_score,
  data = analysis_final
)

# ==============================
# 8. Output
# ==============================

ggsave(
  "appendix_validation_plot.png",
  plot = p_validation,
  width = 8,
  height = 6
)

ggsave(
  "results_attention_amplifier.png",
  plot = p_attention,
  width = 8,
  height = 6,
  dpi = 300
)

cat("\n--- REGRESSION RESULTS: BRIDGE VALIDATION ---\n")
print(summary(model_validation))

cat("\n--- REGRESSION RESULTS: ATTENTION & ENGAGEMENT ---\n")
print(summary(model_attention))

# ============================================================
# Appendix: Jaro-Winkler Bridge Validation (Section 5.1.2)
# ============================================================

validation_plot_data <- analysis_final

pretty_theme_bridge <- theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16,
      margin = ggplot2::margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 11,
      color = "grey40",
      margin = ggplot2::margin(b = 15)
    ),
    axis.title = element_text(face = "bold", color = "grey20"),
    panel.grid.minor = element_blank(),
    plot.margin = ggplot2::margin(30, 30, 30, 30)
  )

p_bridge_val <- ggplot(
  validation_plot_data,
  aes(x = yelp_stars, y = google_rating)
) +
  geom_jitter(alpha = 0.4, color = "#ffc0cb", size = 2) +
  geom_abline(
    slope = 1, intercept = 0,
    linetype = "dashed", color = "#CC79A7", linewidth = 1
  ) +
  geom_smooth(
    method = "lm",
    color = "#0984e3",
    se = FALSE,
    linetype = "solid"
  ) +
  labs(
    title = "Validation of the Cross-Platform Bridge",
    subtitle = "Consensus between historical internal performance and contemporary external reputation.",
    x = "Historical Yelp Rating (Internal)",
    y = "Current Google Rating (External)"
  ) +
  pretty_theme_bridge

ggsave(
  "App_Fig_Bridge_Validation_Section512.png",
  plot = p_bridge_val,
  width = 8,
  height = 6,
  dpi = 300,
  bg = "white"
)

cat("✔ Bridge Validation Plot generated for Section 5.1.2.\n")
