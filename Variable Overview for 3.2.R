# ============================================================
# Script: Fig_3_1_Final_Corner_Layout.R
# Purpose: Corner Layout + Consistent Header Style for ALL boxes
# Output: figures/Fig_3_1_Corner_Final.png
# ============================================================

library(ggplot2)
library(dplyr)
library(stringr)
library(grid)

# 1. SETUP ===================================================
FIG_OUT <- file.path(getwd(), "figures")
dir.create(FIG_OUT, showWarnings = FALSE, recursive = TRUE)

# -- Palette --
pal_box     <- "#DCE3EA"  # Light Blue Header
pal_primary <- "#00618F"  # Dark Blue
pal_text    <- "#2C3E50"  # Body Text
pal_method  <- "#004466"  # Method Text
pal_line    <- "#B7C4D1"  # Connector Grey

# -- Data --
var_data <- tibble(
  Group = c("Engagement Dynamics", "Restaurant Attributes", "Restaurant Categories", "External Context"),
  Vars = c(
    "log_review_count, daily_review_count, daily_checkins, lag_reviews_1d, roll_checkins_7d, cum_reviews",
    "price_high, noise_high, wifi_binary, alcohol_binary, outdoor_seating, table_service, delivery, groups",
    "cat_fast_food, cat_dessert, cat_dietary_specialty",
    "TAVG_sq, TMAX_lag1, is_raining, Weekend, covid_period"
  ),
  Method = c(
    "Counts + Dynamics (log, lags, rolling, cumulative)",
    "Binary Indicators (presence / simplified)",
    "Binary Categories (dummies)",
    "Transforms + Controls (squared weather, time dummies)"
  )
)

# Outcome Text (Body Only)
outcome_body <- "Customer Satisfaction\n(Star Rating)\n\n1 if Daily Avg > Long-Run Avg\n(0 otherwise)"

wrap_txt <- function(x, w) paste(strwrap(x, width=w), collapse="\n")

# 2. COORDINATES =============================================
# Box Dimensions: Width = 4.0, Height = 2.2
w_half <- 2.0 
h_half <- 1.1

plot_data <- var_data %>%
  mutate(
    # Center positions for 4 corners
    x = c(2.5, 7.5, 2.5, 7.5),
    y = c(8.0, 8.0, 2.0, 2.0),
    
    # Text Anchors
    y_title  = y + 0.6,
    y_vars   = y + 0.05,
    y_method = y - 0.6,
    
    # Wrapped Text
    txt_vars   = vapply(Vars, wrap_txt, character(1), w=42),
    txt_method = paste("Method:", Method)
  )

# 3. DRAW PLOT ===============================================
p <- ggplot(plot_data) +
  
  # A. CONNECTORS (Behind)
  # From Corner Centers -> Plot Center (5,5)
  # Shortened to stop at the edge of the center box
  annotate("segment", x = 2.5, xend = 4.0, y = 8.0, yend = 6.0, color = pal_line, size = 1.2) + # TL
  annotate("segment", x = 7.5, xend = 6.0, y = 8.0, yend = 6.0, color = pal_line, size = 1.2) + # TR
  annotate("segment", x = 2.5, xend = 4.0, y = 2.0, yend = 4.0, color = pal_line, size = 1.2) + # BL
  annotate("segment", x = 7.5, xend = 6.0, y = 2.0, yend = 4.0, color = pal_line, size = 1.2) + # BR
  
  # --- CORNER BOXES ---
  
  # B. Backgrounds
  geom_rect(aes(xmin = x - w_half, xmax = x + w_half, 
                ymin = y - h_half, ymax = y + h_half), 
            fill = "white", color = pal_line, size = 0.5) +
  
  # C. Headers (Light Blue)
  geom_rect(aes(xmin = x - w_half, xmax = x + w_half, 
                ymin = y + 0.3, ymax = y + h_half), 
            fill = pal_box, color = NA) +
  
  # D. Text
  geom_text(aes(x = x, y = y_title + 0.1, label = Group), 
            fontface = "bold", color = pal_primary, size = 3.8) +
  geom_text(aes(x = x, y = y_vars - 0.1, label = txt_vars), 
            color = pal_text, size = 3.0, lineheight = 0.95) +
  geom_text(aes(x = x, y = y_method, label = txt_method), 
            fontface = "italic", color = pal_method, size = 2.8) +
  
  # --- CENTER OUTCOME BOX ---
  
  # E. Background (Center) - Slightly Taller
  annotate("rect", xmin = 3.5, xmax = 6.5, ymin = 4.0, ymax = 6.0, 
           fill = "white", color = pal_primary, size = 0.8) +
  
  # F. Header (Center) - Dark Blue to distinguish it as the Target
  annotate("rect", xmin = 3.5, xmax = 6.5, ymin = 5.5, ymax = 6.0, 
           fill = pal_primary, color = NA) +
  
  # G. Text (Center)
  # Header Text (White)
  annotate("text", x = 5.0, y = 5.75, label = "MODEL TARGET", 
           fontface = "bold", color = "white", size = 4.0) +
  # Body Text
  annotate("text", x = 5.0, y = 4.75, label = outcome_body, 
           color = pal_text, size = 3.2, fontface = "bold", lineheight = 1.1) +
  
  # H. SCALES
  xlim(0, 10) + ylim(0, 10) +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))

# 4. SAVE ====================================================
ggsave(file.path(FIG_OUT, "Fig_3_1_Corner_Final.png"), p, width = 11, height = 9, dpi = 300, bg="white")

cat("Final Figure saved to:", file.path(FIG_OUT, "Fig_3_1_Corner_Final.png"), "\n")