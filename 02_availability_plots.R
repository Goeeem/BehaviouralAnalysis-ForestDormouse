# ============================================================
# analysis/02_availability_plots.R
# Availability of substrates
# Output: outputs/figures/substrate_availability.png
# ============================================================

library(tidyverse)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(grid)
library(here)

# ---------------------------
# 1) AVAILABILITY TABLES (A)
# ---------------------------

df_avail_type <- tibble(
  Substrate_Type = c("B", "F"),
  A = c(0.7161, 0.2839)
)

df_avail_size <- tibble(
  Substrate_Size = c("F", "S", "M", "L", "VL"),
  A = c(0.2198, 0.2747, 0.0934, 0.1648, 0.2473)
)

df_avail_orient <- tibble(
  Substrate_Orient = c("H", "O", "S", "V"),
  A = c(0.2007, 0.3366, 0.2669, 0.1958)
)

# ---------------------------
# 2) COLORS + LEGEND LABELS
# ---------------------------

Set1_colors <- rev(brewer.pal(5, "Set1"))

COLOR_MAP_TYPE <- c("F" = Set1_colors[2], "B" = Set1_colors[1])
COLOR_MAP_SIZE <- c("VL" = Set1_colors[5], "S" = Set1_colors[4], "F" = Set1_colors[3],
                    "L"  = Set1_colors[2], "M" = Set1_colors[1])
COLOR_MAP_ORIENT <- c("O" = Set1_colors[4], "S" = Set1_colors[3],
                      "H" = Set1_colors[2], "V" = Set1_colors[1])

LABELS_TYPE <- c("B" = "Branch (B)", "F" = "Floor (F)")
LABELS_SIZE <- c("F" = "Fine (F)", "S" = "Small (S)", "M" = "Medium (M)",
                 "L" = "Large (L)", "VL" = "Very Large (VL)")
LABELS_ORIENT <- c("H" = "Horizontal (H)", "O" = "Oblique (O)",
                   "S" = "Steep (S)", "V" = "Vertical (V)")

# ---------------------------
# 3) DONUT PLOT FUNCTION
# ---------------------------

create_donut_plot <- function(df, category_col, title, color_map, label_map) {

  df_plot <- df %>%
    mutate(
      Category = .data[[category_col]],
      fraction = A / sum(A),
      ymax = cumsum(fraction),
      ymin = c(0, head(ymax, n = -1)),
      label_y = (ymax + ymin) / 2,
      label_text = paste0(round(A * 100, 1), "%")
    ) %>%
    arrange(desc(A))

  donut_width <- 1

  ggplot(df_plot,
         aes(ymax = ymax, ymin = ymin, xmax = donut_width + 1,
             xmin = donut_width, fill = Category)) +
    geom_rect(color = "white") +
    geom_text(
      x = donut_width + 0.5,
      aes(y = label_y, label = label_text),
      size = 3, color = "black",
      fontface = "bold", family = "serif"
    ) +
    coord_polar(theta = "y") +
    theme_void(base_family = "serif", base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, family = "serif"),
      legend.position = "bottom",
      legend.direction = "vertical",
      legend.title = element_text(size = 12, face = "bold", family = "serif"),
      legend.text = element_text(size = 9, family = "serif"),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
      legend.key.size = unit(0.3, "cm")
    ) +
    labs(title = title, fill = title) +
    scale_fill_manual(values = color_map, labels = label_map)
}

# ---------------------------
# 4) CREATE PLOTS
# ---------------------------

plot_type <- create_donut_plot(df_avail_type, "Substrate_Type", "Substrate Type",
                              COLOR_MAP_TYPE, LABELS_TYPE)

plot_size <- create_donut_plot(df_avail_size, "Substrate_Size", "Substrate Size",
                              COLOR_MAP_SIZE, LABELS_SIZE)

plot_orient <- create_donut_plot(df_avail_orient, "Substrate_Orient", "Substrate Inclination",
                                COLOR_MAP_ORIENT, LABELS_ORIENT)

# ---------------------------
# 5) FINAL ARRANGEMENT
# ---------------------------

main_title_grob <- textGrob(
  "AVAILABILITY OF SUBSTRATES",
  gp = gpar(fontface = "bold", fontsize = 20, fontfamily = "serif"),
  just = "centre"
)

plot_final <- grid.arrange(
  plot_type, plot_size, plot_orient,
  ncol = 3,
  top = main_title_grob
)

# ---------------------------
# 6) SAVE OUTPUT
# ---------------------------

dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

ggsave(
  filename = here("outputs", "figures", "substrate_availability_without_wire.png"),
  plot = plot_final,
  width = 18, height = 7.5, dpi = 300
)

message("Saved figure to outputs/figures/substrate_availability_without_wire.png")
