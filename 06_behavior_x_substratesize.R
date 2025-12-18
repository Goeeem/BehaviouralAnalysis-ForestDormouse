# ============================================================
# analysis/06_behavior_x_substratesize.R
# Behavior × Substrate SIZE — faceted by Sex
#
# INPUT:
#   data/processed/df_master.csv
#
# OUTPUT:
#   outputs/tables/behavior_x_substrate_size_props.csv
#   outputs/figures/behavior_x_substrate_size_faceted_sex.png
# ============================================================

library(tidyverse)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(here)

# ----------------------------
# 0) Load input + output folders
# ----------------------------
df_master <- readr::read_csv(here("data", "processed", "df_master.csv"),
                            show_col_types = FALSE)

dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

# ----------------------------
# 1) Helpers
# ----------------------------
na_like_to_na <- function(x) {
  x <- str_trim(as.character(x))
  x[x %in% c("NA","N/A","NULL","-","--","")] <- NA
  x
}

extract_code <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- toupper(x)
  code <- ifelse(str_detect(x, "\\("),
                 str_match(x, "\\(([^)]+)\\)")[, 2],
                 x)
  str_trim(code)
}

valid_size <- c("F","S","M","L","VL")

# ----------------------------
# 2) Clean + keep only valid values
# ----------------------------
df_bxss <- df_master %>%
  mutate(
    Sex = str_trim(as.character(Sex)),
    Behavior = na_like_to_na(Behavior),
    Substrate_Size = extract_code(na_like_to_na(Substrate_Size))
  ) %>%
  filter(!is.na(Sex), !is.na(Behavior), !is.na(Substrate_Size)) %>%
  filter(Substrate_Size %in% valid_size)

# ----------------------------
# 3) Convert behavior to codes (T/F/R/G/E)
# ----------------------------
df_bxss <- df_bxss %>%
  mutate(
    Behavior = toupper(str_trim(Behavior)),
    BehaviorCode = recode(Behavior,
      "TRAVELING"  = "T",
      "TRAVELLING" = "T",
      "FEEDING"    = "F",
      "RESTING"    = "R",
      "GROOMING"   = "G",
      "EXPLORING"  = "E",
      .default = Behavior
    )
  ) %>%
  filter(BehaviorCode %in% c("T","F","R","G","E"))

behavior_legend_grob <- textGrob(
  "Behavior codes: T = Traveling, F = Feeding, R = Resting, G = Grooming, E = Exploring",
  gp = gpar(fontsize = 10, fontfamily = "serif"),
  just = "centre"
)

# ----------------------------
# 4) Compute proportions within Sex × Behavior
# ----------------------------
df_plot <- df_bxss %>%
  count(Sex, BehaviorCode, Substrate_Size, name = "n") %>%
  group_by(Sex, BehaviorCode) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup() %>%
  complete(Sex, BehaviorCode, Substrate_Size, fill = list(n = 0, prop = 0)) %>%
  mutate(
    BehaviorCode = factor(BehaviorCode, levels = c("T","F","R","G","E")),
    Substrate_Size = factor(Substrate_Size,
      levels = c("F","S","M","L","VL"),
      labels = c("Fine", "Small", "Medium", "Large", "Very Large")
    )
  )

# Save table 
readr::write_csv(df_plot, here("outputs","tables","behavior_x_substrate_size_props.csv"))

# ----------------------------
# 5) Plot
# ----------------------------
p_bxss <- ggplot(df_plot, aes(x = BehaviorCode, y = prop, fill = Substrate_Size)) +
  geom_col(width = 0.75, color = "white", linewidth = 0.2) +
  scale_y_continuous(labels = percent_format(accuracy = 1),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_fill_discrete(drop = TRUE) +
  labs(
    title = "Substrate Size Use by Behavior",
    x = NULL,
    y = "% within behavior",
    fill = "Substrate Size"
  ) +
  facet_wrap(~Sex, nrow = 1) +
  theme_classic(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    axis.text.x = element_text(face = "bold", size = 11),
    axis.text.y = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    plot.margin = margin(10, 10, 5, 10)
  )

final_bxss_figure <- grid.arrange(
  p_bxss,
  behavior_legend_grob,
  ncol = 1,
  heights = c(4, 0.45)
)

grid::grid.newpage()
grid::grid.draw(final_bxss_figure)

ggsave(
  filename = here("outputs","figures","behavior_x_substrate_size_faceted_sex.png"),
  plot = final_bxss_figure,
  width = 12, height = 6, dpi = 300
)

message("Saved figure to outputs/figures/behavior_x_substrate_size_faceted_sex.png")