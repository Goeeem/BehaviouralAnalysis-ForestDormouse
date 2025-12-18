# ============================================================
# analysis/05_behavior_preference.R
# Behavior preference (Male vs Female): clustered % bar chart
#
# INPUT:
#   data/processed/df_master.csv
#
# OUTPUT:
#   outputs/tables/behavior_preference_m_vs_f.csv
#   outputs/figures/behavior_preference_m_vs_f.png
# ============================================================

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(here)

# ----------------------------
# 0) Load input + folders
# ----------------------------
df_master <- readr::read_csv(here("data", "processed", "df_master.csv"),
                            show_col_types = FALSE)

dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

SEX_COLORS <- c("Female" = "#1F77B4", "Male" = "#FF7F0E")
Y_MAX_BEHAV <- 100

# ----------------------------
# 1) Clean + standardize behavior
# ----------------------------
df_beh_base <- df_master %>%
  mutate(
    Sex = as.character(Sex),
    Behavior_raw = str_trim(toupper(as.character(Behavior)))
  ) %>%
  filter(!is.na(Behavior_raw), Behavior_raw != "", Behavior_raw != "NA")

# Map full names to codes
df_beh_base <- df_beh_base %>%
  mutate(
    BehaviorCode = recode(Behavior_raw,
      "TRAVELING"  = "T",
      "TRAVELLING" = "T",
      "FEEDING"    = "F",
      "RESTING"    = "R",
      "GROOMING"   = "G",
      "EXPLORING"  = "E",
      .default = Behavior_raw
    )
  ) %>%
  # Keep only the intended 5 categories for this figure
  filter(BehaviorCode %in% c("T","F","R","G","E"))

# ----------------------------
# 2) Compute % behavior by Sex
# ----------------------------
df_beh_use <- df_beh_base %>%
  count(Sex, BehaviorCode, name = "Count") %>%
  group_by(Sex) %>%
  mutate(Percent = 100 * Count / sum(Count)) %>%
  ungroup()

# Save summary table
readr::write_csv(df_beh_use, here("outputs","tables","behavior_preference_m_vs_f.csv"))

# Order on x-axis
df_beh_use <- df_beh_use %>%
  mutate(BehaviorCode = factor(BehaviorCode, levels = c("T","F","R","G","E")))

# ----------------------------
# 3) Plot
# ----------------------------
p_behavior <- ggplot(df_beh_use, aes(x = BehaviorCode, y = Percent, fill = Sex)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(
    aes(label = sprintf("%.2f", Percent)),
    position = position_dodge(width = 0.8),
    vjust = -0.35,
    size = 3,
    family = "serif"
  ) +
  scale_fill_manual(values = SEX_COLORS) +
  scale_y_continuous(
    limits = c(0, Y_MAX_BEHAV),
    breaks = seq(0, Y_MAX_BEHAV, 25),
    expand = expansion(mult = c(0, 0.15))
  ) +
  labs(
    title = "BEHAVIOR PREFERENCE",
    x = NULL,
    y = "% Observations",
    fill = NULL
  ) +
  theme_classic(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    legend.position = "top",
    axis.text.x = element_text(face = "bold", size = 12),
    axis.text.y = element_text(size = 10),
    plot.margin = margin(10, 12, 10, 12)
  )

# Behavior legend text
behavior_legend_text <- textGrob(
  "Behavior codes: T = Traveling, F = Feeding, R = Resting, G = Grooming, E = Exploring",
  gp = gpar(fontsize = 10, fontfamily = "serif"),
  just = "centre"
)

final_behavior_figure <- grid.arrange(
  p_behavior,
  behavior_legend_text,
  ncol = 1,
  heights = c(4, 0.6)
)

grid::grid.newpage()
grid::grid.draw(final_behavior_figure)

ggsave(
  filename = here("outputs","figures","behavior_preference_m_vs_f.png"),
  plot = final_behavior_figure,
  width = 10,
  height = 6,
  dpi = 300
)

message("Saved figure to outputs/figures/behavior_preference_m_vs_f.png")