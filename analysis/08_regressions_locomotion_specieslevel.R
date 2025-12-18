# ============================================================
# analysis/08_regressions_locomotion_specieslevel.R
# Regression of log10 body mass vs LOCOMOTION TYPES (species-level)
#
# INPUT:
#   data/processed/species_locomotion.csv
#
# OUTPUT:
#   outputs/figures/regression_logbw_vs_locomotion.png
#   outputs/tables/regression_logbw_vs_locomotion_models.txt
# ============================================================

library(tidyverse)
library(ggplot2)
library(here)

dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1) Load data
# ------------------------------------------------------------
df_loco <- readr::read_csv(here("data", "processed", "species_locomotion.csv"),
                           show_col_types = FALSE)

# Expected columns:
# Species, Code (optional), logBW, Quadrupedal, Leap, Climb, Clamber, Suspension

# Basic checks (helps other users)
stopifnot(all(c("Species","logBW","Quadrupedal","Leap","Climb","Clamber","Suspension") %in% names(df_loco)))

# ------------------------------------------------------------
# 2) Long format for plotting
# ------------------------------------------------------------
df_loco_long <- df_loco %>%
  pivot_longer(
    cols = c(Quadrupedal, Leap, Climb, Clamber, Suspension),
    names_to = "Locomotion",
    values_to = "UsePercent"
  )

# ------------------------------------------------------------
# 3) Plot
# ------------------------------------------------------------
p_loco <- ggplot(df_loco_long, aes(x = UsePercent, y = logBW, color = Locomotion)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "Body mass vs locomotion type use",
    x = "Locomotion use (%)",
    y = "log10 Body mass",
    color = "Locomotion type"
  ) +
  theme_classic(base_family = "serif") +
  theme(
    plot.title = element_text(face = "bold", hjust = 0),
    legend.position = "bottom"
  )

print(p_loco)

ggsave(
  filename = here("outputs", "figures", "regression_logbw_vs_locomotion.png"),
  plot = p_loco,
  width = 12, height = 6, dpi = 300
)

# ------------------------------------------------------------
# 4) Save model summaries
# ------------------------------------------------------------
sink(here("outputs", "tables", "regression_logbw_vs_locomotion_models.txt"))

cat("\n--- Linear models: Locomotion types ---\n")
print(summary(lm(logBW ~ Quadrupedal, data = df_loco)))
print(summary(lm(logBW ~ Leap,        data = df_loco)))
print(summary(lm(logBW ~ Climb,       data = df_loco)))
print(summary(lm(logBW ~ Clamber,     data = df_loco)))
print(summary(lm(logBW ~ Suspension,  data = df_loco)))

sink()

message("Saved figure + model output in outputs/")
