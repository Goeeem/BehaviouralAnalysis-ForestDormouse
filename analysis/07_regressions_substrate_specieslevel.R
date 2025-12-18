# ============================================================
# analysis/07_regressions_substrate_specieslevel.R
# Regression of log10 body mass vs substrate use (species-level)
# A) Substrate SIZE use (Small/Medium/Large)
# B) Substrate INCLINATION use (Horizontal/Oblique/Vertical)
#
# INPUT:
#   data/processed/species_substrate_size.csv
#   data/processed/species_substrate_inclination.csv
#
# OUTPUT:
#   outputs/figures/regression_logbw_vs_substrateuse.png
#   outputs/tables/regression_logbw_vs_substrateuse_models.txt
# ============================================================

library(tidyverse)
library(ggplot2)
library(patchwork)
library(here)

dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1) Load tables from CSV
# ------------------------------------------------------------
df_size <- readr::read_csv(here("data", "processed", "species_substrate_size.csv"),
                           show_col_types = FALSE)

df_incl <- readr::read_csv(here("data", "processed", "species_substrate_inclination.csv"),
                           show_col_types = FALSE)

# If you MUST use only 13 arboreal species (exclude Dryomys), uncomment:
# df_size <- df_size %>% filter(Code != "DRYO")
# df_incl <- df_incl %>% filter(Code != "DRYO")

# ------------------------------------------------------------
# 2) Prepare for plotting
# ------------------------------------------------------------
df_size <- df_size %>%
  mutate(logBW = log10(BW_g))

df_size_long <- df_size %>%
  pivot_longer(cols = c(Small, Medium, Large),
               names_to = "SizeClass",
               values_to = "UsePercent")

# If df_incl already has LOGBW numeric, keep it as logBW
df_incl <- df_incl %>%
  mutate(logBW = as.numeric(logBW))

df_incl_long <- df_incl %>%
  pivot_longer(cols = c(Horizontal, Oblique, Vertical),
               names_to = "InclinationClass",
               values_to = "UsePercent")

# ------------------------------------------------------------
# 3) Plots
# ------------------------------------------------------------
pA_size <- ggplot(df_size_long, aes(x = UsePercent, y = logBW, color = SizeClass)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "A) Substrate size use vs log10 body mass",
    x = "Substrate use (%)",
    y = "log10 Body mass (g)",
    color = NULL
  ) +
  theme_classic(base_family = "serif") +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        legend.position = "bottom")

pB_incl <- ggplot(df_incl_long, aes(x = UsePercent, y = logBW, color = InclinationClass)) +
  geom_point(size = 3, alpha = 0.85) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(
    title = "B) Substrate inclination use vs log10 body mass",
    x = "Substrate use (%)",
    y = "log10 Body mass (g)",
    color = NULL
  ) +
  theme_classic(base_family = "serif") +
  theme(plot.title = element_text(face = "bold", hjust = 0),
        legend.position = "bottom")

# Optional labels for dormice
label_codes <- c("GRAP", "DRYO")

pA_size <- pA_size +
  geom_text(
    data = df_size_long %>% filter(Code %in% label_codes, SizeClass == "Medium"),
    aes(label = paste0(substr(Species, 1, 1), ". ", stringr::word(Species, 2))),
    color = "black",
    nudge_x = 2,
    size = 3,
    show.legend = FALSE
  )

pB_incl <- pB_incl +
  geom_text(
    data = df_incl_long %>% filter(Code %in% label_codes, InclinationClass == "Oblique"),
    aes(label = paste0(substr(Species, 1, 1), ". ", stringr::word(Species, 2))),
    color = "black",
    nudge_x = 2,
    size = 3,
    show.legend = FALSE
  )

final_regression_figure <- pA_size + pB_incl + plot_layout(ncol = 2)

print(final_regression_figure)

ggsave(
  filename = here("outputs", "figures", "regression_logbw_vs_substrateuse.png"),
  plot = final_regression_figure,
  width = 13, height = 5.5, dpi = 300
)

# ------------------------------------------------------------
# 4) Save regression outputs to a text file
# ------------------------------------------------------------
sink(here("outputs", "tables", "regression_logbw_vs_substrateuse_models.txt"))

cat("\n--- Linear models: Size ---\n")
print(summary(lm(logBW ~ Small,  data = df_size)))
print(summary(lm(logBW ~ Medium, data = df_size)))
print(summary(lm(logBW ~ Large,  data = df_size)))

cat("\n--- Linear models: Inclination ---\n")
print(summary(lm(logBW ~ Horizontal, data = df_incl)))
print(summary(lm(logBW ~ Oblique,    data = df_incl)))
print(summary(lm(logBW ~ Vertical,   data = df_incl)))

sink()

message("Saved figure + model output in outputs/")
