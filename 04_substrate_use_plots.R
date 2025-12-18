# ============================================================
# analysis/04_substrate_use_plots.R
# Substrate use (Male vs Female): Type / Size / Inclination
# Produces one combined 3-panel figure
#
# INPUT:
#   data/processed/df_master.csv
#
# OUTPUT:
#   outputs/tables/use_substrate_type.csv
#   outputs/tables/use_substrate_size.csv
#   outputs/tables/use_substrate_inclination.csv
#   outputs/figures/use_of_substrates_bar_m_vs_f.png
# ============================================================

library(tidyverse)
library(ggplot2)
library(gridExtra)
library(grid)
library(here)

# ----------------------------
# 0) Load input + set folders
# ----------------------------
df_master <- readr::read_csv(here("data", "processed", "df_master.csv"),
                            show_col_types = FALSE)

dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

SEX_COLORS <- c("Female" = "#1F77B4", "Male" = "#FF7F0E")
Y_MAX_USE <- 100

# ----------------------------
# 1) Standardize codes (robust)
# ----------------------------
extract_code <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- toupper(x)
  code <- ifelse(str_detect(x, "\\("),
                 str_match(x, "\\(([^)]+)\\)")[, 2],
                 x)
  str_trim(code)
}

df_use_base <- df_master %>%
  mutate(
    Sex = as.character(Sex),
    Substrate_Type   = extract_code(Substrate_Type),
    Substrate_Size   = extract_code(Substrate_Size),
    Substrate_Orient = extract_code(Substrate_Orient)
  ) %>%
  mutate(
    # normalize synonyms
    Substrate_Type = case_when(
      Substrate_Type %in% c("B","BRANCH","BRANCHES") ~ "B",
      Substrate_Type %in% c("F","FLOOR","GROUND")   ~ "F",
      Substrate_Type %in% c("W","WIRE")             ~ "W",
      TRUE ~ Substrate_Type
    ),
    Substrate_Size = case_when(
      Substrate_Size %in% c("F","FINE") ~ "F",
      Substrate_Size %in% c("S","SMALL") ~ "S",
      Substrate_Size %in% c("M","MEDIUM") ~ "M",
      Substrate_Size %in% c("L","LARGE") ~ "L",
      Substrate_Size %in% c("VL","V LARGE","V. LARGE","VERY LARGE","VERY_LARGE","V-LARGE") ~ "VL",
      TRUE ~ Substrate_Size
    ),
    Substrate_Orient = case_when(
      Substrate_Orient %in% c("H","HORIZONTAL") ~ "H",
      Substrate_Orient %in% c("O","OBLIQUE","INCLINED") ~ "O",
      Substrate_Orient %in% c("S","STEEP") ~ "S",
      Substrate_Orient %in% c("V","VERTICAL") ~ "V",
      TRUE ~ Substrate_Orient
    )
  ) %>%
  # remove wire
  filter(is.na(Substrate_Type) | Substrate_Type != "W")

# Keep only valid codes (prevents earlier H/V leaking into Size)
valid_type   <- c("B","F")
valid_size   <- c("F","S","M","L","VL")
valid_orient <- c("H","O","S","V")

df_use_base <- df_use_base %>%
  mutate(
    Substrate_Type   = ifelse(Substrate_Type   %in% valid_type,   Substrate_Type,   NA),
    Substrate_Size   = ifelse(Substrate_Size   %in% valid_size,   Substrate_Size,   NA),
    Substrate_Orient = ifelse(Substrate_Orient %in% valid_orient, Substrate_Orient, NA)
  )

# ----------------------------
# 2) Compute % use by sex
# ----------------------------
use_percent <- function(df, var) {
  df %>%
    filter(!is.na(.data[[var]]), .data[[var]] != "") %>%
    count(Sex, Value = .data[[var]], name = "Count") %>%
    group_by(Sex) %>%
    mutate(Percent = 100 * Count / sum(Count)) %>%
    ungroup()
}

df_use_type   <- use_percent(df_use_base, "Substrate_Type")
df_use_size   <- use_percent(df_use_base, "Substrate_Size")
df_use_orient <- use_percent(df_use_base, "Substrate_Orient")

# ----------------------------
# 3) labels + ordering
# ----------------------------
df_use_type <- df_use_type %>%
  mutate(
    Value = recode(Value, "B" = "BRANCHES", "F" = "FLOOR"),
    Value = factor(Value, levels = c("FLOOR", "BRANCHES"))
  )

df_use_size <- df_use_size %>%
  mutate(
    Value = recode(Value, "F"="FINE","S"="SMALL","M"="MEDIUM","L"="LARGE","VL"="VERY LARGE"),
    Value = factor(Value, levels = c("FINE","SMALL","MEDIUM","LARGE","VERY LARGE"))
  )

df_use_orient <- df_use_orient %>%
  mutate(
    Value = recode(Value, "H"="HORIZONTAL","O"="OBLIQUE","S"="STEEP","V"="VERTICAL"),
    Value = factor(Value, levels = c("HORIZONTAL","OBLIQUE","STEEP","VERTICAL"))
  )

# Save summary tables
readr::write_csv(df_use_type,   here("outputs","tables","use_substrate_type.csv"))
readr::write_csv(df_use_size,   here("outputs","tables","use_substrate_size.csv"))
readr::write_csv(df_use_orient, here("outputs","tables","use_substrate_inclination.csv"))

# ----------------------------
# 4) Plot function
# ----------------------------
plot_use_bar <- function(df, title) {
  ggplot(df, aes(x = Value, y = Percent, fill = Sex)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.7) +
    geom_text(
      aes(label = sprintf("%.2f", Percent)),
      position = position_dodge(width = 0.8),
      vjust = -0.3,
      size = 2.6,
      family = "serif"
    ) +
    scale_fill_manual(values = SEX_COLORS) +
    scale_y_continuous(
      limits = c(0, Y_MAX_USE),
      breaks = seq(0, Y_MAX_USE, 25),
      expand = expansion(mult = c(0, 0.15))
    ) +
    scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 10)) +
    labs(title = title, x = NULL, y = "% Use", fill = NULL) +
    theme_classic(base_family = "serif") +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12),
      legend.position = "top",
      axis.text.x = element_text(face = "bold", size = 8, angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y = element_text(size = 9),
      plot.margin = margin(10, 12, 10, 12)
    )
}

p_type   <- plot_use_bar(df_use_type,   "Use of Substrates: Type")
p_size   <- plot_use_bar(df_use_size,   "Use of Substrates: Size")
p_orient <- plot_use_bar(df_use_orient, "Use of Substrates: Inclination")

main_title <- textGrob(
  "USE OF SUBSTRATES",
  gp = gpar(fontface = "bold", fontsize = 18, fontfamily = "serif")
)

final_use_bar_figure <- grid.arrange(p_type, p_size, p_orient, ncol = 3, top = main_title)

grid::grid.newpage()
grid::grid.draw(final_use_bar_figure)

ggsave(
  filename = here("outputs","figures","use_of_substrates_bar_m_vs_f.png"),
  plot = final_use_bar_figure,
  width = 18, height = 6, dpi = 300
)

message("Saved figure to outputs/figures/use_of_substrates_bar_m_vs_f.png")