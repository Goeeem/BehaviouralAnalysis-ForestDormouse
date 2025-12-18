# ============================================================
# analysis/03_jacobs_index.R
# Jacobs' Index (D) for substrate Type / Size / Inclination
#
# INPUT:
#   data/processed/df_master.csv
#
# OUTPUT:
#   outputs/tables/jacobs_unmatched_values_*.csv
#   outputs/tables/jacobs_*.csv
#   outputs/figures/jacobs_index_all.png
# ============================================================

library(tidyverse)
library(stringr)
library(ggplot2)
library(gridExtra)
library(grid)
library(patchwork)
library(here)

# ---------------------------
# 0) Load input data
# ---------------------------
df_master <- readr::read_csv(here("data", "processed", "df_master.csv"),
                            show_col_types = FALSE)

# Create output folders
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)

# ---------------------------
# 1) Standardize values into short codes
# ---------------------------
extract_code <- function(x) {
  x <- as.character(x)
  x <- str_trim(x)
  x <- toupper(x)
  code <- ifelse(str_detect(x, "\\("),
                 str_match(x, "\\(([^)]+)\\)")[, 2],
                 x)
  str_trim(code)
}

df_master_std <- df_master %>%
  mutate(
    Sex = as.character(Sex),
    Substrate_Type   = extract_code(Substrate_Type),
    Substrate_Size   = extract_code(Substrate_Size),
    Substrate_Orient = extract_code(Substrate_Orient)
  ) %>%
  mutate(
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
  )

# ---------------------------
# 2) Availability tables (A)
# ---------------------------
df_avail_type <- tibble(Substrate_Type = c("B","F"), A = c(0.7161, 0.2839))
df_avail_size <- tibble(Substrate_Size = c("F","S","M","L","VL"),
                        A = c(0.2198, 0.2747, 0.0934, 0.1648, 0.2473))
df_avail_orient <- tibble(Substrate_Orient = c("H","O","S","V"),
                          A = c(0.2007, 0.3366, 0.2669, 0.1958))

# ---------------------------
# 3) Remove wire from use data
# ---------------------------
df_use_base <- df_master_std %>%
  filter(is.na(Substrate_Type) | Substrate_Type != "W")

# ---------------------------
# 4) Report unmatched values (and drop them)
# ---------------------------
unmatched_type <- df_use_base %>%
  filter(!is.na(Substrate_Type), Substrate_Type != "") %>%
  distinct(Substrate_Type) %>%
  anti_join(df_avail_type %>% distinct(Substrate_Type), by = "Substrate_Type")

unmatched_size <- df_use_base %>%
  filter(!is.na(Substrate_Size), Substrate_Size != "") %>%
  distinct(Substrate_Size) %>%
  anti_join(df_avail_size %>% distinct(Substrate_Size), by = "Substrate_Size")

unmatched_orient <- df_use_base %>%
  filter(!is.na(Substrate_Orient), Substrate_Orient != "") %>%
  distinct(Substrate_Orient) %>%
  anti_join(df_avail_orient %>% distinct(Substrate_Orient), by = "Substrate_Orient")

readr::write_csv(unmatched_type,   here("outputs","tables","jacobs_unmatched_type.csv"))
readr::write_csv(unmatched_size,   here("outputs","tables","jacobs_unmatched_size.csv"))
readr::write_csv(unmatched_orient, here("outputs","tables","jacobs_unmatched_orient.csv"))

df_use_base <- df_use_base %>%
  filter(is.na(Substrate_Type)   | Substrate_Type   %in% df_avail_type$Substrate_Type) %>%
  filter(is.na(Substrate_Size)   | Substrate_Size   %in% df_avail_size$Substrate_Size) %>%
  filter(is.na(Substrate_Orient) | Substrate_Orient %in% df_avail_orient$Substrate_Orient)

# ---------------------------
# 5) Compute use proportions U (returns Category)
# ---------------------------
use_prop <- function(df, col) {
  df %>%
    filter(!is.na(.data[[col]]), .data[[col]] != "") %>%
    count(Sex, Category = .data[[col]], name = "Count") %>%
    group_by(Sex) %>%
    mutate(U = Count / sum(Count)) %>%
    ungroup()
}

use_type <- use_prop(df_use_base, "Substrate_Type")   %>% rename(Substrate_Type = Category)
use_size <- use_prop(df_use_base, "Substrate_Size")   %>% rename(Substrate_Size = Category)
use_incl <- use_prop(df_use_base, "Substrate_Orient") %>% rename(Substrate_Orient = Category)

# ---------------------------
# 6) Compute Jacobs D
# ---------------------------
compute_J <- function(U, A) (U - A) / (U + A - 2 * U * A)

df_Jacobs_type <- use_type %>%
  complete(Sex, Substrate_Type, fill = list(Count = 0, U = 0)) %>%
  left_join(df_avail_type, by = "Substrate_Type") %>%
  mutate(J = compute_J(U, A))

df_Jacobs_size <- use_size %>%
  complete(Sex, Substrate_Size, fill = list(Count = 0, U = 0)) %>%
  left_join(df_avail_size, by = "Substrate_Size") %>%
  mutate(J = compute_J(U, A))

df_Jacobs_incl <- use_incl %>%
  complete(Sex, Substrate_Orient, fill = list(Count = 0, U = 0)) %>%
  left_join(df_avail_orient, by = "Substrate_Orient") %>%
  mutate(J = compute_J(U, A))

# Save tables
readr::write_csv(df_Jacobs_type, here("outputs","tables","jacobs_type.csv"))
readr::write_csv(df_Jacobs_size, here("outputs","tables","jacobs_size.csv"))
readr::write_csv(df_Jacobs_incl, here("outputs","tables","jacobs_inclination.csv"))

# ---------------------------
# 7) Order + labels
# ---------------------------
df_Jacobs_size <- df_Jacobs_size %>%
  mutate(Substrate_Size = factor(Substrate_Size,
                                 levels = c("F","S","M","L","VL"),
                                 labels = c("Fine","Small","Medium","Large","Very Large")))

df_Jacobs_incl <- df_Jacobs_incl %>%
  mutate(Substrate_Orient = factor(Substrate_Orient,
                                   levels = c("H","O","S","V"),
                                   labels = c("Horizontal","Oblique","Steep","Vertical")))

df_Jacobs_type <- df_Jacobs_type %>%
  mutate(Substrate_Type = factor(Substrate_Type,
                                 levels = c("F","B"),
                                 labels = c("Floor","Branch")))

# ---------------------------
# 8) Plot function + combine
# ---------------------------
SEX_COLORS <- c("Female" = "#1F77B4", "Male" = "#FF7F0E")

jacobs_line_plot <- function(df, xvar, title) {
  ggplot(df, aes(x = {{xvar}}, y = J, color = Sex, group = Sex)) +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    scale_color_manual(values = SEX_COLORS) +
    scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
    labs(title = title, x = NULL, y = "Jacobs' Index (D)", color = NULL) +
    theme_classic(base_family = "serif") +
    theme(plot.title = element_text(face = "bold", size = 12, hjust = 0.5),
          legend.position = "bottom")
}

plot_J_size <- jacobs_line_plot(df_Jacobs_size, Substrate_Size, "Preference: Size")
plot_J_incl <- jacobs_line_plot(df_Jacobs_incl, Substrate_Orient, "Preference: Inclination")
plot_J_type <- jacobs_line_plot(df_Jacobs_type, Substrate_Type, "Preference: Type")

final_plot <- (plot_J_size | plot_J_incl | plot_J_type) +
  plot_annotation(
    title = "SUBSTRATE PREFERENCE (Jacobs' Index)",
    theme = theme(
      plot.title = element_text(face = "bold", size = 16, family = "serif", hjust = 0.5)
    )
  ) &
  theme(
    text = element_text(family = "serif"),
    axis.text.x = element_text(size = 9),
    legend.position = "bottom"
  )

print(final_plot)

ggsave(
  filename = here("outputs","figures","jacobs_index_all.png"),
  plot = final_plot,
  width = 18, height = 6, dpi = 300
)

message("Saved figure to outputs/figures/jacobs_index_all.png")
