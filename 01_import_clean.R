# ============================================================
# analysis/01_import_clean.R
# Import + clean dormouse behavior observation files
#
# INPUT:
#   data/raw/Ιάσονας (Male 1).xlsx
#   data/raw/Νεφερτίτη (Female 1).xlsx
#   data/raw/Ρίτσα (Female 2).xlsx
#
# OUTPUT:
#   data/processed/df_master.csv
# ============================================================

library(readxl)
library(tidyverse)
library(janitor)
library(here)

#1) Column types (prevents mixed-type errors)
col_spec <- c(
  "text",    # video number
  "text",    # Time (sec)
  rep("text", 15)
)

#2) Helper function: read + add metadata + clean
read_and_clean <- function(path, sheet, subject, sex) {

  if (!file.exists(path)) {
    stop("File not found: ", path)
  }

  df_raw <- read_excel(path, sheet = sheet, col_types = col_spec) %>%
    janitor::clean_names() %>%
    mutate(Subject = subject, Sex = sex)

  # Safety check: ensure expected number of columns exist
  if (ncol(df_raw) < 9) {
    stop("Expected at least 9 columns but got ", ncol(df_raw), " in: ", path)
  }

  df_clean <- df_raw %>%
    transmute(
      Behavior         = df_raw[[3]],
      Loco_Post        = df_raw[[5]],
      Loco_Mode        = df_raw[[6]],
      Substrate_Type   = df_raw[[7]],
      Substrate_Size   = df_raw[[8]],
      Substrate_Orient = df_raw[[9]],
      Subject,
      Sex
    ) %>%
    mutate(
      # Trim whitespace everywhere (prevents join/plot issues later)
      across(c(Behavior, Loco_Post, Loco_Mode,
               Substrate_Type, Substrate_Size, Substrate_Orient),
             ~ str_trim(as.character(.x)))
    )

  return(df_clean)
}

#3) Path
iasonas_path   <- here("data", "raw", "Ιάσονας (Male 1).xlsx")
nefertiti_path <- here("data", "raw", "Νεφερτίτη (Female 1).xlsx")
ritsa_path     <- here("data", "raw", "Ρίτσα (Female 2).xlsx")

#4) Read + clean each file
iasonas_clean <- read_and_clean(iasonas_path,   sheet = "Ιάσονας",   subject = "Iasonas",   sex = "Male")
nefertiti_clean <- read_and_clean(nefertiti_path, sheet = "Νεφερτίτη", subject = "Nefertiti", sex = "Female")
ritsa_clean <- read_and_clean(ritsa_path,     sheet = "Ρίτσα",     subject = "Ritsa",     sex = "Female")

#5) Combine into master dataset
df_master <- bind_rows(iasonas_clean, nefertiti_clean, ritsa_clean) %>%
  mutate(
    Sex = factor(Sex, levels = c("Female", "Male")),
    Subject = factor(Subject)
  )

# 6) Save processed output
dir.create(here("data", "processed"), recursive = TRUE, showWarnings = FALSE)
write_csv(df_master, here("data", "processed", "df_master.csv"))

#7) Quick checks
message("Saved: ", here("data", "processed", "df_master.csv"))
message("Total rows: ", nrow(df_master))
print(count(df_master, Sex, Subject))

glimpse(df_master)
