# ============================================================
# analysis/02_pooling_check.R
# Check whether female individuals can be pooled
# Example: Chi-square test on locomotor mode distributions
#
# INPUT:
#   data/processed/df_master.csv
#
# OUTPUT:
#   outputs/tables/female_pooling_loco_mode_contingency.csv
#   outputs/tables/female_pooling_loco_mode_chisq.txt
# ============================================================

library(tidyverse)
library(here)

# 1) Load processed master dataset
df_master <- readr::read_csv(here("data", "processed", "df_master.csv"),
                            show_col_types = FALSE)

#2) Filter only the two females
df_females <- df_master %>%
  filter(Sex == "Female", Subject %in% c("Nefertiti", "Ritsa")) %>%
  mutate(
    Subject = as.character(Subject),
    Loco_Mode = str_trim(as.character(Loco_Mode))
  ) %>%
  filter(!is.na(Loco_Mode), Loco_Mode != "", Loco_Mode != "NA")

#3) Build contingency table
contingency_table <- table(df_females$Subject, df_females$Loco_Mode)

# Print to console
cat("\nContingency Table (Loco_Mode counts):\n")
print(contingency_table)

# Save table to file
dir.create(here("outputs", "tables"), recursive = TRUE, showWarnings = FALSE)
write.csv(as.data.frame.matrix(contingency_table),
          here("outputs", "tables", "female_pooling_loco_mode_contingency.csv"))

#4) Chi-square test (with informative warning handling)
chisq_result <- chisq.test(contingency_table)

cat("\nChi-square test result:\n")
print(chisq_result)

# Save result to text file
sink(here("outputs", "tables", "female_pooling_loco_mode_chisq.txt"))
cat("Chi-square test: Nefertiti vs Ritsa on Loco_Mode\n\n")
print(chisq_result)
cat("\nExpected counts (check small cells):\n")
print(chisq_result$expected)
sink()

# Optional: flag if expected counts are low
if (any(chisq_result$expected < 5)) {
  warning("Some expected counts are < 5. Consider collapsing rare categories or using Fisher's exact test.")
}

message("Saved outputs to outputs/tables/")
