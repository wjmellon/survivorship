# =========================================================================
# Neoplasia & malignancy prevalence per species -> Excel
#
# Malignant column coding:
#   -1 = no tumor
#    0 = benign tumor
#    1 = malignant tumor
#
# Neoplasia prevalence  = animals with Malignant 0 or 1 / n
# Malignancy prevalence = animals with Malignant 1      / n
# =========================================================================

library(dplyr)
library(writexl)  

# ---- Settings -----------------------------------------------------------
input_file  <- "Fall 2025/Filtering Data/cleanPath.min20.062822.csv"  # change if needed
output_file <- "species_prevalence.xlsx"

only_necropsies <- TRUE   # keep only Necropsy == 1
exclude_infants <- TRUE   # drop Infant == 1
min_n           <- 20      

# ---- Read data ----------------------------------------------------------
# read.csv for .csv; if your file is tab-separated use read.delim instead
raw <- read.csv(input_file, stringsAsFactors = FALSE)
cat("Rows read:", nrow(raw), "\n")

df <- raw
if (only_necropsies) df <- df %>% filter(Necropsy == 1)
if (exclude_infants) df <- df %>% filter(Infant != 1 | is.na(Infant))
df <- df %>% filter(!is.na(Species), Species != "", !is.na(Malignant))
cat("Rows after filters:", nrow(df), "\n")

# ---- One row per animal -------------------------------------------------
# An animal can show up on multiple rows (e.g. more than one tumor).
# Collapse by ID so each animal is counted once, keeping its "worst" result:
#   any malignant -> 1, else any benign -> 0, else -1
animals <- df %>%
  mutate(ID = ifelse(is.na(ID), paste0("row", row_number()), as.character(ID))) %>%
  group_by(Species, ID) %>%
  summarise(
    Class     = first(Class),
    Malignant = max(Malignant, na.rm = TRUE),
    .groups   = "drop"
  )

dupes <- nrow(df) - nrow(animals)
if (dupes > 0) cat("Collapsed", dupes, "duplicate rows (same animal ID)\n")

# ---- Prevalence per species ---------------------------------------------
prevalence <- animals %>%
  group_by(Species) %>%
  summarise(
    Class                 = first(Class),
    n                     = n(),
    neoplasia_count       = sum(Malignant %in% c(0, 1)),
    malignancy_count      = sum(Malignant == 1),
    neoplasia_prevalence  = neoplasia_count / n,
    malignancy_prevalence = malignancy_count / n,
    .groups = "drop"
  ) %>%
  filter(n >= min_n) %>%
  arrange(Species)

cat("Species in output:", nrow(prevalence), "\n")
print(head(prevalence, 10))

# ---- Write Excel --------------------------------------------------------
write_xlsx(prevalence, output_file)
cat("Saved:", output_file, "\n")