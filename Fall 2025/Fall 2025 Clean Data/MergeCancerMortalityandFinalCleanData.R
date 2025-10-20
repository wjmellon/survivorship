library(readxl)
library(dplyr)

# Read your data
final_clean_data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_data.csv")        # Has Species, Rage, and other columns
cancer_mortality_data <- read_excel("Survivorship Species_10.10.25.xlsx")    # Has Species, CancerMortality

mortality_selected <- cancer_mortality_data %>%
  select(Species, `ZIMS.cancer.mortality(Jan10th.2010-April.29th.2024)`)

# Merge/Join the cancer mortality column into the rage_data sheet
merged_mortality_data <- final_clean_data %>%
  left_join(mortality_selected, by = "Species")   # Adds CancerMortality column to matching Species

final_clean_mortality_data <- merged_mortality_data %>%
  rename(ZIMS_Cancer_Mortality = `ZIMS.cancer.mortality(Jan10th.2010-April.29th.2024)`)


# View the updated dataset
View(final_clean_mortality_data)

write.csv(final_clean_mortality_data, "Fall 2025/Fall 2025 Clean Data/final_clean_mortality_data.csv", row.names = FALSE)

