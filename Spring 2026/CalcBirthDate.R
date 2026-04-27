library(dplyr)
library(tibble)
library(ggplot2)
library(lubridate)

# Load individual data 
individual_data <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")

# Load min 20 species data
species_data <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")

# Filter for individuals that are in min 20 species data 
filtered_individuals <- individual_data %>%
  semi_join(species_data, by = "Species")

# Filter out individuals with odd date formatting
filtered_individuals <- filtered_individuals %>%
  filter(grepl("^\\d{1,2}-[A-Za-z]{3}$", Date))

# Calculate birth date from necropsy date 
individual_birth_dates <- filtered_individuals %>%
  mutate(necropsy_date = dmy(paste(Date, Year))) %>%
  mutate(birth_date = necropsy_date %m-% months(round(age_months))) %>%
  mutate(depart_type = case_when(
    Malignant %in% c(0, -1) ~ "C",
    Malignant == 1          ~ "D",
    TRUE                     ~ NA_character_ # Handles any other unexpected values
  ))
  
# Join 2 data sets 
filtered_individuals <- filtered_individuals %>%
  left_join(individual_birth_dates, by = "ID") %>%
  select(
    ID,
    age_months.x,
    Date.x, 
    Year.x,
    Class.x,
    Species.x,
    common_name.x,
    birth_date,
    necropsy_date,
    Malignant.x,
    depart_type
  )

write.csv(filtered_individuals, "BaSTA_birth_dates.csv", row.names = FALSE)
