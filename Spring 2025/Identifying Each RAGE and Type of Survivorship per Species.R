library(ggplot2)
library(dplyr)
library(survival)
library(survminer)
library(devtools)
install_github("jonesor/rage")
library(Rage)

# Load the dataset
data <- read.csv("records.csv")

# ------------------------------
# Function to calculate survivorship type and RAGE number
# ------------------------------
calculate_rage_type <- function(df) {
  n_ind <- nrow(df) # sample size
  if (n_ind < 2) {
    return(list(RAGE_number = NA, Surv_type = NA_character_, N = n_ind))
  }
  
  relative_age_vector <- df %>%
    select(age_months, max_longevity) %>%
    mutate(relative_age = age_months / max_longevity) %>%
    filter(!is.na(relative_age) & relative_age >= 0 & relative_age <= 1) %>%
    pull(relative_age)
  
  if (length(relative_age_vector) < 2) {
    return(list(RAGE_number = NA, Surv_type = NA_character_, N = n_ind))
  }
  
  time_steps <- seq(0, 1, by = 0.01)
  alive_counts <- sapply(time_steps, function(x) {
    sum(relative_age_vector > x, na.rm = TRUE)
  })
  lx_raw <- alive_counts / max(alive_counts, na.rm = TRUE)
  lx <- ifelse(lx_raw == 0, min(lx_raw[lx_raw > 0], na.rm = TRUE) / 2, lx_raw)
  
  if (all(is.na(lx)) || any(!is.finite(lx))) {
    return(list(RAGE_number = NA, Surv_type = NA_character_, N = n_ind))
  }
  
  shape_type <- tryCatch({
    shape_surv(lx)
  }, error = function(e) {
    message(paste("Error in shape_surv:", e$message))
    return(NA)
  })
  
  surv_type <- case_when(
    !is.na(shape_type) & shape_type >= 0.3 ~ "Type I",
    !is.na(shape_type) & shape_type >= 0.1 & shape_type < 0.3 ~ "Trending I",
    !is.na(shape_type) & shape_type > -0.1 & shape_type < 0.1 ~ "Type II",
    !is.na(shape_type) & shape_type > -0.3 & shape_type <= -0.1 ~ "Trending III",
    !is.na(shape_type) & shape_type <= -0.3 ~ "Type III",
    TRUE ~ NA_character_
  )
  
  return(list(RAGE_number = shape_type, Surv_type = surv_type, N = n_ind))
}

# ------------------------------
# Loop through species
# ------------------------------
unique_species <- unique(data$Species)
surv_type_data_species <- list()

for (sp in unique_species) {
  species_data_filtered <- data %>%
    filter(Necropsy == 1, Infant == 0, Species == sp, age_months != -1)
  
  species_row <- list(Species = sp)
  
  # Calculate survivorship for each group
  res_normal   <- calculate_rage_type(species_data_filtered)
  res_malign   <- calculate_rage_type(species_data_filtered %>% filter(Malignant == 1))
  res_benign   <- calculate_rage_type(species_data_filtered %>% filter(Malignant == 0))
  res_neoplas  <- calculate_rage_type(species_data_filtered %>% filter(Malignant %in% c(0, 1)))
  
  # Store results
  species_row$Normal_RAGE     <- res_normal$RAGE_number
  species_row$Normal_Type     <- res_normal$Surv_type
  species_row$Normal_N        <- res_normal$N
  
  species_row$Malignant_RAGE  <- res_malign$RAGE_number
  species_row$Malignant_Type  <- res_malign$Surv_type
  species_row$Malignant_N     <- res_malign$N
  
  species_row$Benign_RAGE     <- res_benign$RAGE_number
  species_row$Benign_Type     <- res_benign$Surv_type
  species_row$Benign_N        <- res_benign$N
  
  species_row$Neoplasia_RAGE  <- res_neoplas$RAGE_number
  species_row$Neoplasia_Type  <- res_neoplas$Surv_type
  species_row$Neoplasia_N     <- res_neoplas$N
  
  surv_type_data_species[[sp]] <- species_row
}

# ------------------------------
# Combine into a table
# ------------------------------
surv_type_table_species <- bind_rows(surv_type_data_species)

# ------------------------------
# Save to CSV
# ------------------------------
write.csv(surv_type_table_species, "species_survivorship.csv", row.names = FALSE)

print("✅ Survivorship table saved as 'species_survivorship.csv'")
