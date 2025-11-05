library(dplyr)
library(fmsb)
library(readr)

# Load your life table
df <- read_csv("Fall 2025/Siler/life_tables_all_species.csv")

# Make sure your column names are correct
# They must include: Species and q_x
names(df)

# Get unique species list
species_list <- unique(df$Species)

# Create an empty list to store results
results_list <- list()

# Loop through each species
for (sp in species_list) {
  cat("Fitting species:", sp, "...\n")
  
  # Filter data for this species
  df_sp <- df %>% filter(Species == sp)
  
  # Ensure q_x is valid (no NA, between 0 and 1)
  df_sp <- df_sp %>% filter(!is.na(q_x), q_x >= 0, q_x <= 1)
  
  # Skip if not enough intervals
  if (nrow(df_sp) < 5) {
    results_list[[sp]] <- data.frame(
      Species = sp, a = NA, b = NA, c = NA, d = NA, e = NA, FLAG = NA
    )
    next
  }
  
  # First attempt with starting parameters
  res <- tryCatch({
    fitSiler(c(0.1, 0.1, 0.01, 0.1, 0.01), df_sp$q_x)
  }, error = function(e) { return(NULL) })
  
  # Skip if the fit fails
  if (is.null(res)) {
    results_list[[sp]] <- data.frame(
      Species = sp, a = NA, b = NA, c = NA, d = NA, e = NA, FLAG = NA
    )
    next
  }
  
  # Extract FLAG and retry until convergence
  FLAG <- res[7]
  attempt <- 0
  
  while (FLAG > 0 && attempt < 10) {
    res <- fitSiler(res[1:5], df_sp$q_x)
    FLAG <- res[7]
    attempt <- attempt + 1
  }
  
  # Store parameters in a data frame
  results_list[[sp]] <- data.frame(
    Species = sp,
    a1 = res[1],
    b1 = res[2],
    a2 = res[3],
    a3 = res[4],
    b3 = res[5],
    FLAG = res[7]
  )
  
  cat("   Done:", sp, "- FLAG =", FLAG, "\n")
}

# Combine all species results into one data frame
siler_results <- do.call(rbind, results_list)

# View the final parameter table
View(siler_results)


# Merge them by Species
merged_df <- siler_results %>%
  left_join(final_clean_mortality_data %>% select(Species, Class, cancer_prevalence, neoplasia_prevalence),
            by = "Species")

View (merged_df)

# Optionally save to a CSV
write_csv(merged_df, "Fall 2025/Siler/siler_parameters_all_species.csv")

