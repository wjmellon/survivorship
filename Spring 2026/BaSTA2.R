# ============================================================================ #
# COMMENTS: Example code to format lifespan data to BaSTA table.
#           Assumptions: 
#              1.- Your data table is called "myData" (you can change the name 
#                  in the code below)
#              2.- the data consists of individual records including a column 
#                  for lifespans (i.e., age at death in years) and a column 
#                  for cause of death (i.e., cancer vs other) named "cause"
#                  (you can change this in the code). 
# ============================================================================ #

library(dplyr)
library(BaSTA)

# 1. Load data with a safer name
data <- read.csv("Spring 2026/BaSTA_birth_dates.csv")

# 2. Get a list of all unique species
species_list <- unique(data$Species.x) # Make sure "Species" matches your column name exactly

# 3. Create an empty list to store results
all_results <- list()

# 4. Start the Loop
for (sp in species_list) {
  
  cat("\n--- Running Analysis for:", sp, "---\n")
  
  # Filter data for just this species
  sp_subset <- data %>% filter(Species.x == sp)
  
  # Number of individuals for this species
  n_sp <- nrow(sp_subset)
  
  # Check data for this species
  # (Crucial: BaSTA needs a minimum amount of data to converge)
  check <- DataCheck(object = sp_subset, dataType = "census")
  
  # Reconstruct the bastaDat for this specific species
  bastaDat_sp <- data.frame(
    ID             = 1:n_sp, 
    Birth.Date     = sp_subset$birth_date, 
    Min.Birth.Date = sp_subset$birth_date,
    Max.Birth.Date = sp_subset$birth_date,
    Entry.Date     = sp_subset$birth_date,
    Depart.Date    = sp_subset$necropsy_date,
    Depart.Type    = rep("D", n_sp),
    cause          = sp_subset$Malignant.x
  )
  
  # Run BaSTA (wrapped in 'try' so one species failure doesn't stop the whole loop)
  out <- try(basta(
    object = bastaDat_sp, 
    dataType = "census", 
    model = "GO", 
    shape = "bathtub", 
    formulaMort = ~ cause - 1,
    parallel = TRUE, 
    ncpus = 4, 
    nsim = 4
  ))
  
  # Store result and plot
  if (!inherits(out, "try-error")) {
    all_results[[sp]] <- out
    plot(out, type = "demorates")
    title(main = paste("Species:", sp))
    
    # Check if 'out' was successful
    if (!inherits(out, "try-error")) {
      
      # 2. Extract the coefficient table from the BaSTA object
      # This usually pulls the mean, SD, and credible intervals
      stats_table <- as.data.frame(out$coefficients)
      
      sp_summary <- data.frame(
        Species = sp,
        a1 = stats_table["a0", "Mean"],
        b1 = stats_table["a1", "Mean"],
        a2 = stats_table["c",  "Mean"],
        a3 = stats_table["b0", "Mean"],
        b3 = stats_table["b1", "Mean"]
      )
      
      all_results[[sp]] <- sp_summary
    }
  }
   }

summary(all_results)

final_results_df <- do.call(rbind, all_results)


# View summary for a specific species later:
summary(all_results[["Cebuella pygmaea"]])


# 6. Save to a CSV file
write.csv(final_results_df, "BaSTA_Results_All_Species.csv", row.names = FALSE)

print("Final dataset created!")

