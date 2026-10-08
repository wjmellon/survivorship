# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(Rage)

# Load the dataset
data <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")


# Filter the dataset for Mammals with Necropsy data and exclude infants
cutdata <- data %>%
  filter(Necropsy == 1) %>%
  filter(Infant == 0) %>%
  mutate(
    age_months = as.numeric(age_months),
    max_longevity = as.numeric(max_longevity)
  )


# Select relevant columns (assuming columns 3 = Species, 24 = age_months, 48 = max_longevity)
cutdata <- cutdata[, c(4, 29, 49)]
colnames(cutdata) <- c("age_months", "Species", "max_longevity")

cutdata <- cutdata %>%
  filter(!is.na(age_months) & age_months > 0,
         !is.na(max_longevity) & max_longevity > 0) %>%
  mutate(relative_age = age_months / max_longevity)

# Create summary table with species count >= 20
species_counts <- cutdata %>%
  group_by(Species) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 20)

# Filter main dataset to keep only those species
cutdata <- cutdata %>%
  filter(Species %in% species_counts$Species)

time_steps <- seq(0, 1, by = 0.01)

# Ensure Species is character (not factor) and extract unique values
species_list <- unique(as.character(cutdata$Species))

# Check how many species exist
cat("Total species to process:", length(species_list), "\n")

# Initialize empty list BEFORE running the loop
results_list <- list()

for (sp in species_list) {
  # Filter data for current species
  sp_data <- cutdata %>% filter(Species == sp)
  
  # Calculate alive counts across time_steps for this species
  alive_counts <- sapply(time_steps, function(x) {
    sum(sp_data$relative_age >= x)
  })
  
  # Normalize to initial population size
  lx <- alive_counts / max(alive_counts)
  
  # Calculate Rage shape metric (trunc = TRUE prevents log(0) error)
  shape_val <- shape_surv(lx, trunc = TRUE)
  
  # Determine classification
  surv_cat <- case_when(
    shape_val >= 0.3 ~ "Type I (late mortality, senescence)",
    shape_val >= 0.1 & shape_val < 0.3 ~ "Trending toward Type I",
    shape_val > -0.1 & shape_val < 0.1 ~ "Type II (constant mortality)",
    shape_val > -0.3 & shape_val <= -0.1 ~ "Trending toward Type III",
    shape_val <= -0.3 ~ "Type III (early mortality)"
  )
  
  # Print results to console
  cat("\n-----------------------------------\n")
  cat("Species:", sp, "\n")
  cat("Standardized AUC:", round(shape_val, 3), "\n")
  cat("Classification:", surv_cat, "\n")
  
  # Save dataframe row into list
  results_list[[sp]] <- data.frame(
    Species = sp,
    shape_type = round(shape_val, 3),
    surv_type = surv_cat,
    stringsAsFactors = FALSE
  )
}

# Combine all loop outputs into one complete dataframe
surv_summary <- bind_rows(results_list)

write.csv(surv_summary, "Survivorship_Types_Min20.csv", row.names = FALSE)

