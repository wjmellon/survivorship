# Load required libraries
library(dplyr)
library(Rage)
library(tibble)

data <- read.csv("records.csv")

# Step 1: Filter dataset
filtered_data <- data %>%
  filter(Class %in% c("Mammalia", "Reptilia", "Amphibia", "Aves")) %>%
  filter(age_months > 0) %>%
  group_by(Species) %>%
  filter(n() >= 20) %>%
  ungroup()

# Step 2: Calculate neoplasia and cancer prevalence per species
prevalence_data <- filtered_data %>%
  group_by(Species, Class) %>%
  summarise(
    n = n(),  # Number of individuals per species
    neoplasia_cases = sum(Malignant >= 0, na.rm = TRUE),   # 0 & 1 = neoplasia
    neoplasia_prevalence = neoplasia_cases / n,
    cancer_cases = sum(Malignant == 1, na.rm = TRUE),      # 1 = malignant cancer
    cancer_prevalence = cancer_cases / n,
    max_longevity = max(age_months, na.rm = TRUE),         # needed for relative age
    .groups = "drop"
  )

# Step 3: Calculate survivorship type for each species
survival_metrics <- filtered_data %>%
  group_by(Species) %>%
  group_map(~{
    df <- .x
    species_name <- .y$Species
    
    # Compute relative age (0-1) for this species
    df <- df %>%
      mutate(relative_age = age_months / max(age_months, na.rm = TRUE))
    
    # Define relative age steps
    time_steps <- seq(0, 1, by = 0.01)
    
    # Count individuals alive at each relative age
    alive_counts <- sapply(time_steps, function(x) sum(df$relative_age >= x))
    
    # Proportion alive (lx)
    lx <- alive_counts / max(alive_counts)
    
    # Compute shape_surv
    shape_type <- shape_surv(lx)
    
    # Convert to categorical type
    surv_type <- case_when(
      shape_type >= 0.3 ~ "Type I",
      shape_type >= 0.1 & shape_type < 0.3 ~ "Trending toward Type I",
      shape_type > -0.1 & shape_type < 0.1 ~ "Type II",
      shape_type > -0.3 & shape_type <= -0.1 ~ "Trending toward Type III",
      shape_type <= -0.3 ~ "Type III"
    )
    
    tibble(
      Species = species_name,
      survivorship_type = surv_type,
      shape_value = shape_type
    )
  }) %>%
  bind_rows()

# Step 4: Merge prevalence and survival metrics by Species
condensed_data <- prevalence_data %>%
  left_join(survival_metrics, by = "Species") %>%
  select(
    Species,
    Class,
    n,
    neoplasia_prevalence,
    cancer_prevalence,
    survivorship_type,
    shape_value 
  )

# Step 5: Inspect the combined data (optional)
# View(condensed_data)

view(condensed_data)

# Step 6: Save CSV in "Spring 2025" folder with new name
# Ensure the folder exists
if (!dir.exists("Spring 2025")) {
  dir.create("Spring 2025")
}

write.csv(condensed_data, file = "Spring 2025/survivorshipcurve_labeled_eachspecies.csv", row.names = FALSE)
