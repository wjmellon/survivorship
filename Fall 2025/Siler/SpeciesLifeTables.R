library(dplyr)
library(readr)

# Load datasets
individual_data <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")
species_data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_mortality_data.csv")


make_life_table <- function(species_name, individual_data, species_data) {
  
  # Filter individual data by species
  df <- individual_data %>% filter(Species == species_name)
  
  # Get max age for this species
  max_age <- species_data %>% 
    filter(Species == species_name) %>% 
    pull(max_longevity)
  
  # Create 10 equal intervals
  breaks <- seq(0, max_age, length.out = 11)  # 10 intervals → 11 endpoints
  
  # Count deaths in each interval
  deaths <- hist(df$age_months, breaks = breaks, plot = FALSE)$counts
  
  # l(x): number at start of each interval
  l_x <- numeric(10)
  l_x[1] <- nrow(df)  # total individuals at age 0
  
  for (i in 2:10) {
    l_x[i] <- l_x[i-1] - deaths[i-1]
  }
  
  # d(x): deaths in interval
  d_x <- deaths
  
  # q(x): probability of death
  q_x <- ifelse(l_x > 0, d_x / l_x, NA)
  
  # Build the life table
  data.frame(
    Species = species_name,
    Interval_Start = breaks[-11],
    Interval_End   = breaks[-1],
    l_x = l_x,
    d_x = d_x,
    q_x = q_x
  )
}

species_list <- species_data$Species

life_tables <- lapply(species_list, function(sp) {
  make_life_table(sp, individual_data, species_data)
})

# Combine into one dataframe
life_tables_all <- do.call(rbind, life_tables)

view(life_tables_all)

# Save result
write.csv(life_tables_all, "life_tables_all_species.csv", row.names = FALSE)


