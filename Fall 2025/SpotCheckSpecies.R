library(dplyr)

spot_check_data <- read_csv("Fall 2025/Fall 2025 Clean Data/final_clean_mortality_data.csv")

# Define your target points
targets <- c(-0.5, 0, 0.5)

# Function to get the 5 species closest to a target value
closest_to <- function(spot_check_data, target, n = 5) {
  spot_check_data %>%
    mutate(distance = abs(shape_value - target)) %>%
    arrange(distance) %>%
    slice_head(n = n)
}

# Get lists
closest_type_3 <- closest_to(spot_check_data, -0.5, n = 5)
closest_type_2     <- closest_to(spot_check_data, 0, n = 5)
closest_type_1 <- closest_to(spot_check_data, 0.5, n = 5)

View(closest_type_3)
View(closest_type_2)
View(closest_type_1)