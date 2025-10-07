library(ggplot2)
library(dplyr)
library(tidyr)
library(Rage)

# Load data
data <- read.csv("records.csv")
table_data <- read.csv("Updated Survivorship Curves/Species Survivorship Curve Analysis/DataSheet w All Species Survivorship Curves - Sheet1.csv", skip = 1, header = TRUE)

# 1. Calculate Neoplasia Prevalence
prevalence_data <- data %>%
  filter(!is.na(Malignant)) %>%
  group_by(Species) %>%
  summarise(
    Total = n(),
    Malignant = sum(Malignant == 1),
    Benign = sum(Malignant == 0),
    Malignant_Prev = Malignant / Total,
    Neoplasia_Prev = (Malignant + Benign) / Total,
    .groups = 'drop'
  )

# 2. Calculate RAGE number
calculate_rage <- function(species_name) {
  species_data <- data %>% filter(Species == species_name, Necropsy == 1, Infant == 0)
  
  if(nrow(species_data) < 10){
    return(NA)
  }
  
  species_data <- species_data %>%
    mutate(
      age_months = as.numeric(age_months),
      max_longevity = as.numeric(max_longevity)
    )
  
  species_data <- species_data[, c(3, 28, 48)]
  colnames(species_data) <- c("age_months", "Species", "max_longevity")
  
  species_data$age_months[species_data$age_months <= 0] <- NA
  species_data$max_longevity[species_data$max_longevity <= 0] <- NA
  species_data <- na.omit(species_data)
  
  species_data <- species_data %>%
    mutate(relative_age = age_months / max_longevity)
  
  time_steps <- seq(0, 1, by = 0.01)
  
  alive_counts <- sapply(time_steps, function(x) {
    sum(species_data$relative_age > x)
  })
  
  alive_data <- data.frame(
    relative_age = time_steps,
    count_alive = alive_counts
  ) %>% mutate(proportion_alive = count_alive / max(count_alive))
  
  lx <- alive_data$proportion_alive
  lx <- lx / max(lx)
  
  shape_type <- shape_surv(lx)
  return(shape_type)
}

# Get valid species
valid_species <- intersect(unique(data$Species), unique(table_data$Scientific.Name))

# Calculate RAGE number for each species
rage_values <- sapply(valid_species, calculate_rage)
names(rage_values) <- valid_species

rage_df <- data.frame(Species = names(rage_values), RAGE = rage_values)
rage_df <- rage_df %>% filter(!is.na(RAGE))

# 3. Merge Prevalence and RAGE data
merged_data_plot <- merge(prevalence_data, rage_df, by = "Species")

# Merge Class info
merged_data_plot <- merge(merged_data_plot, unique(data[, c("Species", "Class")]), by = "Species")

# 4. Add log-transformed adult weight
weight_data <- data %>%
  filter(!is.na(adult_weight)) %>%
  group_by(Species) %>%
  summarise(log_adult_weight = log10(mean(adult_weight, na.rm = TRUE)), .groups = 'drop')

# Merge weight
merged_data_plot <- merge(merged_data_plot, weight_data, by = "Species")

# 5. Linear regression
regression_model <- lm(Malignant_Prev ~ RAGE, data = merged_data_plot)
r_squared <- summary(regression_model)$r.squared
p_value <- summary(regression_model)$coefficients[2, 4]
intercept <- coef(regression_model)[1]
slope <- coef(regression_model)[2]
equation_string <- paste("Neoplasia_Prev = ", round(slope, 3), " * RAGE + ", round(intercept, 3), sep = "")

# 6. Plot
ggplot(merged_data_plot, aes(x = RAGE, y = log(Neoplasia_Prev), color = log_adult_weight)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  scale_color_gradient(low = "lightblue", high = "darkred", name = "Log Adult Weight") +
  labs(
    title = "RAGE Number vs. Neoplasia Prevalence",
    x = "RAGE Number",
    y = "Neoplasia Prevalence",
    caption = paste("R-squared = ", round(r_squared, 3), ", p-value = ", round(p_value, 3), "\n", equation_string)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.caption = element_text(hjust = 0),
    legend.position = "bottom"
  )
