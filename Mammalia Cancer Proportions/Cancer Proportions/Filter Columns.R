# Load necessary libraries
library(dplyr)
library(ggplot2)

data <- read.csv("records.csv")

# Filter data for Mammalia and calculate the proportion of melanoma
species_melanoma <- data %>%
  filter(Class == "Mammalia") %>%
  group_by(source_name) %>%
  summarise(melanoma_prop = mean(Type == "melanoma"))

# Plot
ggplot(species_melanoma, aes(x = source_name, y = melanoma_prop)) +
  geom_point() +
  labs(x = "Species", y = "Proportion of Melanoma") +

