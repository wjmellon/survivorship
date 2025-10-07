# Load libraries
library(ggplot2)
library(dplyr)

data <- read.csv("Fall 2025/Filtering Data/condensed_data.csv")


# Scatterplot with linear regression line
ggplot(data, aes(x = shape_value, y = cancer_prevalence, color = Class)) +
  geom_point(alpha = 1, size = 1) +  # points
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # regression line
  theme_minimal() +
  labs(
    x = "Survivorship",
    y = "Cancer Prevalence (%)",
    color = "Class", 
    title = "Survivorship vs. Cancer Prevalence"
     ) +
    theme(legend.position = "bottom") 


# Run the linear model
model <- lm(cancer_prevalence ~ shape_value, data = data)

# See results
summary(model)



