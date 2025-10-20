# Load libraries
library(ggplot2)
library(dplyr)


data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_mortality_data.csv")


# Scatterplot with linear regression line
ggplot(data, aes(x = shape_value, y = ZIMS_Cancer_Mortality, color = Class)) +
  geom_point(alpha = 1, size = 1) +  # points
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # regression line
  theme_minimal() +
  labs(
    x = "Survivorship",
    y = "Cancer Mortality (%)",
    color = "Class", 
    title = "Survivorship vs. Cancer Mortality"
  ) +
  theme(legend.position = "bottom") 


# Run the linear model
model <- lm(ZIMS_Cancer_Mortality ~ shape_value, data = data)

# See results
summary(model)
