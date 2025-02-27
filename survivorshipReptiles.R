# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Load the dataset
data <- malignant_data


cutdata <- filter(data, Necropsy == 1)

# Filter for Class Reptilia and relevant columns
cutdata <- filter(cutdata, Class == "Reptilia")


cutdata <- filter(cutdata, Infant == 0)




#cutdata <- filter(cutdata, com == "Reptilia")

cutdata <- cutdata[, c(3, 24,48)]  # Assuming columns 3 and 24 are `age_months` and `max_longevity`
cutdata$age_months[cutdata$age_months <= 0] <- NA
cutdata <- na.omit(cutdata)

# Define time steps (e.g., ages to check survival at)
time_steps <- seq(0, max(cutdata$age_months, na.rm = TRUE), by = 1)  # 1-month intervals

# Calculate the number of individuals alive at each time step
alive_counts <- sapply(time_steps, function(x) {
  sum(cutdata$age_months > x)  # Count individuals with age > x
})

# Prepare a dataframe for plotting
alive_data <- data.frame(
  age = time_steps,
  count_alive = alive_counts
)
# Normalize counts to a proportion of the original population
alive_data <- alive_data %>%
  mutate(proportion_alive = count_alive / max(count_alive))  # Divide by initial population size




max_age <- max(cutdata$max_longevity, na.rm = TRUE)

#step line
ggplot(alive_data, aes(x = age, y = (proportion_alive))) +
  geom_point(color = "green", size = 2) +
  geom_line(color = "blue", size = 1) +
  ggtitle("Survivorship Curve for Reptiles with Malignancy") +
  xlab("Age (months)") +
  ylab("log(Proportion Alive)") +
  scale_y_log10() + # Set x-axis limits
  theme_cowplot(12)


#smooth line
ggplot(alive_data, aes(x = age, y = (count_alive))) +
  geom_point(color = "pink", size = 1) +  # Keep points for individual data
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", size = 1, se = FALSE) +
  ggtitle("Survivorship Curve for Reptiles with Malignancy") +
  xlab("Age (C)") +
  ylab("log(Count Alive)") +
  scale_y_log10() +  # Set x-axis limits
  theme_cowplot(12)
summary(cutdata$age_months)
head(alive_data)
tail(alive_data)
