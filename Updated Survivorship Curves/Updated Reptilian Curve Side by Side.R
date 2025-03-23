# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Load the dataset
data <- read.csv("records.csv")

# Filter the dataset for Reptilia with Necropsy data and exclude infants
cutdata_reptilia <- data %>% 
  filter(Necropsy == 1) %>% 
  filter(Class == "Reptilia") %>% 
  filter(Infant == 0)

# Select relevant columns (Species, age_months, max_longevity, Malignancy)
cutdata_reptilia <- cutdata_reptilia[, c(3, 28, 48, 18)]  # Adjust column indices based on your data
colnames(cutdata_reptilia) <- c("age_months", "Species", "max_longevity", "Malignant")

# Clean data: remove individuals with invalid ages and longevity
cutdata_reptilia$age_months[cutdata_reptilia$age_months <= 0] <- NA
cutdata_reptilia$max_longevity[cutdata_reptilia$max_longevity <= 0] <- NA
cutdata_reptilia <- na.omit(cutdata_reptilia)

# Create a relative_age column: individual age divided by species-specific maximum longevity
cutdata_reptilia <- cutdata_reptilia %>% 
  mutate(relative_age = age_months / max_longevity)

# Define relative age time steps (0 to 1 by increments of 0.01)
time_steps <- seq(0, 1, by = 0.01)  # 1% increments of lifespan

# Filter for malignant and benign groups based on Malignancy column
malignant_reptilia <- cutdata_reptilia %>% filter(Malignant == 1)
benign_reptilia <- cutdata_reptilia %>% filter(Malignant == 0)
all_reptilia <- cutdata_reptilia  # All reptiles (with or without malignancy)

# Calculate survival for each group (number of individuals alive at each time step)
malignant_alive_reptilia <- sapply(time_steps, function(x) sum(malignant_reptilia$relative_age > x))
benign_alive_reptilia <- sapply(time_steps, function(x) sum(benign_reptilia$relative_age > x))
all_reptilia_alive <- sapply(time_steps, function(x) sum(all_reptilia$relative_age > x))

# Create a data frame for plotting
alive_data_reptilia <- data.frame(
  relative_age = time_steps,
  proportion_alive_malignant = malignant_alive_reptilia / max(malignant_alive_reptilia),
  proportion_alive_benign = benign_alive_reptilia / max(benign_alive_reptilia),
  proportion_alive_all = all_reptilia_alive / max(all_reptilia_alive)
)

# Plot smoothed survivorship curves for all, malignant, and benign (for Reptilia)
ggplot(alive_data_reptilia, aes(x = relative_age)) +
  geom_smooth(aes(y = proportion_alive_all, color = "All Reptilia"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_malignant, color = "Malignant"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_benign, color = "Benign"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  scale_color_manual(values = c(
    "All Reptilia" = "blue",
    "Malignant" = "red",
    "Benign" = "green"
  )) +
  ggtitle("Smoothed Survivorship Curve: All Reptilia vs. Malignant vs. Benign") +
  xlab("Proportion of Maximum Lifespan") +
  ylab("Proportion Alive (log scale)") +
  scale_y_log10() +
  theme_cowplot(12)
