# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Load the dataset
data <- read.csv("records.csv")

# Filter the dataset for Amphibia with Necropsy data and exclude infants
cutdata_amphibia <- data %>% 
  filter(Necropsy == 1) %>% 
  filter(Class == "Amphibia") %>% 
  filter(Infant == 0)

# Select relevant columns (Species, age_months, max_longevity, Malignancy)
cutdata_amphibia <- cutdata_amphibia[, c(3, 28, 48, 18)]  # Adjust column indices based on your data
colnames(cutdata_amphibia) <- c("age_months", "Species", "max_longevity", "Malignant")

# Clean data: remove individuals with invalid ages and longevity
cutdata_amphibia$age_months[cutdata_amphibia$age_months <= 0] <- NA
cutdata_amphibia$max_longevity[cutdata_amphibia$max_longevity <= 0] <- NA
cutdata_amphibia <- na.omit(cutdata_amphibia)

# Create a relative_age column: individual age divided by species-specific maximum longevity
cutdata_amphibia <- cutdata_amphibia %>% 
  mutate(relative_age = age_months / max_longevity)

# Define relative age time steps (0 to 1 by increments of 0.01)
time_steps <- seq(0, 1, by = 0.01)  # 1% increments of lifespan

# Filter for malignant and benign groups based on Malignancy column
malignant_amphibia <- cutdata_amphibia %>% filter(Malignant == 1)
benign_amphibia <- cutdata_amphibia %>% filter(Malignant == 0)
all_amphibia <- cutdata_amphibia  # All amphibians (with or without malignancy)

# Calculate survival for each group (number of individuals alive at each time step)
malignant_alive_amphibia <- sapply(time_steps, function(x) sum(malignant_amphibia$relative_age > x))
benign_alive_amphibia <- sapply(time_steps, function(x) sum(benign_amphibia$relative_age > x))
all_amphibia_alive <- sapply(time_steps, function(x) sum(all_amphibia$relative_age > x))

# Create a data frame for plotting
alive_data_amphibia <- data.frame(
  relative_age = time_steps,
  proportion_alive_malignant = malignant_alive_amphibia / max(malignant_alive_amphibia),
  proportion_alive_benign = benign_alive_amphibia / max(benign_alive_amphibia),
  proportion_alive_all = all_amphibia_alive / max(all_amphibia_alive)
)

# Plot smoothed survivorship curves for all, malignant, and benign (for Amphibia)
ggplot(alive_data_amphibia, aes(x = relative_age)) +
  geom_smooth(aes(y = proportion_alive_all, color = "All Amphibia"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_malignant, color = "Malignant"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_benign, color = "Benign"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  scale_color_manual(values = c(
    "All Amphibia" = "blue",
    "Malignant" = "red",
    "Benign" = "green"
  )) +
  ggtitle("Smoothed Survivorship Curve: All Amphibia vs. Malignant vs. Benign") +
  xlab("Proportion of Maximum Lifespan") +
  ylab("Proportion Alive (log scale)") +
  scale_y_log10() +
  theme_cowplot(12)
