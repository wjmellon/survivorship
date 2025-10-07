# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Load the dataset
data <- read.csv("records.csv")

# Filter the dataset for Aves with Necropsy data and exclude infants
cutdata_aves <- data %>% 
  filter(Necropsy == 1) %>% 
  filter(Class == "Aves") %>% 
  filter(Infant == 0)

# Select relevant columns (Species, age_months, max_longevity, Malignancy)
cutdata_aves <- cutdata_aves[, c(3, 28, 48, 18)]  # Adjust column indices based on your data
colnames(cutdata_aves) <- c("age_months", "Species", "max_longevity", "Malignant")

# Clean data: remove individuals with invalid ages and longevity
cutdata_aves$age_months[cutdata_aves$age_months <= 0] <- NA
cutdata_aves$max_longevity[cutdata_aves$max_longevity <= 0] <- NA
cutdata_aves <- na.omit(cutdata_aves)

# Create a relative_age column: individual age divided by species-specific maximum longevity
cutdata_aves <- cutdata_aves %>% 
  mutate(relative_age = age_months / max_longevity)

# Define relative age time steps (0 to 1 by increments of 0.01)
time_steps <- seq(0, 1, by = 0.01)  # 1% increments of lifespan

# Filter for malignant and benign groups based on Malignancy column
malignant_aves <- cutdata_aves %>% filter(Malignant == 1)
benign_aves <- cutdata_aves %>% filter(Malignant == 0)
all_aves <- cutdata_aves  # All aves (with or without malignancy)

# Calculate survival for each group (number of individuals alive at each time step)
malignant_alive_aves <- sapply(time_steps, function(x) sum(malignant_aves$relative_age > x))
benign_alive_aves <- sapply(time_steps, function(x) sum(benign_aves$relative_age > x))
all_aves_alive <- sapply(time_steps, function(x) sum(all_aves$relative_age > x))

# Create a data frame for plotting
alive_data_aves <- data.frame(
  relative_age = time_steps,
  proportion_alive_malignant = malignant_alive_aves / max(malignant_alive_aves),
  proportion_alive_benign = benign_alive_aves / max(benign_alive_aves),
  proportion_alive_all = all_aves_alive / max(all_aves_alive)
)

# Plot smoothed survivorship curves for all, malignant, and benign (for Aves)
ggplot(alive_data_aves, aes(x = relative_age)) +
  geom_smooth(aes(y = proportion_alive_all, color = "All Aves"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_malignant, color = "Malignant"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_benign, color = "Benign"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  scale_color_manual(values = c(
    "All Aves" = "blue",
    "Malignant" = "red",
    "Benign" = "green"
  )) +
  ggtitle("Smoothed Survivorship Curve: All Aves vs. Malignant vs. Benign") +
  xlab("Proportion of Maximum Lifespan") +
  ylab("Proportion Alive (log scale)") +
  scale_y_log10() +
  theme_cowplot(12)
