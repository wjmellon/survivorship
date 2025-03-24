# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Load the dataset
data <- read.csv("records.csv")

# Filter the dataset for Mammals with Necropsy data and exclude infants
cutdata <- data %>% 
  filter(Necropsy == 1) %>% 
  filter(Class == "Mammalia") %>% 
  filter(Infant == 0)

# Select relevant columns (Species, age_months, max_longevity, Malignancy)
cutdata <- cutdata[, c(3, 28, 48, 18)]  # Adjust column indices based on your data
colnames(cutdata) <- c("age_months", "Species", "max_longevity", "Malignant")

# Clean data: remove individuals with invalid ages and longevity
cutdata$age_months[cutdata$age_months <= 0] <- NA
cutdata$max_longevity[cutdata$max_longevity <= 0] <- NA
cutdata <- na.omit(cutdata)

# Create a relative_age column: individual age divided by species-specific maximum longevity
cutdata <- cutdata %>% 
  mutate(relative_age = age_months / max_longevity)

# Define relative age time steps (0 to 1 by increments of 0.01)
time_steps <- seq(0, 1, by = 0.01)  # 1% increments of lifespan

# Filter for malignant and benign groups based on Malignancy column
malignant_mammals <- cutdata %>% filter(Malignant == 1)
benign_mammals <- cutdata %>% filter(Malignant == 0)
all_mammals <- cutdata  # All mammals (with or without malignancy)

# Calculate survival for each group (number of individuals alive at each time step)
malignant_alive <- sapply(time_steps, function(x) sum(malignant_mammals$relative_age > x))
benign_alive <- sapply(time_steps, function(x) sum(benign_mammals$relative_age > x))
all_mammals_alive <- sapply(time_steps, function(x) sum(all_mammals$relative_age > x))

# Create a data frame for plotting
alive_data_mammals <- data.frame(
  relative_age = time_steps,
  proportion_alive_malignant = malignant_alive / max(malignant_alive),
  proportion_alive_benign = benign_alive / max(benign_alive),
  proportion_alive_all = all_mammals_alive / max(all_mammals_alive)
)

# Plot smoothed survivorship curves for all, malignant, and benign
ggplot(alive_data_mammals, aes(x = relative_age)) +
  geom_smooth(aes(y = proportion_alive_all, color = "All Mammals"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_malignant, color = "Malignant"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  geom_smooth(aes(y = proportion_alive_benign, color = "Benign"), method = "gam", formula = y ~ s(x, bs = "cs"), se = FALSE, size = 1) +
  scale_color_manual(values = c(
    "All Mammals" = "blue",
    "Malignant" = "red",
    "Benign" = "green"
  )) +
  ggtitle("Smoothed Survivorship Curve: All Mammals vs. Malignant vs. Benign") +
  xlab("Proportion of Maximum Lifespan") +
  ylab("Proportion Alive (log scale)") +
  scale_y_log10() +
  theme_cowplot(12)
