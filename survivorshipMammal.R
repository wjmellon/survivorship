# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)

# Load the dataset
data <- read.csv("records.csv")

# Filter for Class Mammalia and calculate proportion lifespan
cutdata <- filter(data, Class == "Mammalia")
cutdata<-cutdata[,c(3,24,48)]
cutdata$age_months[cutdata$age_months <= 0] <- NA
cutdata$max_longevity[cutdata$max_longevity <= 0] <- NA
cutdata <- na.omit(cutdata)
cutdata$proportion_lifespan <- cutdata$age_months / cutdata$max_longevity

# Remove invalid or extreme values
cutdata <- cutdata %>%
  filter(!is.na(proportion_lifespan), proportion_lifespan <= 2) # Remove NAs and values > 2

# Data Preparation: Count occurrences of each proportion lifespan (binned)
lifespan_counts <- cutdata %>%
  mutate(lifespan_bin = cut(proportion_lifespan,                 # Bin proportion lifespan
                            breaks = seq(0, 2, by = 0.05),       # Bins of 0.05
                            right = FALSE)) %>%
  group_by(lifespan_bin) %>%                                    # Group by bins
  summarize(count = n(), .groups = 'drop') %>%                  # Count occurrences in each bin
  mutate(
    bin_midpoint = as.numeric(sub("\\[|\\)", "",                # Calculate bin midpoints
                                  gsub(",.*", "", lifespan_bin))) + 0.025
  )

# Plot: Count of Proportion Lifespan (Log Scale)
ggplot(lifespan_counts, aes(x = bin_midpoint, y = count)) +
  geom_point(color = "red", size = 2) +                         # Add points for each bin
  geom_smooth(method = "gam",                                  # Use GAM for smooth fit
              formula = y ~ s(x, bs = "cs"),                   # Cubic splines for flexibility
              color = "blue", size = 1, se = FALSE) +          # Add smooth line, no CI
  scale_y_log10() +                                            # Use log scale for y-axis
  ggtitle("Count of Proportion Lifespan (Log Scale) Mammals") +
  xlab("Proportion of Lifespan (midpoint of 0.05 bins)") +
  ylab("Log(Count)") +
  theme_cowplot(12)


