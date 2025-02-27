library(ggplot2)
library(dplyr)
library(ggrepel)  # For non-overlapping labels

# Load the dataset
data <- read.csv("records.csv")

# Filter data for Mammalia class
cutdata <- filter(data, Necropsy == 1)
cutdata <- filter(cutdata, Class == "Mammalia")  # Filter for Mammalia
cutdata <- filter(cutdata, Infant == 0)  # Remove infants

# Ensure columns used for lymphoma deaths are correct
# Assuming lymphoma deaths data is in a column named "Lymphoma_Deaths"
cutdata <- cutdata[, c(3, 24, 48)]  # Assuming these columns contain age_months, max_longevity, lymphoma deaths
cutdata$age_months[cutdata$age_months <= 0] <- NA
cutdata <- na.omit(cutdata)

# Check if Lymphoma_Deaths is a valid column (modify if needed)
colnames(cutdata)  # Verify the names of the columns (adjust accordingly)

# Scatterplot with lymphoma deaths for Mammalia species
ggplot(cutdata, aes(x = Lymphoma_Deaths)) +
  geom_jitter(aes(y = 0), width = 0.2, height = 0, size = 3, alpha = 0.7, color = "red") +
  geom_text_repel(aes(y = 0, label = Species), size = 3, nudge_y = 0.1) +  # Label all species
  theme_minimal(base_size = 14) +
  labs(title = "Lymphoma Death Frequency Across Mammal Species",
       x = "Number of Lymphoma Deaths", y = "") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())
