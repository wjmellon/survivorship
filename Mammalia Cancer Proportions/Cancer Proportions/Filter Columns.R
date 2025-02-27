library(dplyr)
library(ggplot2)
library(readr)
library(cowplot)

ucfirst <- function(s) {
  paste0(toupper(substr(s, 1, 1)), tolower(substr(s, 2, nchar(s))))
}

# Load dataset
data <- read_csv("records.csv")

# Define cancer type to filter
cancer_type <- "rhabdomyosarcoma"  # Change this to desired cancer type

# Filter for mammals and deceased individuals (Necropsy == 1, not infants)
mammal_data <- data %>% 
  filter(Class == "Mammalia", Necropsy == 1, Infant == 0)

# Filter data for selected cancer type
cancer_data <- mammal_data %>% 
  filter(grepl(cancer_type, Type, ignore.case = TRUE))

# Count cases per species
total_cases <- mammal_data %>% count(Species, name = "total_count") %>% filter(total_count > 50)
cancer_cases <- cancer_data %>% count(Species, name = "cancer_count")

# Compute proportions
proportions <- cancer_cases %>% 
  left_join(total_cases, by = "Species") %>% 
  mutate(proportion = cancer_count / total_count) %>% 
  arrange(desc(proportion))

# Ensure gestation data is present
gestation_data <- data %>% select(Species, Gestation) %>% distinct()

# Merge proportions with gestation data
proportion_vs_gestation <- proportions %>% 
  left_join(gestation_data, by = "Species") %>% 
  filter(!is.na(Gestation))

# Generate dynamic filename based on cancer type
output_filename <- paste0("Mammalia Cancer Proportions/Cancer Proportions/Proportion Excel Sheets/", cancer_type, "_vs_gestation.csv")

# Save to CSV
write_csv(proportion_vs_gestation, output_filename)

# Select top 12 species with the most cases for labeling
label_species <- proportion_vs_gestation %>% top_n(12, wt = total_count)

# Plot cancer proportion against gestation time with point size representing total cases
# and labels for top 12 species
ggplot(proportion_vs_gestation, aes(x = Gestation, y = proportion, size = total_count)) +
  geom_point(color = "blue", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "green") +
  geom_text(data = label_species, aes(label = Species), hjust = -0.1, vjust = 0, size = 4) +
  theme_minimal() +
  labs(title = paste0(ucfirst(cancer_type), " Proportion vs. Gestation Time"),
       x = "Gestation Time (days)",
       y = paste0("Proportion of ", ucfirst(cancer_type), " Cases"),
       size = "Total Cases") +
  theme(text = element_text(size = 14))
