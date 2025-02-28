library(dplyr)
library(ggplot2)
library(readr)
library(cowplot)

# Load dataset
data <- read_csv("records.csv")

# Filter for deceased individuals (Necropsy == 1, not infants), and lymphoma cases
lymphoma_data <- data %>% 
  filter(Necropsy == 1, Infant == 0, grepl("lymphoma", Type, ignore.case = TRUE))

# Count lymphoma cases per species
total_cases <- data %>% 
  filter(Necropsy == 1, Infant == 0) %>% 
  count(Species, name = "total_count") %>% 
  filter(total_count > 60)
lymphoma_cases <- lymphoma_data %>% count(Species, name = "lymphoma_count")

# Compute proportions
proportions <- lymphoma_cases %>% 
  left_join(total_cases, by = "Species") %>% 
  mutate(proportion = lymphoma_count / total_count) %>% 
  arrange(desc(proportion))

# Print the top 5 proportions to the terminal
top_5_proportions <- head(proportions, 5)
print(top_5_proportions)

# Remove NA values from common names
top_species <- proportions %>% top_n(20, wt = proportion) %>% 
  left_join(data %>% select(Species, common_name) %>% distinct(), by = "Species") %>% 
  filter(!is.na(common_name))

# Save to CSV
write_csv(proportions, "Mammalia Cancer Proportions/Cancer Proportions/Proportion Excel Sheets/lymphoma_proportions.csv")

# Plot top species with common names
ggplot(top_species, aes(x = reorder(common_name, proportion), y = proportion)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Proportion of lymphoma Cases in Species",
       x = "Common Name",
       y = "Proportion of Cases") +
  theme(text = element_text(size = 14))

# Analyze lymphoma proportion against gestation time
# Ensure gestation data is present
gestation_data <- data %>% select(Species, Gestation) %>% distinct()

# Merge proportions with gestation data and filter out rows with Gestation == -1 or missing values
proportion_vs_gestation <- proportions %>% 
  left_join(gestation_data, by = "Species") %>% 
  filter(!is.na(Gestation), Gestation >= 0, Gestation != -1)

# Save to CSV
write_csv(proportion_vs_gestation, "Mammalia Cancer Proportions/Cancer Proportions/Proportion Excel Sheets/lymphoma_vs_gestation.csv")

# Select top 12 species with the most cases for labeling

label_species <- proportion_vs_gestation %>% top_n(12, wt = total_count) %>%
  left_join(data %>% select(Species, common_name) %>% distinct(), by = "Species")

# Plot lymphoma proportion against gestation time with point size representing total cases
# and labels for top 12 species using common name
ggplot(proportion_vs_gestation, aes(x = Gestation, y = proportion, size = total_count)) +
  geom_point(color = "darkred", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  geom_text(data = label_species, aes(label = common_name), hjust = -0.1, vjust = 0, size = 4) +
  theme_minimal() +
  labs(title = "lymphoma Proportion vs. Gestation Time",
       x = "Gestation Time (days)",
       y = "Proportion of lymphoma Cases",
       size = "Total Cases") +
  theme(text = element_text(size = 14))

# Check the number of species by Class
data %>% group_by(Class) %>% count(Species) %>% arrange(desc(n))
