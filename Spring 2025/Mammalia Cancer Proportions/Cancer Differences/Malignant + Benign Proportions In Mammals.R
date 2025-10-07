# Load libraries
library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

# Define cancer categories
malignant_types <- c(
  "melanoma", "carcinoma", "lymphoma", "adenocarcinoma", "leukemia",
  "cholangiocarcinoma", "leiomyosarcoma", "fibrosarcoma", "sarcoma",
  "lymphosarcoma", "hemangiosarcoma", "osteosarcoma", "myxosarcoma",
  "liposarcoma", "mast cell tumor", "seminoma", "insulinoma",
  "lymphangiosarcoma", "anaplastic", "mesothelioma", "cystadenocarcinoma",
  "neurofibrosarcoma", "chondrosarcoma", "dysgerminoma", "histiocytic sarcoma",
  "rhabdomyosarcoma", "synovial cell sarcoma", "melanosarcoma",
  "multiple myeloma", "medulloblastoma", "astrocytoma", "plasmacytoma",
  "Adenocarcinoma", "Leiomyosarcoma", "Fibrosarcoma", "Carcinoma",
  "Leukemia/Lymphoma", "Lymphoma", "Neoplasia", "Sarcoma", "Melanoma",
  "squamous cell carcinoma", "soft tissue sarcoma",
  "round cell sarcoma", "undifferentiated sarcoma", "fibroadenocarcinoma",
  "round cell tumor", "carinoma"
)

benign_types <- c(
  "cyst", "fibroma", "polyp", "adenoma", "lipoma", "trichoepithelioma",
  "hemangioma", "thymoma", "myxoma", "leiomyoma", "melanocytoma",
  "hepatoma", "cystadenoma", "neurofibroma", "trichoblastoma", "odontoma",
  "osteoma", "chondroma", "schwannoma", "ganglioneuroma", "myelolipoma",
  "rhabdomyoma", "adenomas", "fibrolipoma", "osteochondroma", "neurilemmoma"
)

# Load dataset
data <- read_csv("records.csv")

# Filter for mammals and deceased individuals (Necropsy == 1, not infants)
mammal_data <- data %>% 
  filter(Class == "Mammalia", Necropsy == 1, Infant == 0)

# Count total cancer cases per species
total_cases <- mammal_data %>% count(Species, name = "total_count") %>% filter(total_count >= 30)  # Ensure species have at least 30 cases

# Count malignant and benign cases per species
malignant_cases <- mammal_data %>%
  filter(grepl(paste(malignant_types, collapse = "|"), Type, ignore.case = TRUE)) %>%
  count(Species, name = "malignant_count")

benign_cases <- mammal_data %>%
  filter(grepl(paste(benign_types, collapse = "|"), Type, ignore.case = TRUE)) %>%
  count(Species, name = "benign_count")

# Compute proportions
malignant_proportions <- malignant_cases %>%
  left_join(total_cases, by = "Species") %>%
  mutate(malignant_proportion = malignant_count / total_count) %>%
  select(Species, malignant_proportion)

benign_proportions <- benign_cases %>%
  left_join(total_cases, by = "Species") %>%
  mutate(benign_proportion = benign_count / total_count) %>%
  select(Species, benign_proportion)

# Merge malignant and benign proportions
cancer_proportions <- full_join(malignant_proportions, benign_proportions, by = "Species") %>%
  replace_na(list(malignant_proportion = 0, benign_proportion = 0)) %>%
  filter(malignant_proportion > 0 | benign_proportion > 0)  # Exclude rows with zero proportions for both

# Prepare data for plotting
plot_data <- cancer_proportions %>%
  pivot_longer(cols = c("malignant_proportion", "benign_proportion"), 
               names_to = "Cancer_Type", 
               values_to = "Proportion") %>%
  mutate(Cancer_Type = ifelse(Cancer_Type == "malignant_proportion", "Malignant", "Benign"),
         Proportion = ifelse(Cancer_Type == "Benign", -Proportion, Proportion)) # Make benign negative

# Create bar plot
ggplot(plot_data, aes(x = Proportion, y = Species, fill = Cancer_Type)) +
  geom_bar(stat = "identity") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
  scale_fill_manual(values = c("Malignant" = "#B2182B", "Benign" = "#2166AC")) +  # Dark red & blue
  theme_bw() +  # Scientific-looking theme
  theme(
    panel.grid.major = element_line(color = "gray80", linetype = "dotted"),  # Subtle major grid lines
    panel.grid.minor = element_blank(),  # No minor grid lines for clarity
    axis.text = element_text(size = 12, color = "black"),  # Larger axis labels
    axis.title = element_text(size = 14, face = "bold"),  # Bold axis titles
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5)  # Centered bold title
  ) +
  labs(title = "Malignant vs Benign Cancer Proportions in Mammals",
       x = "Proportion of Cancer Cases",
       y = "Species")

# Print first few rows of the proportions
print(head(cancer_proportions, 5))
