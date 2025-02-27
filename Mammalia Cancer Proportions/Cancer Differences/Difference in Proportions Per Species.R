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
  "squamous cell carcinoma", "soft tissue sarcoma", "round cell sarcoma",
  "undifferentiated sarcoma", "fibroadenocarcinoma", "round cell tumor", "carinoma"
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
total_cases <- mammal_data %>% count(Species, name = "total_count") %>% filter(total_count > 30)

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
  replace_na(list(malignant_proportion = 0, benign_proportion = 0))

# Filter for species with at least 30 cases and calculate differences
cancer_differences <- cancer_proportions %>%
  left_join(total_cases, by = "Species") %>%  # Include total_count for filtering
  filter(total_count >= 30) %>%  # Only species with at least 30 cases
  mutate(cancer_difference = malignant_proportion - benign_proportion,
         absolute_difference = abs(cancer_difference))  # Absolute difference for bubble size

# Create bar plot for cancer proportions by species
ggplot(cancer_differences, aes(x = reorder(Species, cancer_difference), y = cancer_difference, fill = cancer_difference > 0)) +
  geom_bar(stat = "identity", show.legend = FALSE, alpha = 0.7) +  # Bars representing the difference
  scale_fill_manual(values = c("TRUE" = "#D7301F", "FALSE" = "#4575B4")) +  # Red for malignant > benign, Blue for benign > malignant
  labs(title = "Difference Between Malignant and Benign Proportions by Species", 
       x = "Species", 
       y = "Difference in Proportions (Malignant - Benign)") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12, color = "black"),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate species names for readability
  )
