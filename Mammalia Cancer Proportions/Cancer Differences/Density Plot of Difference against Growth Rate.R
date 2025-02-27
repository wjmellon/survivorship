library(ggplot2)
library(dplyr)
library(readr)
library(ggrepel)

# Load dataset
data <- read_csv("records.csv")

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
  "squamous cell carcinoma", "soft tissue sarcoma", "",
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

# Filter for mammals only and species with more than 30 recorded cases
mammal_data <- data %>% 
  filter(Class == "Mammalia", growth_rate != -1) %>% 
  group_by(Species) %>% 
  filter(n() > 30) %>% 
  ungroup()

# Count malignant and benign cases per species
malignant_counts <- mammal_data %>%
  filter(Type %in% malignant_types) %>%
  count(Species, name = "malignant_count")

benign_counts <- mammal_data %>%
  filter(Type %in% benign_types) %>%
  count(Species, name = "benign_count")

# Count total cases per species
total_counts <- mammal_data %>% count(Species, name = "total_count")

# Compute proportions
cancer_proportions <- total_counts %>%
  left_join(malignant_counts, by = "Species") %>%
  left_join(benign_counts, by = "Species") %>%
  mutate(
    malignant_proportion = malignant_count / total_count,
    benign_proportion = benign_count / total_count,
    CancerDifference = malignant_proportion - benign_proportion
  ) %>%
  select(Species, CancerDifference)

# Merge with growth rate data
growth_rate_data <- mammal_data %>% select(Species, growth_rate) %>% distinct()
mammal_data <- cancer_proportions %>% left_join(growth_rate_data, by = "Species")

# Ensure Growth Rate and Cancer Difference columns exist
mammal_data <- mammal_data %>% filter(!is.na(growth_rate), !is.na(CancerDifference))

# Get top 20 species with highest CancerDifference
top_species <- mammal_data %>% 
  arrange(desc(CancerDifference)) %>% 
  head(20)

# Linear model for annotation
model <- lm(CancerDifference ~ growth_rate, data = mammal_data)
r_sq <- summary(model)$r.squared
p_val <- summary(model)$coefficients[2,4]
# Get regression coefficients for equation
slope <- coef(model)[2] %>% round(3)
intercept <- coef(model)[1] %>% round(3)

ggplot(mammal_data, aes(x = growth_rate, y = CancerDifference)) +
  # Density contours
  geom_density_2d(color = "gray30", linewidth = 0.3, alpha = 0.8) +
  # Points with subtle appearance
  geom_point(shape = 21, fill = "gray40", color = "black", 
             size = 2.5, alpha = 0.7, stroke = 0.3) +
  # Regression line with confidence interval
  geom_smooth(method = "lm", color = "darkblue", linewidth = 0.8,  # Brighter blue color
              fill = "gray80", linetype = "solid") +
  # Label top 20 species
  geom_text_repel(data = top_species, aes(label = Species), 
                  size = 3.5, box.padding = 0.4, seed = 123) +  # Consistent label placement
  # Statistical annotations
  annotate("text", x = min(mammal_data$growth_rate), y = max(mammal_data$CancerDifference),
           label = sprintf("y = %.3f + %.3fx\nR² = %.2f\np = %.3f", 
                           intercept, slope, r_sq, p_val),
           hjust = 0, vjust = 1, size = 4, family = "serif", lineheight = 0.9) +
  # Theme customization
  theme_bw(base_size = 12) +
  theme(
    text = element_text(family = "serif"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10, color = "black"),
    plot.margin = unit(c(5,5,5,5), "mm")
  ) +
  # Axis labels
  labs(x = "Growth Rate", y = "Malignant-Benign Cancer Proportion Difference")