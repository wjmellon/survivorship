library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(broom)  # for glance() and tidy()

# Load and preprocess data
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

# Preprocess cancer proportions for mammals
mammal_data <- data %>% 
  filter(Class == "Mammalia") %>% 
  group_by(Species) %>% 
  filter(n() > 30) %>% 
  ungroup()

malignant_counts <- mammal_data %>%
  filter(Type %in% malignant_types) %>%
  count(Species, name = "malignant_count")

benign_counts <- mammal_data %>%
  filter(Type %in% benign_types) %>%
  count(Species, name = "benign_count")

total_counts <- mammal_data %>% 
  count(Species, name = "total_count")

cancer_proportions <- total_counts %>%
  left_join(malignant_counts, by = "Species") %>%
  left_join(benign_counts, by = "Species") %>%
  mutate(
    malignant_proportion = malignant_count / total_count,
    benign_proportion = benign_count / total_count,
    CancerDifference = malignant_proportion - benign_proportion
  ) %>%
  select(Species, CancerDifference)

# Define variables to analyze
variables <- c("birth_weight", "weaning_weight", "adult_weight", "Gestation", "Infancy",
               "litter_size", "litters_year", "interbirth_interval", "female_maturity",
               "male_maturity", "growth_rate", "max_longevity", "metabolic_rate", "Weaning")

# Function to analyze each variable
analyze_variable <- function(var) {
  # Filter out rows with -1 (and NA) for the current variable only
  valid_data <- data %>%
    filter(Class == "Mammalia",
           !!sym(var) != -1,
           !is.na(!!sym(var))) %>%
    group_by(Species) %>%
    summarise(Value = first(!!sym(var)))
  
  # Merge with cancer proportions
  merged_data <- cancer_proportions %>%
    inner_join(valid_data, by = "Species") %>%
    filter(!is.na(CancerDifference))
  
  if(nrow(merged_data) < 2) return(NULL)
  
  # Fit linear model and extract statistics
  model <- lm(CancerDifference ~ Value, data = merged_data)
  glance_stats <- broom::glance(model)
  tidy_stats <- broom::tidy(model)
  
  tibble(
    Variable = var,
    R_squared = glance_stats$r.squared,
    P_value = tidy_stats$p.value[2],
    Slope = tidy_stats$estimate[2],
    N = nrow(merged_data)
  )
}

# Analyze all variables and combine the results
results <- map_dfr(variables, analyze_variable) %>%
  filter(!is.na(R_squared)) %>%
  arrange(desc(R_squared))

# Visualization: R-squared values with significance
ggplot(results, aes(x = reorder(Variable, R_squared), y = R_squared, 
                    fill = P_value < 0.05)) +
  geom_col() +
  geom_text(aes(label = sprintf("R²=%.2f", R_squared)), 
            hjust = -0.1, size = 3) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "#1f77b4", "FALSE" = "gray70"),
                    name = "Significant (p < 0.05)") +
  labs(x = "Variable", y = "R-squared", 
       title = "Correlation Strength Between Biological Variables and Cancer Difference",
       subtitle = "Sorted by explanatory power (R-squared)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom")

