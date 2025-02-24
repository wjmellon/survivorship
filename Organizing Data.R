library(dplyr)
library(ggplot2)
library(cowplot)

# Load the dataset (adjust the path and sheet name as needed)
data <- read.csv("records.csv")

# Malignant Tumors
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

# Benign Tumors
benign_types <- c(
  "cyst", "fibroma", "polyp", "adenoma", "lipoma", "trichoepithelioma",
  "hemangioma", "thymoma", "myxoma", "leiomyoma", "melanocytoma",
  "hepatoma", "cystadenoma", "neurofibroma", "trichoblastoma", "odontoma",
  "osteoma", "chondroma", "schwannoma", "ganglioneuroma", "myelolipoma",
  "rhabdomyoma", "adenomas", "fibrolipoma", "osteochondroma", "neurilemmoma"
)

# Other Tumors
other_types <- c(
  "hyperplasia", "pheochromocytoma", "nephroblastoma", "carcinoid",
  "epithelioma", "meningioma", "chromatophoroma", "mastocytoma",
  "fibromatous epulis", "mastocytosis", "fibropapilloma", "odontogenic",
  "fibropapillomatosis", "lymphangioma", "iridophoroma", "ameloblastoma",
  "sarcoids", "glioma", "teratoma", "histiocytoma", "islet cell tumor",
  "chemodectoma", "chordoma", "neuroectodermal tumor", "mass",
  "neuroednocrine", "Carcinoid", "leydig cell tumor", "neuroendocrine",
  "granulosa cell", "tumors", "chromatophroma", "paraganglioma",
  "sertoli cell tumor"
)


# Filter the data
filtered_data_manual <- data %>%
  filter(Necropsy == 1, Infant == 0, Type %in% malignant_types
  )

# View the filtered data
head(filtered_data_manual)

filtered_data <- filter(data, Necropsy == 1, Infant == 0)

# Split into malignant and benign datasets
malignant_data <- filter(filtered_data, Malignant == 1)
benign_data <- filter(filtered_data, Malignant == 0)

# View the first few rows to check
head(malignant_data)
head(benign_data)

# Assuming filtered_data_manual and malignant_data are your two dataframes

# Find the differences between the two datasets
differences <- anti_join(filtered_data_manual, malignant_data)

# Display differences in the terminal
print(differences)

# Write differences to CSV file
write.csv(differences, "differences_between_manual_and_malignant.csv", row.names = FALSE)
