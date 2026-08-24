library(ggplot2)
library(dplyr)
library(scales)
library(cowplot)

# =========================================================================
# Mammalia
# =========================================================================
mammals_data <- final_clean_mortality_data %>%
  filter(Class == "Mammalia") %>%
  mutate(abs_shape = abs(shape_value))

neoplasia_mammals_model <- lm(neoplasia_prevalence ~ abs_shape, data = mammals_data)
summary(neoplasia_mammals_model)

ggplot(mammals_data, aes(x = abs_shape, y = neoplasia_prevalence, size = n)) +
  geom_point(alpha = 1, color = "lightblue") +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(neoplasia_mammals_model)[1],
    slope = coef(neoplasia_mammals_model)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    title = "Neoplasia Prevalence vs |Survivorship| (Mammals)"
  ) +
  theme_cowplot(12)
ggsave(filename = 'neoplasia_mammals.png', width = 10, height = 8, limitsize = FALSE, bg = "white")

cancer_mammals_model <- lm(cancer_prevalence ~ abs_shape, data = mammals_data)
summary(cancer_mammals_model)

ggplot(mammals_data, aes(x = abs_shape, y = cancer_prevalence, size = n)) +
  geom_point(alpha = 1, color = "lightblue") +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(cancer_mammals_model)[1],
    slope = coef(cancer_mammals_model)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    title = "Cancer Prevalence vs |Survivorship| (Mammals)"
  ) +
  theme_cowplot(12)
ggsave(filename = 'cancer_mammals.png', width = 10, height = 8, limitsize = FALSE, bg = "white")


# =========================================================================
# Aves
# =========================================================================
aves_data <- final_clean_mortality_data %>%
  filter(Class == "Aves") %>%
  mutate(abs_shape = abs(shape_value))

neoplasia_aves_model <- lm(neoplasia_prevalence ~ abs_shape, data = aves_data)
summary(neoplasia_aves_model)

ggplot(aves_data, aes(x = abs_shape, y = neoplasia_prevalence, size = n)) +
  geom_point(alpha = 1, color = "lightgreen") +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(neoplasia_aves_model)[1],
    slope = coef(neoplasia_aves_model)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    title = "Neoplasia Prevalence vs |Survivorship| (Aves)"
  ) +
  theme_cowplot(12)
ggsave(filename = 'neoplasia_aves.png', width = 10, height = 8, limitsize = FALSE, bg = "white")

cancer_aves_model <- lm(cancer_prevalence ~ abs_shape, data = aves_data)
summary(cancer_aves_model)

ggplot(aves_data, aes(x = abs_shape, y = cancer_prevalence, size = n)) +
  geom_point(alpha = 1, color = "lightgreen") +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(cancer_aves_model)[1],
    slope = coef(cancer_aves_model)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    title = "Cancer Prevalence vs |Survivorship| (Aves)"
  ) +
  theme_cowplot(12)
ggsave(filename = 'cancer_aves.png', width = 10, height = 8, limitsize = FALSE, bg = "white")


# =========================================================================
# Reptilia
# =========================================================================
reptilia_data <- final_clean_mortality_data %>%
  filter(Class == "Reptilia") %>%
  mutate(abs_shape = abs(shape_value))

neoplasia_reptilia_model <- lm(neoplasia_prevalence ~ abs_shape, data = reptilia_data)
summary(neoplasia_reptilia_model)

ggplot(reptilia_data, aes(x = abs_shape, y = neoplasia_prevalence, size = n)) +
  geom_point(alpha = 1, color = "purple") +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(neoplasia_reptilia_model)[1],
    slope = coef(neoplasia_reptilia_model)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    title = "Neoplasia Prevalence vs |Survivorship| (Reptilia)"
  ) +
  theme_cowplot(12)
ggsave(filename = 'neoplasia_reptilia.png', width = 10, height = 8, limitsize = FALSE, bg = "white")

cancer_reptilia_model <- lm(cancer_prevalence ~ abs_shape, data = reptilia_data)
summary(cancer_reptilia_model)

ggplot(reptilia_data, aes(x = abs_shape, y = cancer_prevalence, size = n)) +
  geom_point(alpha = 1, color = "purple") +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(cancer_reptilia_model)[1],
    slope = coef(cancer_reptilia_model)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    title = "Cancer Prevalence vs |Survivorship| (Reptilia)"
  ) +
  theme_cowplot(12)
ggsave(filename = 'cancer_reptilia.png', width = 10, height = 8, limitsize = FALSE, bg = "white")


# =========================================================================
# Amphibia
# =========================================================================
amphibia_data <- final_clean_mortality_data %>%
  filter(Class == "Amphibia") %>%
  mutate(abs_shape = abs(shape_value))

neoplasia_amphibia_model <- lm(neoplasia_prevalence ~ abs_shape, data = amphibia_data)
summary(neoplasia_amphibia_model)

ggplot(amphibia_data, aes(x = abs_shape, y = neoplasia_prevalence, size = n)) +
  geom_point(alpha = 1, color = "orange") +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(neoplasia_amphibia_model)[1],
    slope = coef(neoplasia_amphibia_model)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    title = "Neoplasia Prevalence vs |Survivorship| (Amphibia)"
  ) +
  theme_cowplot(12)
ggsave(filename = 'neoplasia_amphibia.png', width = 10, height = 8, limitsize = FALSE, bg = "white")

cancer_amphibia_model <- lm(cancer_prevalence ~ abs_shape, data = amphibia_data)
summary(cancer_amphibia_model)

ggplot(amphibia_data, aes(x = abs_shape, y = cancer_prevalence, size = n)) +
  geom_point(alpha = 1, color = "orange") +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(cancer_amphibia_model)[1],
    slope = coef(cancer_amphibia_model)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    title = "Cancer Prevalence vs |Survivorship| (Amphibia)"
  ) +
  theme_cowplot(12)
ggsave(filename = 'cancer_amphibia.png', width = 10, height = 8, limitsize = FALSE, bg = "white")