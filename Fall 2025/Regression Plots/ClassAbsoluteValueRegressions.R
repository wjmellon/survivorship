library(ggplot2)
library(dplyr)

# Filter to mammals
mammals_data <- final_clean_mortality_data %>%
   filter(Class == "Mammalia") %>%
   mutate(abs_shape = abs(shape_value))


# Plot Neoplasia vs |Survivorship| for mammals
ggplot(mammals_data, aes(x = abs_shape, y = neoplasia_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "lightblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    title = "Neoplasia Prevalence vs |Survivorship| (Mammals)"
  )

# Run the linear model
neoplasia_mammals_model <- lm(neoplasia_prevalence ~ abs_shape, data = mammals_data)

# See results
summary(neoplasia_mammals_model)


# Filter to mammals
mammals_data <- final_clean_mortality_data %>%
  filter(Class == "Mammalia") %>%
  mutate(abs_shape = abs(shape_value))


# Plot Neoplasia vs |Survivorship| for mammals
ggplot(mammals_data, aes(x = abs_shape, y = cancer_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "lightblue") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    title = "Cancer Prevalence vs |Survivorship| (Mammals)"
  )

# Run the linear model
cancer_mammals_model <- lm(cancer_prevalence ~ abs_shape, data = mammals_data)

# See results
summary(cancer_mammals_model)




# Filter to Aves
aves_data <- final_clean_mortality_data %>%
  filter(Class == "Aves") %>%
  mutate(abs_shape = abs(shape_value))


# Plot Neoplasia vs |Survivorship| for Aves
ggplot(aves_data, aes(x = abs_shape, y = neoplasia_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "lightgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    title = "Neoplasia Prevalence vs |Survivorship| (Aves)"
  )

# Run the linear model
neoplasia_aves_model <- lm(neoplasia_prevalence ~ abs_shape, data = aves_data)

# See results
summary(neoplasia_aves_model)


# Filter to Aves
aves_data <- final_clean_mortality_data %>%
  filter(Class == "Aves") %>%
  mutate(abs_shape = abs(shape_value))


# Plot Neoplasia vs |Survivorship| for Aves
ggplot(aves_data, aes(x = abs_shape, y = cancer_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "lightgreen") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    title = "Cancer Prevalence vs |Survivorship| (Aves)"
  )

# Run the linear model
cancer_aves_model <- lm(cancer_prevalence ~ abs_shape, data = aves_data)

# See results
summary(cancer_aves_model)



# Filter to Reptilia
reptilia_data <- final_clean_mortality_data %>%
  filter(Class == "Reptilia") %>%
  mutate(abs_shape = abs(shape_value))


# Plot Neoplasia vs |Survivorship| for Reptilia
ggplot(reptilia_data, aes(x = abs_shape, y = neoplasia_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    title = "Neoplasia Prevalence vs |Survivorship| (Reptilia)"
  )

# Run the linear model
neoplasia_reptilia_model <- lm(neoplasia_prevalence ~ abs_shape, data = reptilia_data)

# See results
summary(neoplasia_reptilia_model)


# Filter to Reptilia
reptilia_data <- final_clean_mortality_data %>%
  filter(Class == "Reptilia") %>%
  mutate(abs_shape = abs(shape_value))


# Plot Neoplasia vs |Survivorship| for Reptilia
ggplot(reptilia_data, aes(x = abs_shape, y = cancer_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "purple") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    title = "Cancer Prevalence vs |Survivorship| (Reptilia)"
  )

# Run the linear model
cancer_reptilia_model <- lm(cancer_prevalence ~ abs_shape, data = reptilia_data)

# See results
summary(cancer_reptilia_model)




# Filter to Amphibia
amphibia_data <- final_clean_mortality_data %>%
  filter(Class == "Amphibia") %>%
  mutate(abs_shape = abs(shape_value))


# Plot Neoplasia vs |Survivorship| for Amphibia
ggplot(amphibia_data, aes(x = abs_shape, y = neoplasia_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "orange") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    title = "Neoplasia Prevalence vs |Survivorship| (Amphibia)"
  )

# Run the linear model
neoplasia_amphibia_model <- lm(neoplasia_prevalence ~ abs_shape, data = amphibia_data)

# See results
summary(neoplasia_amphibia_model)


# Filter to Amphibia
amphibia_data <- final_clean_mortality_data %>%
  filter(Class == "Amphibia") %>%
  mutate(abs_shape = abs(shape_value))


# Plot Neoplasia vs |Survivorship| for Amphibia
ggplot(amphibia_data, aes(x = abs_shape, y = cancer_prevalence)) +
  geom_point(alpha = 1, size = 1, color = "orange") +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Cancer Prevalence (%)",
    title = "Cancer Prevalence vs |Survivorship| (Amphibia)"
  )

# Run the linear model
cancer_amphibia_model <- lm(cancer_prevalence ~ abs_shape, data = amphibia_data)

# See results
summary(cancer_amphibia_model)


