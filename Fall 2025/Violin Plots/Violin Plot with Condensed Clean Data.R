library(ggplot2)
library(dplyr)

select <- dplyr::select   # in case MASS is loaded and masks dplyr::select

# Survivorship types (Species, survivorship_type) come from here
condensed_clean_data <- read.csv("Fall 2025/Fall 2025 Clean Data/final_clean_data.csv")

# NEW: prevalence + Class come from the prevalence dataset
prevalence_data <- read.csv("species-cancer-prevalence-data.csv", check.names = FALSE) %>%
  mutate(Species = gsub(" ", "_", Species)) %>%
  select(Species, Class,
         neoplasia_prevalence = NeoplasiaPrevalence,
         cancer_prevalence    = MalignancyPrevalence)

# Drop old prevalence / Class columns, then join in the new values
condensed_clean_data <- condensed_clean_data %>%
  mutate(Species = gsub(" ", "_", Species)) %>%
  select(-any_of(c("Class", "neoplasia_prevalence", "cancer_prevalence",
                   "NeoplasiaPrevalence", "MalignancyPrevalence"))) %>%
  inner_join(prevalence_data, by = "Species")

cat("Species after merge:", nrow(condensed_clean_data), "\n")

# Ensure factor levels for consistent order
condensed_clean_data$survivorship_type <- factor(
  condensed_clean_data$survivorship_type,
  levels = c("Type III", "Trending toward Type III", "Type II", "Trending toward Type I", "Type I")
)

condensed_clean_data$Class <- factor(condensed_clean_data$Class)

class_colors <- c(
  "Amphibia" = "#F8766D",
  "Aves"     = "#7CAE00",
  "Mammalia" = "#00BFC4",
  "Reptilia" = "#C77CFF"
)

legend_fill <- class_colors[levels(condensed_clean_data$Class)]

legend_guide <- guide_legend(
  override.aes = list(
    shape  = 16,
    size   = 3,
    alpha  = 1
  )
)

# -------------------------------------------------------------------------
# Plot 1: Cancer Prevalence by Survivorship Curve Type
# -------------------------------------------------------------------------
ggplot(condensed_clean_data, aes(x = survivorship_type, y = cancer_prevalence)) +
  geom_violin(trim = FALSE, alpha = 0.9, fill = "lightgray", color = "black") +
  geom_jitter(aes(color = Class), width = 0.10, alpha = 0.7, size = 1.8) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4, fatten = 0.5,
               color = "black", linewidth = 2.5) +
  scale_color_manual(values = class_colors, guide = legend_guide) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Cancer Prevalence by Survivorship Curve Type",
    x = "Survivorship Curve Type",
    y = "Cancer Prevalence (%)",
    color = "Class"
  ) +
  coord_cartesian(xlim = c(0.2, 5.8), ylim = c(0, 1), expand = FALSE, clip = "on") +
  scale_y_continuous(breaks = c(0, 0.5, 1.0),
                     labels = c("0", "50", "100")) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.line.x = element_line(linewidth = 1.2, color = "black"),
    axis.line.y = element_line(linewidth = 1.2, color = "black"),
    axis.ticks = element_line(linewidth = 1),
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_blank()
  )

ggsave(filename='Cancer Violin Plot.png', width=11, height=8, limitsize=FALSE, bg="white")


# -------------------------------------------------------------------------
# Plot 2: Neoplasia Prevalence by Survivorship Curve Type
# -------------------------------------------------------------------------
ggplot(condensed_clean_data, aes(x = survivorship_type, y = neoplasia_prevalence)) +
  geom_violin(trim = FALSE, alpha = 0.9, fill = "lightgray", color = "black") +
  geom_jitter(aes(color = Class), width = 0.10, alpha = 0.7, size = 1.8) +
  stat_summary(fun = median, geom = "crossbar", width = 0.4, fatten = 0.5,
               color = "black", linewidth = 2.5) +
  scale_color_manual(values = class_colors, guide = legend_guide) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Neoplasia Prevalence by Survivorship Curve Type",
    x = "Survivorship Curve Type",
    y = "Neoplasia Prevalence (%)",
    color = "Class"
  ) +
  coord_cartesian(xlim = c(0.2, 5.8), ylim = c(0, 1), expand = FALSE, clip = "on") +
  scale_y_continuous(breaks = c(0, 0.5, 1.0),
                     labels = c("0", "50", "100")) +
  theme(
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 0.5),
    axis.line.x = element_line(linewidth = 1.2, color = "black"),
    axis.line.y = element_line(linewidth = 1.2, color = "black"),
    axis.ticks = element_line(linewidth = 1),
    legend.position = "right",
    panel.grid = element_blank(),
    panel.border = element_blank()
  )

ggsave(filename='Neoplasia Violin Plot.png', width=11, height=8, limitsize=FALSE, bg="white")


# -------------------------------------------------------------------------
# Species Counts Per Category
# -------------------------------------------------------------------------
category_counts <- condensed_clean_data %>%
  group_by(survivorship_type) %>%
  summarise(num_species = n()) %>%
  arrange(desc(num_species))

for(i in seq_len(nrow(category_counts))) {
  cat(as.character(category_counts$survivorship_type[i]), "has",
      category_counts$num_species[i], "species\n")
}

# -------------------------------------------------------------------------
# Median Prevalence Per Survivorship Type (console output)
# -------------------------------------------------------------------------
median_summary <- condensed_clean_data %>%
  group_by(survivorship_type) %>%
  summarise(
    median_cancer_prevalence    = median(cancer_prevalence, na.rm = TRUE),
    median_neoplasia_prevalence = median(neoplasia_prevalence, na.rm = TRUE)
  )

cat("======================\n")
cat("Median Cancer Prevalence by Survivorship Type\n")
cat("======================\n")
for (i in seq_len(nrow(median_summary))) {
  cat(sprintf("  - %-30s : %.4f\n",
              as.character(median_summary$survivorship_type[i]),
              median_summary$median_cancer_prevalence[i]))
}

cat("\n======================\n")
cat("Median Neoplasia Prevalence by Survivorship Type\n")
cat("======================\n")
for (i in seq_len(nrow(median_summary))) {
  cat(sprintf("  - %-30s : %.4f\n",
              as.character(median_summary$survivorship_type[i]),
              median_summary$median_neoplasia_prevalence[i]))
}