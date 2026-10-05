# Run PGLS Function 
library(phytools)
library(geiger)
library(caper)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(rr2)
library(nlme)
library(scales)

## Read phylogenetic tree in
tree <- read.tree("min20Fixed516.nwk")

# Class colors
class_colors <- c(
  "Mammalia" = "purple",
  "Amphibia" = "blue",
  "Reptilia" = "darkgreen",
  "Aves"     = "red"
)

# #########################################################################
# PART 1: DISTANCE FROM TYPE II SURVIVORSHIP (abs_shape)
# #########################################################################

## Read in the csv sheet
data <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")
data <- data %>%
  mutate(
    abs_shape = abs(shape_value),
    SE_simple = 1 / sqrt(n)
  )

# Match data to phylogeny and prune
data$Species <- gsub(" ", "_", data$Species)
includedSpecies <- data$Species
pruned.tree <- drop.tip(tree, setdiff(tree$tip.label, includedSpecies))
pruned.tree <- keep.tip(pruned.tree, pruned.tree$tip.label)
data$Keep <- data$Species %in% pruned.tree$tip.label
data <- data[!(data$Keep == FALSE), ]
rownames(data) <- data$Species
SE <- setNames(data$SE_simple, data$Species)[rownames(data)]

# --- PGLS Models ---
abs_shape_neoplasia <- pglsSEyPagel(neoplasia_prevalence ~ abs_shape, data = data, tree = pruned.tree, se = SE, method = "ML")
abs_shape_cancer <- pglsSEyPagel(cancer_prevalence ~ abs_shape, data = data, tree = pruned.tree, se = SE, method = "ML")
summary(abs_shape_neoplasia)
summary(abs_shape_cancer)

# =========================================================================
# Plot 1: Neoplasia Prevalence vs. abs_shape
# =========================================================================
ggplot(data, aes(x = abs_shape, y = (neoplasia_prevalence), color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = class_colors) +
  scale_size_continuous(range = c(3, 8), name = "Sample Size (n)") +
  guides(
    color = guide_legend(override.aes = list(size = 6), order = 1),
    size  = guide_legend(override.aes = list(color = "grey40"), order = 2)
  ) +
  geom_abline(
    intercept = coef(abs_shape_neoplasia)[1],
    slope = coef(abs_shape_neoplasia)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Distance from Type II Survivorship",
    y = "Neoplasia Prevalence (%)",
    color = "Class",
    title = "Neoplasia Prevalence vs. Distance from Type II Survivorship"
  ) +
  theme_cowplot(12) +
  theme(legend.position = "right")
ggsave(filename = 'abs_shape_neoplasia.png', width = 10, height = 8, limitsize = FALSE, bg = "white")

# =========================================================================
# Plot 2: Cancer Prevalence vs. abs_shape
# =========================================================================
ggplot(data, aes(x = abs_shape, y = (cancer_prevalence), color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = class_colors) +
  scale_size_continuous(range = c(3, 8), name = "Sample Size (n)") +
  guides(
    color = guide_legend(override.aes = list(size = 6), order = 1),
    size  = guide_legend(override.aes = list(color = "grey40"), order = 2)
  ) +
  geom_abline(
    intercept = coef(abs_shape_cancer)[1],
    slope = coef(abs_shape_cancer)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Distance from Type II Survivorship",
    y = "Cancer Prevalence (%)",
    color = "Class",
    title = "Cancer Prevalence vs. Distance from Type II Survivorship"
  ) +
  theme_cowplot(12) +
  theme(legend.position = "right")
ggsave(filename = 'abs_shape_cancer.png', width = 10, height = 8, limitsize = FALSE, bg = "white")


# #########################################################################
# PART 2: SILER b3 (Change in Mortality Risk in the Senescent Stage)
# #########################################################################

## Read in the csv sheet
data <- read.csv("Fall 2025/Siler/siler_parameters_all_min50species.csv")
data <- data %>%
  filter(FLAG == "0") %>%
  mutate(SE_simple = 1 / sqrt(n))

# Match data to phylogeny and prune
data$Species <- gsub(" ", "_", data$Species)
includedSpecies <- data$Species
pruned.tree <- drop.tip(tree, setdiff(tree$tip.label, includedSpecies))
pruned.tree <- keep.tip(pruned.tree, pruned.tree$tip.label)
data$Keep <- data$Species %in% pruned.tree$tip.label
data <- data[!(data$Keep == FALSE), ]
rownames(data) <- data$Species
SE <- setNames(data$SE_simple, data$Species)[rownames(data)]

# --- PGLS Models ---
siler_b3_neoplasia <- pglsSEyPagel(neoplasia_prevalence ~ b3, data = data, tree = pruned.tree, se = SE, method = "ML")
siler_b3_cancer <- pglsSEyPagel(cancer_prevalence ~ b3, data = data, tree = pruned.tree, se = SE, method = "ML")
summary(siler_b3_neoplasia)
summary(siler_b3_cancer)

# =========================================================================
# Plot 3: Neoplasia Prevalence vs. b3
# =========================================================================
ggplot(data, aes(x = b3, y = (neoplasia_prevalence), color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = class_colors) +
  scale_size_continuous(range = c(3, 8), name = "Sample Size (n)") +
  guides(
    color = guide_legend(override.aes = list(size = 6), order = 1),
    size  = guide_legend(override.aes = list(color = "grey40"), order = 2)
  ) +
  geom_abline(
    intercept = coef(siler_b3_neoplasia)[1],
    slope = coef(siler_b3_neoplasia)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Change in Mortality Risk in the Senescent Stage (b3)",
    y = "Neoplasia Prevalence (%)",
    color = "Class",
    title = "Neoplasia Prevalence vs. Change in Mortality Risk in the Senescent Stage (b3)"
  ) +
  theme_cowplot(12) +
  theme(legend.position = "right")
ggsave(filename = 'siler_b3_neoplasia.png', width = 10, height = 8, limitsize = FALSE, bg = "white")

# =========================================================================
# Plot 4: Cancer Prevalence vs. b3
# =========================================================================
ggplot(data, aes(x = b3, y = (cancer_prevalence), color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_color_manual(values = class_colors) +
  scale_size_continuous(range = c(3, 8), name = "Sample Size (n)") +
  guides(
    color = guide_legend(override.aes = list(size = 6), order = 1),
    size  = guide_legend(override.aes = list(color = "grey40"), order = 2)
  ) +
  geom_abline(
    intercept = coef(siler_b3_cancer)[1],
    slope = coef(siler_b3_cancer)[2],
    color = 'grey', linewidth = 1.2
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  labs(
    x = "Change in Mortality Risk in the Senescent Stage (b3)",
    y = "Cancer Prevalence (%)",
    color = "Class",
    title = "Cancer Prevalence vs. Change in Mortality Risk in the Senescent Stage (b3)"
  ) +
  theme_cowplot(12) +
  theme(legend.position = "right")
ggsave(filename = 'siler_b3_cancer.png', width = 10, height = 8, limitsize = FALSE, bg = "white")