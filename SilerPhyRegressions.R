# Run PGLS Function 

library(phytools)
library(geiger)
library(caper)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(rr2)
library(nlme)

## Read in the csv sheet
data <- read.csv("Fall 2025/Siler/siler_parameters_all_min50species.csv")

data <- data %>%
  filter(FLAG == "0") %>%
  mutate(SE_simple = 1/sqrt(n))

## Read phylogenetic tree in
tree <- read.tree("min20Fixed516.nwk")

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
# Plot 1: Neoplasia Prevalence
# =========================================================================
ggplot(data, aes(x = b3, y = neoplasia_prevalence, color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(siler_b3_neoplasia)[1], 
    slope = coef(siler_b3_neoplasia)[2],
    color = 'grey', linewidth = 1.2
  ) +
  theme_minimal() +
  labs(
    x = "Change in Mortality Risk in the Senescent Stage (b3)",
    y = "Neoplasia Prevalence (%)",
    color = "Class",
    title = "Neoplasia Prevalence vs. Change in Mortality Risk in the Senescent Stage (b3)"
  ) +
  theme_cowplot(12)

ggsave(filename = 'siler_b3_neoplasia.png', width = 10, height = 8, limitsize = FALSE, bg = "white")


# =========================================================================
# Plot 2: Cancer Prevalence
# =========================================================================
ggplot(data, aes(x = b3, y = cancer_prevalence, color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # hide size legend
  geom_abline(
    intercept = coef(siler_b3_cancer)[1], 
    slope = coef(siler_b3_cancer)[2],
    color = 'grey', linewidth = 1.2
  ) +
  theme_minimal() +
  labs(
    x = "Change in Mortality Risk in the Senescent Stage (b3)",
    y = "Cancer Prevalence (%)",
    color = "Class",
    title = "Cancer Prevalence vs. Change in Mortality Risk in the Senescent Stage (b3)"
  ) +
  theme_cowplot(12)

ggsave(filename = 'siler_b3_cancer.png', width = 10, height = 8, limitsize = FALSE, bg = "white")