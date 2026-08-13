# Run PGLS Function 

library(phytools)
library(geiger)
library(caper)
library(tidyverse)
library(cowplot)
library(ggplot2)
library(rr2)
library(nlme)

## Read in the csv sheet that has your cancer data and your predictor variables
data <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv")

data <- data %>%
  mutate(abs_shape = abs(shape_value))

## Simple standard error calculation: 1 / sqrt(n)
data <- data %>%
  mutate(SE_simple = 1/sqrt(data$n))

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

# Run PGLS Model
abs_neoplasia <- pglsSEyPagel(neoplasia_prevalence ~ abs_shape, data = data, tree = pruned.tree, se = SE, method = "ML")
summary(abs_neoplasia)

# Extract statistics for plot subtitle
r.v.abs_neoplasia <- R2(phy = pruned.tree, abs_neoplasia)
r.v.abs_neoplasia <- format(r.v.abs_neoplasia[3])
r.v.abs_neoplasia <- signif(as.numeric(r.v.abs_neoplasia), digits = 2)

ld.v.abs_neoplasia <- summary(abs_neoplasia)$modelStruct$corStruct
ld.v.abs_neoplasia <- signif(ld.v.abs_neoplasia[1], digits = 2)

p.v.abs_neoplasia <- summary(abs_neoplasia)$tTable
p.v.abs_neoplasia <- signif(p.v.abs_neoplasia[2, 4], digits = 2)

# Updated Plot (Matching the second style format)
ggplot(data, aes(x = abs_shape, y = neoplasia_prevalence, color = Class, size = n)) +
  geom_point(alpha = 1) +
  scale_size_continuous(range = c(3, 8), guide = "none") +  # Hide size legend
  geom_abline(
    intercept = coef(abs_neoplasia)[1],
    slope = coef(abs_neoplasia)[2],
    color = 'grey', size = 1.2
  ) +
  theme_minimal() +
  labs(
    x = "|Survivorship|",
    y = "Neoplasia Prevalence (%)",
    color = "Class"
  ) +
  labs(
    title = "Neoplasia Prevalence vs. Absolute Value of Survivorship",
  ) +
  theme_cowplot(12)

## Save plot to working directory
ggsave(filename = 'abs_neoplasia.png', width = 10, height = 8, limitsize = FALSE, bg = "white")