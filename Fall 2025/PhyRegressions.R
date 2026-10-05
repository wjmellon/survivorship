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

# Class colors (ggplot default hue palette, so they match your earlier figures)
class_colors <- c(
  "Amphibia" = "#F8766D",
  "Aves"     = "#7CAE00",
  "Mammalia" = "#00BFC4",
  "Reptilia" = "#C77CFF"
)

# -------------------------------------------------------------------------
# Helper: prune data + tree so they match
# -------------------------------------------------------------------------
prep_data <- function(data, tree) {
  data$Species <- gsub(" ", "_", data$Species)
  pruned.tree <- drop.tip(tree, setdiff(tree$tip.label, data$Species))
  data <- data[data$Species %in% pruned.tree$tip.label, ]
  rownames(data) <- data$Species
  SE <- setNames(data$SE_simple, data$Species)[rownames(data)]
  list(data = data, tree = pruned.tree, SE = SE)
}

# -------------------------------------------------------------------------
# Helper: plot one PGLS model
#   - size legend uses fixed clean breaks (1,000 / 2,500 / 5,000 / 7,500)
#     so every figure has the same scale
# -------------------------------------------------------------------------
pgls_plot <- function(data, xvar, yvar, model, xlab, ylab, title, file) {
  
  p <- ggplot(data, aes(x = .data[[xvar]], y = .data[[yvar]],
                        color = Class, size = n)) +
    geom_point(alpha = 1) +
    scale_color_manual(values = class_colors) +
    # Fixed, evenly spaced breaks shared by ALL plots (n ranges 34-7322).
    # Dot AREA is proportional to n, so the scale is linear.
    scale_size_continuous(
      range    = c(1.5, 8),   # 1.5 floor so n = 34 species stay visible
      breaks   = c(1000, 2500, 5000, 7500),
      labels   = scales::comma,
      limits   = c(0, 7500),
      name     = "Sample Size (n)"
    ) +
    guides(
      color = guide_legend(override.aes = list(size = 4), order = 1),
      size  = guide_legend(override.aes = list(color = "grey40"), order = 2)
    ) +
    geom_abline(
      intercept = coef(model)[1],
      slope     = coef(model)[2],
      color = "grey", linewidth = 1.2
    ) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = xlab, y = ylab, color = "Class", title = title) +
    theme_cowplot(12) +
    theme(legend.position = "right")
  
  print(p)
  ggsave(filename = file, plot = p, width = 10, height = 8,
         limitsize = FALSE, bg = "white")
  invisible(p)
}

# #########################################################################
# PART 1: DISTANCE FROM TYPE II SURVIVORSHIP (abs_shape)
# #########################################################################
data <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv") %>%
  mutate(abs_shape = abs(shape_value),
         SE_simple = 1 / sqrt(n))

prep <- prep_data(data, tree)
data <- prep$data; pruned.tree <- prep$tree; SE <- prep$SE
summary(data$n)   # sanity check: these are the n values the legend uses

abs_shape_neoplasia <- pglsSEyPagel(neoplasia_prevalence ~ abs_shape, data = data,
                                    tree = pruned.tree, se = SE, method = "ML")
abs_shape_cancer    <- pglsSEyPagel(cancer_prevalence ~ abs_shape, data = data,
                                    tree = pruned.tree, se = SE, method = "ML")
summary(abs_shape_neoplasia)
summary(abs_shape_cancer)

pgls_plot(data, "abs_shape", "neoplasia_prevalence", abs_shape_neoplasia,
          "Distance from Type II Survivorship", "Neoplasia Prevalence (%)",
          "Neoplasia Prevalence vs. Distance from Type II Survivorship",
          "abs_shape_neoplasia.png")

pgls_plot(data, "abs_shape", "cancer_prevalence", abs_shape_cancer,
          "Distance from Type II Survivorship", "Cancer Prevalence (%)",
          "Cancer Prevalence vs. Distance from Type II Survivorship",
          "abs_shape_cancer.png")

# #########################################################################
# PART 2: SILER b3 (Change in Mortality Risk in the Senescent Stage)
# #########################################################################
data <- read.csv("Fall 2025/Siler/siler_parameters_all_min50species.csv") %>%
  filter(FLAG == "0") %>%
  mutate(SE_simple = 1 / sqrt(n))

prep <- prep_data(data, tree)
data <- prep$data; pruned.tree <- prep$tree; SE <- prep$SE
summary(data$n)   # sanity check

siler_b3_neoplasia <- pglsSEyPagel(neoplasia_prevalence ~ b3, data = data,
                                   tree = pruned.tree, se = SE, method = "ML")
siler_b3_cancer    <- pglsSEyPagel(cancer_prevalence ~ b3, data = data,
                                   tree = pruned.tree, se = SE, method = "ML")
summary(siler_b3_neoplasia)
summary(siler_b3_cancer)

pgls_plot(data, "b3", "neoplasia_prevalence", siler_b3_neoplasia,
          "Change in Mortality Risk in the Senescent Stage (b3)",
          "Neoplasia Prevalence (%)",
          "Neoplasia Prevalence vs. Change in Mortality Risk in the Senescent Stage (b3)",
          "siler_b3_neoplasia.png")

pgls_plot(data, "b3", "cancer_prevalence", siler_b3_cancer,
          "Change in Mortality Risk in the Senescent Stage (b3)",
          "Cancer Prevalence (%)",
          "Cancer Prevalence vs. Change in Mortality Risk in the Senescent Stage (b3)",
          "siler_b3_cancer.png")