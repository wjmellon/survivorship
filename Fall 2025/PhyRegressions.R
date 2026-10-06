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

# Minimum species a class needs to get its own PGLS + plot.
# Pagel's lambda is unreliable with very few tips, so small classes
# (probably Amphibia) are skipped and reported in the console.
MIN_SPECIES <- 10

# Which subsets to run: "All" = every species, then one per class
subsets <- c("All", names(class_colors))

# Collects stats for every model fit
all_results <- list()

# -------------------------------------------------------------------------
# Helper: drop rows missing x / y / n, then prune data + tree so they match
# -------------------------------------------------------------------------
prep_data <- function(data, tree, xvar, yvar) {
  data <- data[complete.cases(data[, c(xvar, yvar, "n")]), ]
  data$Species <- gsub(" ", "_", data$Species)
  pruned.tree <- drop.tip(tree, setdiff(tree$tip.label, data$Species))
  data <- data[data$Species %in% pruned.tree$tip.label, ]
  rownames(data) <- data$Species
  SE <- setNames(data$SE_simple, data$Species)[rownames(data)]
  list(data = data, tree = pruned.tree, SE = SE)
}

# -------------------------------------------------------------------------
# Helper: fit PGLS on one subset (All or a single class) and plot it
#   - one grey regression line (same format as before)
#   - same colors, legend, dot sizes, size breaks for every plot
# -------------------------------------------------------------------------
pgls_plot <- function(data, tree, xvar, yvar, xlab, ylab, title, file,
                      subset = "All") {
  
  if (subset != "All") data <- data[data$Class == subset, ]
  prep <- prep_data(data, tree, xvar, yvar)
  label <- paste0(yvar, " ~ ", xvar, " | ", subset)
  cat("\n==========", label, "==========\n")
  
  if (nrow(prep$data) < MIN_SPECIES) {
    cat("  - Skipping (only", nrow(prep$data), "species)\n")
    return(invisible(NULL))
  }
  
  form  <- as.formula(paste(yvar, "~", xvar))
  model <- tryCatch(
    pglsSEyPagel(form, data = prep$data, tree = prep$tree,
                 se = prep$SE, method = "ML"),
    error = function(e) {
      message("  ! PGLS failed: ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(model)) return(invisible(NULL))
  print(summary(model))
  
  # Stats for the summary table
  tt <- summary(model)$tTable
  all_results[[label]] <<- tibble(
    subset    = subset,
    x         = xvar,
    y         = yvar,
    n_species = nrow(prep$data),
    intercept = tt[1, 1],
    slope     = tt[2, 1],
    p_value   = tt[2, 4],
    R2        = tryCatch(as.numeric(R2(phy = prep$tree, model)[3]),
                         error = function(e) NA),
    lambda    = tryCatch(as.numeric(summary(model)$modelStruct$corStruct[1]),
                         error = function(e) NA)
  )
  
  # Title / filename get the class name added for class-specific plots
  if (subset != "All") {
    title <- paste0(title, " (", subset, ")")
    file  <- sub("\\.png$", paste0("_", subset, ".png"), file)
  }
  
  p <- ggplot(prep$data, aes(x = .data[[xvar]], y = .data[[yvar]],
                             color = Class, size = n)) +
    geom_point(alpha = 1) +
    scale_color_manual(values = class_colors) +
    scale_size_continuous(
      range    = c(3, 8),
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

outcomes <- c(neoplasia_prevalence = "Neoplasia", cancer_prevalence = "Cancer")

# #########################################################################
# PART 1: DISTANCE FROM TYPE II SURVIVORSHIP (abs_shape)
# #########################################################################
data <- read.csv("Spring 2026/final_clean_data_w_multivariate.csv") %>%
  mutate(abs_shape = abs(shape_value),
         SE_simple = 1 / sqrt(n))
summary(data$n)   # sanity check

for (s in subsets) {
  for (yvar in names(outcomes)) {
    pgls_plot(data, tree, "abs_shape", yvar,
              "Distance from Type II Survivorship",
              paste0(outcomes[[yvar]], " Prevalence (%)"),
              paste0(outcomes[[yvar]], " Prevalence vs. Distance from Type II Survivorship"),
              paste0("abs_shape_", tolower(outcomes[[yvar]]), ".png"),
              subset = s)
  }
}

# #########################################################################
# PART 2: ALL SILER PARAMETERS
# #########################################################################
data <- read.csv("Fall 2025/Siler/siler_parameters_all_min50species.csv") %>%
  filter(FLAG == "0") %>%
  mutate(SE_simple = 1 / sqrt(n))
summary(data$n)   # sanity check

# Siler parameters -> axis labels. Edit names here if your columns differ.
siler_params <- c(
  a1 = "Initial Juvenile Mortality Risk (a1)",
  b1 = "Rate of Decline in Juvenile Mortality Risk (b1)",
  a2 = "Age-Independent Mortality Risk (a2)",
  a3 = "Initial Senescent Mortality Risk (a3)",
  b3 = "Change in Mortality Risk in the Senescent Stage (b3)"
)

missing <- setdiff(names(siler_params), names(data))
if (length(missing) > 0) {
  cat("Not found in Siler CSV, skipping:", paste(missing, collapse = ", "), "\n")
  siler_params <- siler_params[names(siler_params) %in% names(data)]
}

for (param in names(siler_params)) {
  for (s in subsets) {
    for (yvar in names(outcomes)) {
      pgls_plot(data, tree, param, yvar,
                siler_params[[param]],
                paste0(outcomes[[yvar]], " Prevalence (%)"),
                paste0(outcomes[[yvar]], " Prevalence vs. ", siler_params[[param]]),
                paste0("siler_", param, "_", tolower(outcomes[[yvar]]), ".png"),
                subset = s)
    }
  }
}

# #########################################################################
# SUMMARY TABLE: every model (All + each class)
# #########################################################################
results_table <- bind_rows(all_results) %>%
  mutate(across(c(intercept, slope, p_value, R2, lambda), ~ signif(.x, 3)))
print(results_table, n = Inf)
write.csv(results_table, "pgls_results_by_class.csv", row.names = FALSE)
