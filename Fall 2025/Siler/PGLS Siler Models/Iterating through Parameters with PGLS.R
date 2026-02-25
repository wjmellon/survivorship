# -----------------------------
# PGLS pipeline — custom pglsSEyPagel
# Response: neoplasia_prevalence
# Predictors: b1, b2, c1, c2 (+ logs)
# -----------------------------

library(dplyr)
library(ggplot2)
library(ape)
library(readr)
library(cowplot)

# -----------------------------
# 1. Source custom PGLS functions
# -----------------------------
source("Fall 2025/Siler/PGLS Siler Models/pglsFunction.R")  # must contain pglsSEyPagel

# -----------------------------
# 2. Load phylogenetic tree
# -----------------------------
tree <- read.tree("min20Fixed516.nwk")

# -----------------------------
# 3. Load data
# -----------------------------
data <- read.csv("Fall 2025/Siler/siler_parameters_all_species.csv", stringsAsFactors = FALSE)

# Ensure species names match tree
data$Species <- gsub(" ", "_", data$Species)
includedSpecies <- intersect(data$Species, tree$tip.label)
pruned.tree <- drop.tip(tree, setdiff(tree$tip.label, includedSpecies))
data <- data[data$Species %in% pruned.tree$tip.label, ]
rownames(data) <- data$Species

# -----------------------------
# 4. Define predictors
# -----------------------------
predictors <- c("a1", "a2", "a3", "b1", "b2", "b3")
log_predictors <- paste0("log_", predictors)

# Create log versions with improved error handling
for (p in predictors) {
  # Check if column exists
  if (!p %in% names(data)) {
    warning(paste("Column", p, "not found in data. Skipping."))
    next
  }
  
  # Check if column is numeric
  if (!is.numeric(data[[p]])) {
    warning(paste("Column", p, "is not numeric. Attempting to convert."))
    data[[p]] <- as.numeric(as.character(data[[p]]))
  }
  
  # Remove NA values for calculation
  valid_vals <- data[[p]][!is.na(data[[p]])]
  
  if (length(valid_vals) == 0) {
    warning(paste("Column", p, "has no valid numeric values. Skipping."))
    next
  }
  
  # Handle non-positive values
  if (any(valid_vals <= 0, na.rm = TRUE)) {
    minpos <- min(valid_vals[valid_vals > 0], na.rm = TRUE)
    eps <- minpos * 1e-6
    data[[paste0("log_", p)]] <- log(data[[p]] + eps)
    warning(paste("Added epsilon to", p, "before log transformation due to non-positive values"))
  } else {
    data[[paste0("log_", p)]] <- log(data[[p]])
  }
}

# Verify which log predictors were successfully created
log_predictors <- log_predictors[log_predictors %in% names(data)]
cat("\nSuccessfully created log predictors:", paste(log_predictors, collapse=", "), "\n")

# -----------------------------
# 5. Build SE vector helper
# -----------------------------
get_SE <- function(df, pred_name) {
  se_col <- paste0(pred_name, "_se")
  if (se_col %in% names(df)) {
    se_vec <- df[[se_col]]
  } else if ("n" %in% names(df)) {
    se_vec <- 1 / sqrt(df$n)
  } else {
    stop("No SE info found for ", pred_name)
  }
  names(se_vec) <- rownames(df)
  se_vec[rownames(df)]
}

# -----------------------------
# 6. Run PGLS for each predictor
# -----------------------------
response <- "neoplasia_prevalence"

# Only use predictors that exist in data
all_preds <- c(predictors[predictors %in% names(data)], log_predictors)

results <- data.frame(
  predictor = character(),
  slope = numeric(),
  p_value = numeric(),
  lambda = numeric(),
  sig2e = numeric(),
  stringsAsFactors = FALSE
)

for (pred in all_preds) {
  cat("\nProcessing:", pred, "\n")
  
  # Check if predictor exists and has valid data
  if (!pred %in% names(data)) {
    warning(paste("Predictor", pred, "not found in data. Skipping."))
    next
  }
  
  if (all(is.na(data[[pred]]))) {
    warning(paste("Predictor", pred, "has only NA values. Skipping."))
    next
  }
  
  # Create complete cases subset
  complete_rows <- complete.cases(data[[response]], data[[pred]])
  if (sum(complete_rows) < 3) {
    warning(paste("Too few complete cases for", pred, "(n =", sum(complete_rows), "). Skipping."))
    next
  }
  
  data_complete <- data[complete_rows, ]
  
  # Prune tree to match complete cases
  tree_complete <- drop.tip(pruned.tree, setdiff(pruned.tree$tip.label, rownames(data_complete)))
  
  formula_text <- paste0(response, " ~ ", pred)
  se_vec <- tryCatch({
    se_full <- get_SE(data, sub("^log_", "", pred))
    se_full[rownames(data_complete)]
  }, error = function(e) {
    warning("Could not get SE for ", pred, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(se_vec)) next
  
  fit <- tryCatch({
    pglsSEyPagel(as.formula(formula_text), data = data_complete, tree = tree_complete, se = se_vec, method = "ML")
  }, error = function(e) {
    warning("PGLS failed for ", formula_text, ": ", e$message)
    return(NULL)
  })
  
  if (is.null(fit)) next
  
  s <- summary(fit)
  slope <- s$tTable[2, "Value"]
  pval  <- s$tTable[2, "p-value"]
  lam   <- as.numeric(fit$modelStruct$corStruct[1])
  s2e   <- fit$sig2e
  
  results <- rbind(results, data.frame(
    predictor = pred,
    slope = slope,
    p_value = pval,
    lambda = lam,
    sig2e = s2e,
    stringsAsFactors = FALSE
  ))
  
  cat("  -> n =", sum(complete_rows), "| slope =", round(slope, 5), "| p =", round(pval, 4), "\n")
}

# -----------------------------
# 7. Print tables
# -----------------------------
cat("\n===== ALL PGLS RESULTS =====\n")
print(results)

if (nrow(results) > 0) {
  sig_results <- results %>% filter(!is.na(p_value) & p_value < 0.05)
  cat("\n===== SIGNIFICANT PGLS RESULTS (p < 0.05) =====\n")
  if (nrow(sig_results) > 0) {
    print(sig_results)
  } else {
    cat("No significant results found.\n")
  }
} else {
  cat("\nNo results to display.\n")
  sig_results <- data.frame()
}

# -----------------------------
# 8. Plot significant models
# -----------------------------
plot_pgls <- function(df, xvar, yvar, fit_obj=NULL, out_file=NULL) {
  intercept <- coef(fit_obj)[1]
  slope <- coef(fit_obj)[2]
  s <- summary(fit_obj)
  
  p <- ggplot(df, aes_string(x = xvar, y = yvar)) +
    geom_point(alpha = 0.8, size = 2, aes(color = if("Class" %in% names(df)) Class else NULL)) +
    geom_abline(intercept = intercept, slope = slope, color = "black", size = 1) +
    theme_minimal() +
    labs(title = paste("PGLS:", yvar, "vs", xvar),
         subtitle = paste0("Slope=", round(slope,4), "  |  p=", signif(s$tTable[2, "p-value"],3)),
         x = xvar,
         y = yvar) +
    theme(legend.position = "bottom")
  
  if (!is.null(out_file)) ggsave(out_file, p, width=8, height=6)
  return(p)
}

# Generate plots for significant results
if (nrow(sig_results) > 0) {
  for (pred in sig_results$predictor) {
    cat("\nGenerating plot for:", pred, "\n")
    
    # Create complete cases for this predictor
    complete_rows <- complete.cases(data[[response]], data[[pred]])
    data_complete <- data[complete_rows, ]
    tree_complete <- drop.tip(pruned.tree, setdiff(pruned.tree$tip.label, rownames(data_complete)))
    
    fit_obj <- tryCatch({
      se_vec <- get_SE(data, sub("^log_", "", pred))[rownames(data_complete)]
      pglsSEyPagel(as.formula(paste(response, "~", pred)), data=data_complete, tree=tree_complete,
                   se=se_vec, method="ML")
    }, error=function(e) {
      warning("Could not refit model for plotting: ", e$message)
      return(NULL)
    })
    
    if (!is.null(fit_obj)) {
      print(plot_pgls(data_complete, pred, response, fit_obj))
    }
  }
} else {
  cat("\nNo significant results to plot.\n")
}