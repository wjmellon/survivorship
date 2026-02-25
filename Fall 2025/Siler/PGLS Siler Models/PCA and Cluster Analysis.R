# =====================================================
# PCA + Cluster Analysis — Siler Parameters
# =====================================================
library(dplyr)
library(ggplot2)
library(readr)
library(cowplot)
library(ape)
library(factoextra)  # For PCA visualization & clustering
library(cluster)

# -----------------------------
# 1. Load data
# -----------------------------
data <- read.csv("Fall 2025/Siler/siler_parameters_all_species.csv", stringsAsFactors = FALSE)

# Ensure species names match
data$Species <- gsub(" ", "_", data$Species)
rownames(data) <- data$Species

# -----------------------------
# 2. Select Siler parameters + create log versions
# -----------------------------
params <- c("a1", "a2", "a3", "b1", "b2", "b3")

# Check which parameters exist
params_exist <- params[params %in% names(data)]
if (length(params_exist) < length(params)) {
  missing <- setdiff(params, params_exist)
  warning("Missing columns: ", paste(missing, collapse=", "))
}
params <- params_exist

# Create log versions with robust error handling
for (p in params) {
  cat("Processing:", p, "\n")
  
  # Convert to numeric if needed
  if (!is.numeric(data[[p]])) {
    data[[p]] <- as.numeric(as.character(data[[p]]))
  }
  
  # Remove NAs for calculating minimum
  valid_vals <- data[[p]][!is.na(data[[p]])]
  
  if (length(valid_vals) == 0) {
    warning(paste("Column", p, "has no valid values. Skipping log transformation."))
    next
  }
  
  positive_vals <- valid_vals[valid_vals > 0]
  
  if (length(positive_vals) == 0) {
    warning(paste("Column", p, "has no positive values. Cannot log transform."))
    next
  }
  
  # Log-transform with epsilon for non-positive values
  minpos <- min(positive_vals, na.rm = TRUE)
  eps <- minpos * 1e-6
  
  # Apply transformation
  data[[paste0("log_", p)]] <- ifelse(
    is.na(data[[p]]) | data[[p]] <= 0,
    NA,
    log(data[[p]])
  )
  
  # Handle any remaining edge cases
  if (any(is.infinite(data[[paste0("log_", p)]]), na.rm = TRUE)) {
    data[[paste0("log_", p)]][is.infinite(data[[paste0("log_", p)]])] <- NA
  }
  
  cat("  -> Created log_", p, " (", sum(!is.na(data[[paste0("log_", p)]])), " valid values)\n", sep="")
}

# Build list of all variables that were successfully created
log_params <- paste0("log_", params)
log_params <- log_params[log_params %in% names(data)]
all_vars <- c(params, log_params)

cat("\nUsing variables:", paste(all_vars, collapse=", "), "\n")

# ===== DIAGNOSTIC: Check missingness pattern =====
cat("\n===== MISSINGNESS ANALYSIS =====\n")
cat("Total species in dataset:", nrow(data), "\n\n")

# Count missing values per variable
missing_counts <- sapply(data[, all_vars], function(x) sum(is.na(x)))
cat("Missing values per variable:\n")
print(sort(missing_counts, decreasing = TRUE))

# Count complete cases per variable
cat("\n\nComplete cases if using only raw parameters:\n")
raw_complete <- sum(complete.cases(data[, params]))
cat("  Raw params only:", raw_complete, "species\n")

cat("\nComplete cases if using only log parameters:\n")
log_complete <- sum(complete.cases(data[, log_params]))
cat("  Log params only:", log_complete, "species\n")

cat("\nComplete cases if using both raw + log:\n")
all_complete <- sum(complete.cases(data[, all_vars]))
cat("  Raw + Log params:", all_complete, "species\n")

# ===== SOLUTION: Use only raw OR only log parameters =====
cat("\n===== CHOOSING BEST VARIABLE SET =====\n")

if (raw_complete >= log_complete && raw_complete >= 10) {
  all_vars <- params
  cat("Using ONLY raw parameters (n =", raw_complete, "species)\n")
} else if (log_complete >= 10) {
  all_vars <- log_params
  cat("Using ONLY log parameters (n =", log_complete, "species)\n")
} else if (raw_complete >= 3) {
  all_vars <- params
  cat("WARNING: Small sample size. Using raw parameters (n =", raw_complete, "species)\n")
} else if (log_complete >= 3) {
  all_vars <- log_params
  cat("WARNING: Small sample size. Using log parameters (n =", log_complete, "species)\n")
} else {
  stop("Insufficient complete data for PCA. Try imputation or use fewer variables.")
}

# Remove rows with NA in selected variables
pca_data <- data[complete.cases(data[, all_vars]), all_vars]

cat("\nFinal PCA dataset: ", nrow(pca_data), " species with complete data\n")
cat("Using variables:", paste(all_vars, collapse=", "), "\n")

if (nrow(pca_data) < 3) {
  stop("Insufficient data for PCA. Only ", nrow(pca_data), " species with complete data.")
}

# -----------------------------
# 3. Standardize variables
# -----------------------------
pca_data_scaled <- scale(pca_data)

# Check for zero variance columns
zero_var <- apply(pca_data_scaled, 2, function(x) sd(x, na.rm=TRUE) == 0)
if (any(zero_var)) {
  warning("Removing zero-variance columns: ", paste(names(zero_var)[zero_var], collapse=", "))
  pca_data_scaled <- pca_data_scaled[, !zero_var]
  all_vars <- all_vars[!zero_var]
}

# -----------------------------
# 4. PCA
# -----------------------------
pca_res <- prcomp(pca_data_scaled, center = FALSE, scale. = FALSE)  # Already scaled
summary(pca_res)

# Plot variance explained
print(
  fviz_eig(pca_res, addlabels = TRUE, ylim = c(0, 60)) +
    ggtitle("PCA: Variance Explained by Components")
)

# -----------------------------
# 5. PCA Biplot with Class coloring
# -----------------------------
species_metadata <- data[rownames(pca_data), ]

# Check if Class column exists
if ("Class" %in% names(species_metadata)) {
  print(
    fviz_pca_ind(pca_res,
                 geom.ind = "point",
                 pointshape = 21,
                 pointsize = 3,
                 fill.ind = species_metadata$Class,
                 palette = "jco",
                 addEllipses = TRUE,
                 legend.title = "Class") +
      ggtitle("PCA of Siler Parameters (Raw + Log)")
  )
} else {
  warning("'Class' column not found. Plotting without class colors.")
  print(
    fviz_pca_ind(pca_res,
                 geom.ind = "point",
                 pointshape = 21,
                 pointsize = 3) +
      ggtitle("PCA of Siler Parameters (Raw + Log)")
  )
}

# -----------------------------
# 6. Clustering — Hierarchical + k-means
# -----------------------------

# Hierarchical clustering
dist_matrix <- dist(pca_data_scaled)
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, labels = rownames(pca_data), main = "Hierarchical Clustering Dendrogram", cex=0.6)
rect.hclust(hc, k = 4, border = 2:5)

# Cut tree to assign cluster labels
hc_clusters <- cutree(hc, k = 4)
species_metadata$HC_cluster <- hc_clusters

# K-means clustering
set.seed(123)
k <- 4
km_res <- kmeans(pca_data_scaled, centers = k, nstart = 25)
species_metadata$KM_cluster <- km_res$cluster

# -----------------------------
# 7. PCA plot colored by clusters
# -----------------------------
print(
  fviz_pca_ind(pca_res,
               geom.ind = "point",
               pointshape = 21,
               pointsize = 3,
               fill.ind = factor(species_metadata$KM_cluster),
               palette = "Set2",
               addEllipses = TRUE,
               legend.title = "K-means Cluster") +
    ggtitle("PCA of Siler Parameters with K-means Clusters")
)

# -----------------------------
# 8. Summary statistics by cluster
# -----------------------------
cat("\n===== CLUSTER SIZES =====\n")
cat("Hierarchical clustering:\n")
print(table(species_metadata$HC_cluster))
cat("\nK-means clustering:\n")
print(table(species_metadata$KM_cluster))

if ("Class" %in% names(species_metadata)) {
  cat("\n===== CLASS DISTRIBUTION BY K-MEANS CLUSTER =====\n")
  print(table(species_metadata$KM_cluster, species_metadata$Class))
}

# -----------------------------
# 9. Save results
# -----------------------------
write.csv(species_metadata, "Siler_PCA_Cluster_Metadata.csv", row.names = TRUE)
cat("\nResults saved to: Siler_PCA_Cluster_Metadata.csv\n")

# Optional: Save PCA loadings
loadings <- as.data.frame(pca_res$rotation[, 1:4])
loadings$Variable <- rownames(loadings)
write.csv(loadings, "Siler_PCA_Loadings.csv", row.names = FALSE)
cat("PCA loadings saved to: Siler_PCA_Loadings.csv\n")