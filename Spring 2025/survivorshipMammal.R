# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(Rage)

# Load the dataset
data <- read.csv("Fall 2025/Filtering Data/cleanPath.min20.062822.csv")


# Filter the dataset for Mammals with Necropsy data and exclude infants
cutdata <- data %>%
  filter(Necropsy == 1) %>%
  filter(Infant == 0) %>%
  mutate(
    age_months = as.numeric(age_months),
    max_longevity = as.numeric(max_longevity)
  )
  

# Select relevant columns (assuming columns 3 = Species, 24 = age_months, 48 = max_longevity)
cutdata <- cutdata[, c(4, 29, 49)]
colnames(cutdata) <- c("age_months", "Species", "max_longevity")

cutdata <- cutdata %>%
  filter(!is.na(age_months) & age_months > 0,
         !is.na(max_longevity) & max_longevity > 0) %>%
  mutate(relative_age = age_months / max_longevity)

# Create summary table with species count >= 20
species_counts <- cutdata %>%
  group_by(Species) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n >= 20)

# Filter main dataset to keep only those species
cutdata <- cutdata %>%
  filter(Species %in% species_counts$Species)

time_steps <- seq(0, 1, by = 0.01)

# Ensure Species is character (not factor) and extract unique values
species_list <- unique(as.character(cutdata$Species))

# Check how many species exist
cat("Total species to process:", length(species_list), "\n")

# Initialize empty list BEFORE running the loop
results_list <- list()

for (sp in species_list) {
  # Filter data for current species
  sp_data <- cutdata %>% filter(Species == sp)
  
  # Calculate alive counts across time_steps for this species
  alive_counts <- sapply(time_steps, function(x) {
    sum(sp_data$relative_age >= x)
  })
  
  # Normalize to initial population size
  lx <- alive_counts / max(alive_counts)
  
  # Calculate Rage shape metric (trunc = TRUE prevents log(0) error)
  shape_val <- shape_surv(lx, trunc = TRUE)
  
  # Determine classification
  surv_cat <- case_when(
    shape_val >= 0.3 ~ "Type I (late mortality, senescence)",
    shape_val >= 0.1 & shape_val < 0.3 ~ "Trending toward Type I",
    shape_val > -0.1 & shape_val < 0.1 ~ "Type II (constant mortality)",
    shape_val > -0.3 & shape_val <= -0.1 ~ "Trending toward Type III",
    shape_val <= -0.3 ~ "Type III (early mortality)"
  )
  
  # Print results to console
  cat("\n-----------------------------------\n")
  cat("Species:", sp, "\n")
  cat("Standardized AUC:", round(shape_val, 3), "\n")
  cat("Classification:", surv_cat, "\n")
  
  # Save dataframe row into list
  results_list[[sp]] <- data.frame(
    Species = sp,
    shape_type = round(shape_val, 3),
    surv_type = surv_cat,
    stringsAsFactors = FALSE
  )
}

# Combine all loop outputs into one complete dataframe
surv_summary <- bind_rows(results_list)





# ----------- PLOTS -----------

# 1. Step line plot using relative age (proportion of lifespan)
ggplot(alive_data, aes(x = relative_age, y = proportion_alive)) +
  geom_point(color = "green", size = 2) +
  geom_line(color = "blue", size = 1) +
  ggtitle("Normalized Survivorship Curve for Mammals") +
  xlab("Proportion of Maximum Lifespan") +
  ylab("Proportion Alive") +
  scale_y_log10() +  # Optional: log scale on y-axis for better visualization
  theme_cowplot(12) +
  theme(plot.title = element_text(size = 12))


# 2. Smoothed line plot using relative age and count_alive
ggplot(alive_data, aes(x = relative_age, y = count_alive)) +
  geom_point(color = "brown", size = 1) +  # Points show raw data
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", size = 1, se = FALSE) +
  ggtitle("Normalized Survivorship Curve (Smoothed) for Mammals") +
  xlab("Proportion of Maximum Lifespan") +
  ylab("Count Alive") +
  scale_y_log10() +  # Optional: log scale on y-axis
  scale_x_continuous(limits = c(0, 1)) +
  theme_cowplot(12) +
  theme(plot.title = element_text(size = 12))



# Survivorship vector (proportion_alive from your plot data)
lx <- alive_data$proportion_alive
lx <- lx / max(lx)  # Ensure normalization

# Load rage just to be sure


shape_type <- shape_surv(lx)
cat("Standardized AUC (Type I to III scale):", round(shape_type, 3), "\n")


surv_type <- case_when(
  shape_type >= 0.3 ~ "Type I (late mortality, senescence)",
  shape_type >= 0.1 & shape_type < 0.3 ~ "Trending toward Type I",
  shape_type > -0.1 & shape_type < 0.1 ~ "Type II (constant mortality)",
  shape_type > -0.3 & shape_type <= -0.1 ~ "Trending toward Type III",
  shape_type <= -0.3 ~ "Type III (early mortality)"
)


print(surv_type)


# Entropy (Demetrius' H)
H <- entropy_k(lx)
cat("Entropy (H):", round(H, 3), "\n")


# Create a directory to store plots (optional)
if (!dir.exists("species_plots")) dir.create("species_plots")

# Loop through each unique species
valid_species <- cutdata %>%
  group_by(Species) %>%
  summarize(n = n()) %>%
  filter(n > 20) %>%
  pull(Species)


for (spec in unique(valid_species)) {
  
  # Filter data for the current species
  species_data <- cutdata %>% filter(Species == spec)
  
  # Recalculate survivorship curve for that species
  time_steps <- seq(0, 1, by = 0.01)
  alive_counts <- sapply(time_steps, function(x) {
    sum(species_data$relative_age > x)
  })
  
  alive_data <- data.frame(
    relative_age = time_steps,
    count_alive = alive_counts
  ) %>% mutate(proportion_alive = count_alive / max(count_alive))
  
  # Create ggplot
  p <- ggplot(alive_data, aes(x = relative_age, y = proportion_alive)) +
    geom_line(color = "blue", size = 1) +
    ggtitle(paste("Survivorship Curve -", spec)) +
    xlab("Proportion of Maximum Lifespan") +
    ylab("Proportion Alive") +
    theme_cowplot(12) +
    theme(plot.title = element_text(size = 10)) +
    scale_y_log10()
  
  # Save the plot
  ggsave(
    filename = paste0("species_plots_Aves/", gsub(" ", "_", spec), "_curve.png"),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}
