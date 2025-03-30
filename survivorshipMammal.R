# Load required libraries
library(ggplot2)
library(dplyr)
library(cowplot)
library(Rage)

# Load the dataset
data <- read.csv("records.csv")


# Filter the dataset for Mammals with Necropsy data and exclude infants
cutdata <- data %>%
  filter(Necropsy == 1) %>%
  filter(Class == "Mammalia") %>%
  filter(Infant == 0)

cutdata <- cutdata %>%
  mutate(
    age_months = as.numeric(age_months),
    max_longevity = as.numeric(max_longevity)
  )


# Select relevant columns (assuming columns 3 = Species, 24 = age_months, 48 = max_longevity)
cutdata <- cutdata[, c(3, 28, 48)]
colnames(cutdata) <- c("age_months", "Species", "max_longevity")


# Clean data: remove individuals with invalid ages
cutdata$age_months[cutdata$age_months <= 0] <- NA
cutdata <- na.omit(cutdata)

# Clean data: remove individuals with invalid ages
cutdata$max_longevity[cutdata$max_longevity <= 0] <- NA
cutdata <- na.omit(cutdata)


# Create a relative_age column: individual age divided by species-specific maximum longevity
cutdata <- cutdata %>%
  mutate(relative_age = age_months / max_longevity)

# Define relative age time steps (0 to 1 by increments of 0.01)
time_steps <- seq(0, 1, by = 0.01)  # 1% increments of lifespan

# Calculate the number of individuals alive at each relative age step
alive_counts <- sapply(time_steps, function(x) {
  sum(cutdata$relative_age > x)  # Count individuals alive beyond time step x
})

# Prepare a dataframe for plotting
alive_data <- data.frame(
  relative_age = time_steps,
  count_alive = alive_counts
)

# Normalize counts to a proportion of the original population
alive_data <- alive_data %>%
  mutate(proportion_alive = count_alive / max(count_alive))  # Divide by initial population size


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