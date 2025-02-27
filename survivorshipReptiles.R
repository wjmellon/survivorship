library(ggplot2)
library(dplyr)
library(ggrepel)  # For non-overlapping labels

# Load the dataset
data <- malignant_data

# Filter data for Mammalia class
cutdata <- filter(data, Necropsy == 1)
cutdata <- filter(cutdata, Class == "Mammalia")  # Filter for Mammalia
cutdata <- filter(cutdata, Infant == 0)  # Remove infants

<<<<<<< HEAD
# Ensure columns used for lymphoma deaths are correct
# Assuming lymphoma deaths data is in a column named "Lymphoma_Deaths"
cutdata <- cutdata[, c(3, 24, 48)]  # Assuming these columns contain age_months, max_longevity, lymphoma deaths
=======
# Filter for Class Reptilia and relevant columns
cutdata <- filter(cutdata, Class == "Reptilia")


cutdata <- filter(cutdata, Infant == 0)




#cutdata <- filter(cutdata, com == "Reptilia")

cutdata <- cutdata[, c(3, 24,48)]  # Assuming columns 3 and 24 are `age_months` and `max_longevity`
>>>>>>> 2af795a5e2cd2eddb802c707ab84d6a42064ad1d
cutdata$age_months[cutdata$age_months <= 0] <- NA
cutdata <- na.omit(cutdata)

# Check if Lymphoma_Deaths is a valid column (modify if needed)
colnames(cutdata)  # Verify the names of the columns (adjust accordingly)

<<<<<<< HEAD
# Scatterplot with lymphoma deaths for Mammalia species
ggplot(cutdata, aes(x = Lymphoma_Deaths)) +
  geom_jitter(aes(y = 0), width = 0.2, height = 0, size = 3, alpha = 0.7, color = "red") +
  geom_text_repel(aes(y = 0, label = Species), size = 3, nudge_y = 0.1) +  # Label all species
  theme_minimal(base_size = 14) +
  labs(title = "Lymphoma Death Frequency Across Mammal Species",
       x = "Number of Lymphoma Deaths", y = "") +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank())
=======
# Calculate the number of individuals alive at each time step
alive_counts <- sapply(time_steps, function(x) {
  sum(cutdata$age_months > x)  # Count individuals with age > x
})

# Prepare a dataframe for plotting
alive_data <- data.frame(
  age = time_steps,
  count_alive = alive_counts
)
# Normalize counts to a proportion of the original population
alive_data <- alive_data %>%
  mutate(proportion_alive = count_alive / max(count_alive))  # Divide by initial population size




max_age <- max(cutdata$max_longevity, na.rm = TRUE)

#step line
ggplot(alive_data, aes(x = age, y = (proportion_alive))) +
  geom_point(color = "green", size = 2) +
  geom_line(color = "blue", size = 1) +
  ggtitle("Survivorship Curve for Reptiles with Malignancy") +
  xlab("Age (months)") +
  ylab("log(Proportion Alive)") +
  scale_y_log10() + # Set x-axis limits
  theme_cowplot(12)


#smooth line
ggplot(alive_data, aes(x = age, y = (count_alive))) +
  geom_point(color = "pink", size = 1) +  # Keep points for individual data
  geom_smooth(method = "gam", formula = y ~ s(x, bs = "cs"), color = "blue", size = 1, se = FALSE) +
  ggtitle("Survivorship Curve for Reptiles with Malignancy") +
  xlab("Age (C)") +
  ylab("log(Count Alive)") +
  scale_y_log10() +  # Set x-axis limits
  theme_cowplot(12)
summary(cutdata$age_months)
head(alive_data)
tail(alive_data)
>>>>>>> 2af795a5e2cd2eddb802c707ab84d6a42064ad1d
