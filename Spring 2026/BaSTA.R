# ============================================================================ #
# COMMENTS: Example code to format lifespan data to BaSTA table.
#           Assumptions: 
#              1.- Your data table is called "myData" (you can change the name 
#                  in the code below)
#              2.- the data consists of individual records including a column 
#                  for lifespans (i.e., age at death in years) and a column 
#                  for cause of death (i.e., cancer vs other) named "cause"
#                  (you can change this in the code). 
# ============================================================================ #
library(BaSTA)
library(dplyr)

data <- read.csv("Spring 2026/BaSTA_birth_dates.csv")

# Number of individuals in your dataset:
n <- nrow(data)

species_list <- split(data, data$Species.x)

# Dummy birth date:
BirthDate <- as.Date(data$birth_date)

# Departure (i.e., death date) (divide by 365.25 if the lifespans are in years)
DepartDate <- as.Date(data$necropsy_date)

data <- data %>%
  mutate(
    cause = as.factor(Malignant.x)
  )


# BaSTA dataset:
bastaDat <- data.frame(ID = 1:n, Birth.Date = BirthDate, 
                       Min.Birth.Date = BirthDate,
                       Max.Birth.Date = BirthDate,
                       Entry.Date = BirthDate,
                       Depart.Date = DepartDate,
                       Depart.Type = rep("D", n),
                       cause = data$cause)


# Run BaSTA on Data:
out <- basta(object = bastaDat, dataType = "census", 
             model = "GO", shape = "bathtub", formulaMort = ~ cause - 1,
             parallel = TRUE, ncpus = 4, nsim = 4)



results <- lapply(species_list, function(df_species) {
  
  n <- nrow(df_species)
  
  # Species-level bounds
  min_birth <- min(df_species$BirthDate, na.rm = TRUE)
  max_birth <- max(df_species$BirthDate, na.rm = TRUE)
  
  bastaDat <- data.frame(
    ID = 1:n,
    Birth.Date = df_species$BirthDate,
    Min.Birth.Date = rep(min_birth, n),
    Max.Birth.Date = rep(max_birth, n),
    Entry.Date = df_species$BirthDate,
    Depart.Date = df_species$DepartDate,
    Depart.Type = rep("D", n),
    cause = df_species$cause
  )
  
  basta(
    object = bastaDat,
    dataType = "census",
    model = "GO",
    shape = "bathtub",
    formulaMort = ~ cause - 1,
    parallel = FALSE,
    nsim = 4
  )
})