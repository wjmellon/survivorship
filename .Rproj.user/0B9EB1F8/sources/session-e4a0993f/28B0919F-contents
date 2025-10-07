
data <- read.csv("records.csv")

# lowest value
min(data$age_months, na.rm = TRUE)

# highest value
max(data$age_months, na.rm = TRUE)

# row with lowest max_longevity
data$ID[which.min(data$age_months)]

# row with highest max_longevity
data$ID[which.max(data$age_months)]


# top 3 species with highest max_longevity
data$Species[order(-data$max_longevity)][1:3]

# top 3 species with lowest max_longevity
data$Species[order(data$max_longevity)][1:3]


# row with lowest max_longevity
data$ID[which.min(data$age_months)]

# row with highest max_longevity
data$ID[which.max(data$age_months)]


# top 3 species with their max_longevity
head(data[order(-data$age_months), c("Species", "age_months")], 3)

# bottom 3 species with their max_longevity
head(data[order(data$age_months), c("Species", "age_months")], 3)
