
library(dplyr)
library(fmsb)
library(readr)

# Read your full life table
df <- read_csv("Fall 2025/Siler/life_tables_all_species.csv")

res <- fitSiler(df$q_x)
FLAG <- res[7]
while (FLAG>0) {
  res <- fitSiler(res[1:5], df$q_x)
  FLAG <- res[7]
}
print(res)