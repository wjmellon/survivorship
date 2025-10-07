# Count castration status
castration_counts <- table(your_data[,5])  # 5th column is Castrated
print(castration_counts)

# Or with explicit labels:
cat("Castrated (1):", sum(your_data$Castrated == 1, na.rm = TRUE), "\n")
cat("Not Castrated (0):", sum(your_data$Castrated == 0, na.rm = TRUE))