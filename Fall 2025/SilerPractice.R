

library(fmsb)

life_table <- data.frame(
  x  = 0:10,
  qx = c(0.10, 0.11, 0.10, 0.07, 0.07, 
         0.15, 0.25, 0.35, 0.40, 0.58, 1.00)
)

# Build lx from qx
lx <- numeric(length(life_table$qx))
lx[1] <- 1
for (i in 2:length(lx)) {
  lx[i] <- lx[i-1] * (1 - life_table$qx[i-1])
}
life_table$lx <- lx

print(life_table)

plot(life_table$x, life_table$lx, type="b",
     main="Survivorship curve", xlab="Age", ylab="lx")

# Safer fit with starting parameters
res <- fitSiler(c(0.1, 0.1, 0.01, 0.1, 0.01), life_table$lx)

print(res)



