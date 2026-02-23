# 1. Load the library
library(BaSTA)

# 2. Load the built-in example dataset (Capture-Mark-Recapture data)
data("bastaCMRdat", package = "BaSTA")

## Check data consistency:
checkedData  <- DataCheck(bastaCMRdat, dataType = "CMR", studyStart = 51, 
                          studyEnd = 70)

# 3. Run the Siler Model
# model = "GO" + shape = "bathtub" = Siler Model
siler_fit <- basta(bastaCMRdat, 
                   model = "GO",            # Base model: Gompertz
                   shape = "bathtub",       # Adds juvenile and constant terms (Siler)
                   studyStart = 51,       # Start year of the study
                   studyEnd = 70,         # End year of the study
                   niter = 100,           # Number of MCMC iterations
                   burnin = 11,           # Initial iterations to discard
                   thinning = 10,           # Save every 10th iteration
                   nsim = 1,                # Number of parallel chains
                   parallel = FALSE)        # Set to TRUE if you have multiple cores

# 4. View the 5 Parameters (a0, a1, c, b0, b1)
summary(siler_fit)

# 5. Visualize the "Bathtub" Mortality Curve
plot(siler_fit, type = "demorates")


## Load data:
data("bastaCensDat", package = "BaSTA")

## Check data consistency:
checkedData  <- DataCheck(bastaCensDat, dataType = "census")

