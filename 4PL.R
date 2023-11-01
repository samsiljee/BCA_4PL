# Script to run 4 parameter logistic regression on the standards
# Sam Siljee
# Created 1st November 2023

# Load libraries
library(drc)

# Load data
dat <- read.csv("standards.csv")

# Fit 4PL curve
fit <- drm(absorbance ~ concentration, data = dat, fct = LL.4())

# View the summary of the fitted curve
summary(fit)

# Generate a vector of some random test data
test_dat <- c("Sample_A" = 0.5667, "Sample_B" = 1.004, "Sample_c" = 0.2332)

# Predict concentrations of samples
predicted_concentrations <- predict(fit, newdata = data.frame(responses = test_dat))

# Plot the fitted curve
plot(fit, log = "x", col = "red", xlab = "Concentration", ylab = "Absorbance")

# Add original data points to the plot
points(dat$concentration, dat$absorbance, pch = 16, col = "black")
