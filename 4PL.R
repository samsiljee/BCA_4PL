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
test_dat <- c("Sample_A" = 0.410666667
, "Sample_B" = 0.338666667
, "Sample_c" = 0.2332)

standards_dat <- c(
  A = 1.417666667,
  B = 0.942,
  C = 0.513,
  D = 0.262333333,
  E = 0.139333333,
  "F" = 0.066666667,
  G = 0.025333333
)

# Predict concentrations of samples
predicted_concentrations <- predict(fit, newdata = data.frame(responses = test_dat))

standards_check <- predict(fit, newdata = data.frame(responses = standards_dat))

# Plot the fitted curve
plot(fit, log = "x", col = "red", xlab = "Concentration", ylab = "Absorbance")

# Add original data points to the plot
points(dat$concentration, dat$absorbance, pch = 16, col = "black")


# Second suggestion

# Install and load the drc package
# install.packages("drc")
library(drc)

# Assume you have a data frame 'standards_data' with columns 'Absorbance' and 'Concentration'
# Fit a 4PL model
model_4pl <- drm(concentration ~ absorbance, data = dat, fct = LL.4())

# Extract parameter estimates
parameters <- as.numeric(coefficients(model_4pl))

# Print parameter estimates
print(parameters)

# Predict concentrations from new absorbance readings
new_absorbance <- c(0.410666667, 0.338666667)  # Replace with your actual absorbance readings
predicted_concentrations <- predict(model_4pl, data.frame(absorbance = new_absorbance))

# Print predicted concentrations
print(predicted_concentrations)

