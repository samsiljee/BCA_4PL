# Script to run 4 parameter logistic regression on the standards
# Sam Siljee
# Created 1st November 2023

# Load libraries
library(drc)

# Load data
standards_dat <- read.csv("standards.csv")
samples_dat <- read.csv("samples.csv")

# Fit 4PL curve
fit <- drm(concentration ~ absorbance, data = standards_dat, fct = LL.3())

# View the summary of the fitted curve
summary(fit)

# Predict concentrations of samples
predicted_concentrations <- predict(fit, newdata = as.data.frame(samples_dat$absorbance))

standards_check <- predict(fit, newdata = as.data.frame(standards_dat$absorbance))

# calculate R2
cor(fitted(fit), standards_dat$concentration) ^2

# Save the results and correct dilution
write.csv(data.frame(Predicted_c = predicted_concentrations*3), "predictions.csv")

