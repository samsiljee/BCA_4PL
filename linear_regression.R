# Script to run basic linear regression on the standards
# Sam Siljee
# Created 1st November 2023

# Load data
dat <- read.csv("standards.csv")

# run linear regression
regression <- lm(absorbance ~ concentration, data = dat)

# return the results
print(regression$coefficients)
