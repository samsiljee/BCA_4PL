# Script to run 4 parameter logistic regression on the standards
# Sam Siljee
# Created 1st November 2023

# Fit 4PL curve
fit <- drm(responses ~ concentrations, data = your_data_frame, fct = LL.4())

# View the summary of the fitted curve
summary(fit)

# Predict concentrations of samples
predicted_concentrations <- predict(fit, newdata = data.frame(responses = sample_responses))

# Plot the fitted curve
plot(fit, log = "x", col = "red", xlab = "Concentration", ylab = "Response")

# Add original data points to the plot
points(concentrations, responses, pch = 16, col = "black")
