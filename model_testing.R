library(tidyverse)
library(drc)

# Create fake data
data <- data.frame(Absorbance = c(0.988, 0.7665, 0.645, 0.45534, 0.2332, 0.5544, 0.433, 0.4432, 0.7664),
                   Concentration = NA,
                   Type = 'Sample')

# Read in test standards
standards <- read.csv("standards.csv")

# Combine
data <- rbind(
  data,
  data.frame(
    Absorbance = standards$absorbance,
    Concentration = standards$concentration,
    Type = "Standard"
  )
)

# Make model
model <- drm(Concentration ~ Absorbance,
      data = data %>%
        filter(Type == "Standard") %>%
        dplyr::select(Concentration, Absorbance),
      fct = LL.4())

# Run predictions
predictions <- predict(model,
          newdata = data %>%
            filter(Type == "Sample") %>%
            dplyr::select(Concentration, Absorbance),
          interval = "confidence")

# Make x and y vectors to plot the model
x_values <- seq(
  min(data$Absorbance)*0.8,
  max(data$Absorbance)*1.2,
  length.out = 50)

y_values <- predict(model, newdata = data.frame(Absorbance = x_values))

# Make a plot
data.frame(Absorbance = x_values, Concentration = y_values) %>%
ggplot(aes(x = Absorbance, y = Concentration)) +
  geom_line() +
  geom_point(
    data = filter(data, Type == "Standard"),
    aes(x = Absorbance, y = Concentration),
    col = 'red') +
  geom_point(
    data = filter(data, Type == "Sample"),
    aes(x = Absorbance, y = Concentration),
    col = 'blue') +
  theme_bw()
