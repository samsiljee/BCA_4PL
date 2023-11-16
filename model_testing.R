library(tidyverse)
library(drc)

# Creat fake data
data <- data.frame(Absorbance = c(0.988, 0.7665, 0.645, 0.45534, 0.2332, 0.5544, 0.433, 0.4432, 0.7664),
                   Concentration = c(10, 8, 6, 5, rep(NA, 5)),
                   Type = c(rep("Standard", 4), rep("Sample", 5)))

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
