#MultiLinearRegression
# Load necessary libraries
library(tidyverse)

# Read the dataset
insurance <- read_csv("insurance.csv")

# Convert the necessary variables to factors
insurance <- insurance %>%
  mutate(
    sex = as.factor(sex),
    smoker = as.factor(smoker),
    region = as.factor(region)
  )

# Splitting the data into training and testing sets
set.seed(123) # Set a random seed for reproducibility
training_indices <- createDataPartition(insurance$charges, p = 0.8, list = FALSE)
train_data <- insurance[training_indices, ]
test_data <- insurance[-training_indices, ]

# Fit a multiple linear regression model
lm_model <- lm(charges ~ age + sex + bmi + children + smoker + region, data = train_data)

# Summary of the model
summary(lm_model)

# Predicting on the test set
lm_predictions <- predict(lm_model, newdata = test_data)

# Evaluate the model's performance
lm_mse <- mean((test_data$charges - lm_predictions)^2)
lm_rmse <- sqrt(mean((test_data$charges - lm_predictions)^2))
lm_mae <- mean(abs(test_data$charges - lm_predictions))
lm_r2 <- summary(lm_model)$r.squared

# Print the performance metrics
cat("Linear Regression RMSE:", lm_rmse, "\nMAE:", lm_mae, "\n")

#TOBIT Model
# Load the AER package for Tobit models
library(AER)

# Assuming that the 'insurance' dataframe has already been loaded and the factors have been set

# Fit a Tobit model to the data
# Assuming 'charges' is left-censored at 0 for this example
tobit_model <- tobit(charges ~ age + sex + bmi + children + smoker + region, data = insurance, left = 0)

# Summary of the Tobit model
summary(tobit_model)

# Predicting on the test set
tobit_predictions <- predict(tobit_model, newdata = test_data, type = "response")

# Evaluate the Tobit model's performance
tobit_rmse <- sqrt(mean((test_data$charges - tobit_predictions)^2))
tobit_mae <- mean(abs(test_data$charges - tobit_predictions))

# Print the performance metrics
cat("Tobit Model RMSE:", tobit_rmse, "\nMAE:", tobit_mae, "\n")
