#SVM
# Load necessary library
library(e1071)
library(caret) # For createDataPartition function

# Load dataset
insurance_data <- read.csv("insurance.csv") # Assuming "insurance.csv" is your dataset file

# Convert factors to numeric if necessary
insurance_data$sex <- as.numeric(factor(insurance_data$sex))
insurance_data$smoker <- as.numeric(factor(insurance_data$smoker))
insurance_data$region <- as.numeric(factor(insurance_data$region))

# Feature Engineering: Create interaction term
insurance_data$age_bmi_interaction <- insurance_data$age * insurance_data$bmi

# Splitting the data into training and testing sets
set.seed(123) # for reproducibility
training_index <- createDataPartition(insurance_data$charges, p = 0.8, list = FALSE)
train_data <- insurance_data[training_index, ]
test_data <- insurance_data[-training_index, ]

# Train the SVM model
svm_model <- svm(charges ~ age + sex + bmi + children + smoker + region + age_bmi_interaction,
                 data = train_data,
                 kernel = "radial", # Radial basis kernel for non-linear relationships
                 cost = 10, # Cost parameter for the SVM
                 scale = TRUE) # Scale features before training

# Making predictions on the test set
svm_predictions <- predict(svm_model, test_data)

# Calculating accuracy metrics
svm_rmse <- sqrt(mean((svm_predictions - test_data$charges)^2))
svm_mae <- mean(abs(svm_predictions - test_data$charges))
svm_r_squared <- cor(svm_predictions, test_data$charges)^2

# Print accuracy metrics
print(paste("RMSE of the SVM model:", svm_rmse))
print(paste("MAE of the SVM model:", svm_mae))
print(paste("R-squared of the SVM model:", svm_r_squared))

# Plotting actual vs. predicted charges
plot(test_data$charges, svm_predictions, main = "Actual vs. Predicted Charges (SVM)",
     xlab = "Actual Charges", ylab = "Predicted Charges", col = "blue")
abline(0, 1, col = "red") # Add a diagonal line for reference
legend("topleft", legend = "Ideal Prediction", col = "red", lty = 1, cex = 0.8)

#GBM
# Load necessary libraries
library(gbm)
library(caret)

# Read the insurance dataset
insurance_data <- read.csv("insurance.csv")

# Convert factors to numeric if necessary
insurance_data$sex <- as.numeric(factor(insurance_data$sex))
insurance_data$smoker <- as.numeric(factor(insurance_data$smoker))
insurance_data$region <- as.numeric(factor(insurance_data$region))

# Splitting the data into training and testing sets
set.seed(123) # for reproducibility
training_index <- createDataPartition(insurance_data$charges, p = 0.8, list = FALSE)
train_data <- insurance_data[training_index, ]
test_data <- insurance_data[-training_index, ]

# Feature Engineering (if needed)
# Example: Create interaction terms
train_data$age_bmi_interaction <- train_data$age * train_data$bmi
test_data$age_bmi_interaction <- test_data$age * test_data$bmi

# Training the GBM model
gbm_model <- gbm(charges ~ ., data = train_data, distribution = "gaussian", n.trees = 1000, 
                 interaction.depth = 3, shrinkage = 0.1, verbose = TRUE)

# Making predictions on the test set
gbm_predictions <- predict(gbm_model, newdata = test_data, n.trees = 1000)

# Calculate RMSE
gbm_rmse <- sqrt(mean((gbm_predictions - test_data$charges)^2))
print(paste("RMSE of the GBM model:", gbm_rmse))

# Calculate MAE
gbm_mae <- mean(abs(gbm_predictions - test_data$charges))
print(paste("MAE of the GBM model:", gbm_mae))

# Additional accuracy metrics (if needed)
# Example: R-squared
gbm_r_squared <- cor(gbm_predictions, test_data$charges)^2
print(paste("R-squared of the GBM model:", gbm_r_squared))

# Plotting variable importance
summary(gbm_model)

# Plotting the GBM model (partial dependence plot)
plot(gbm_model, i = "age")

# Performance comparison table
model_comparison <- data.frame(
  Model = c("Linear Regression", "Random Forest", "XGBoost", "Tobit", "SVM", "GBM"),
  RMSE = c(lm_rmse, rf_rmse, xgb_rmse, tobit_rmse, svm_rmse, gbm_rmse),
  MAE = c(lm_mae, rf_mae, xgb_mae, tobit_mae, svm_mae, gbm_mae),
  R2 = c(lm_r2, rf_r2, xgb_r2, NA, svm_r_squared, gbm_r_squared) # NA for Tobit because R2 isn't typically used
)

# Print the comparison table
print(model_comparison)
