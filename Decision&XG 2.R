# Load necessary libraries
library(tidyverse)
library(caret)
library(randomForest)
library(xgboost)

# Importing data
healthcare_costs <- read_csv("insurance.csv")

# Data Cleaning: Convert categorical variables to factors
healthcare_costs <- healthcare_costs %>%
  mutate(
    sex = as.factor(sex),
    smoker = as.factor(smoker),
    region = as.factor(region)
  )

# Exploratory Data Analysis
ggplot(healthcare_costs, aes(x = age, y = charges)) +
  geom_boxplot() +
  facet_wrap(~ smoker) + 
  labs(title = "Healthcare Charges by Age and Smoking Status")

# Feature Engineering: Convert 'sex', 'smoker', and 'region' to numeric for modeling
healthcare_costs <- healthcare_costs %>%
  mutate_at(vars(sex, smoker, region), as.numeric)

# Data Splitting
set.seed(123)
training_rows <- createDataPartition(healthcare_costs$charges, p = 0.8, list = FALSE)
train_data <- healthcare_costs[training_rows, ]
test_data <- healthcare_costs[-training_rows, ]

# Model Fitting: Random Forest as an example
set.seed(123)
rf_model <- randomForest(charges ~ ., data = train_data, ntree = 100)

# Model Evaluation
rf_predictions <- predict(rf_model, test_data)
rf_results <- data.frame(Actual = test_data$charges, Predicted = rf_predictions)
postResample(pred = rf_predictions, obs = test_data$charges)

# Assuming rf_predictions are the predictions from your random forest model
rf_mse <- mean((rf_predictions - test_data$charges)^2)
rf_rmse <- sqrt(rf_mse)
rf_mae <- mean(abs(rf_predictions - test_data$charges))
rf_r2 <- cor(rf_predictions, test_data$charges)^2

#Feature Importance
importance(rf_model)
varImpPlot(rf_model)

# XGBoost Model for comparison
set.seed(123)
# Prepare the data for xgboost
xgb_train_data <- as.matrix(train_data %>% select(-charges))
xgb_train_label <- train_data$charges
dtrain <- xgb.DMatrix(data = xgb_train_data, label = xgb_train_label)

xgb_model <- xgboost(data = dtrain, nrounds = 100, objective = "reg:squarederror")

# Predict with XGBoost
xgb_test_data <- as.matrix(test_data %>% select(-charges))
dtest <- xgb.DMatrix(data = xgb_test_data)
xgb_predictions <- predict(xgb_model, dtest)
xgb_results <- data.frame(Actual = test_data$charges, Predicted = xgb_predictions)
postResample(pred = xgb_predictions, obs = test_data$charges)

# Assuming xgb_predictions are the predictions from your XGBoost model
xgb_mse <- mean((xgb_predictions - test_data$charges)^2)
xgb_rmse <- sqrt(xgb_mse)
xgb_mae <- mean(abs(xgb_predictions - test_data$charges))
xgb_r2 <- cor(xgb_predictions, test_data$charges)^2

model_performance <- data.frame(
  Model = c("Linear Regression", "Random Forest", "XGBoost"),
  MSE = c(lm_mse, rf_mse, xgb_mse),
  RMSE = c(lm_rmse, rf_rmse, xgb_rmse),
  MAE = c(lm_mae, rf_mae, xgb_mae),
  R2 = c(lm_r2, rf_r2, xgb_r2)
)

# Print the summary table
print(model_performance)


