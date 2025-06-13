# Load the plm package
library(plm)

# Assuming you have a dataframe 'panel_data' with id and time variables
# id: the individual dimension (e.g., patient_id)
# time: the time dimension (e.g., year)

# Convert the data to a panel data frame (pdata.frame)
pd <- pdata.frame(panel_data, index = c("id", "time"))

# Fit a basic panel model
# Fixed effects model
fe_model <- plm(total_annual_costs ~ age + number_of_visits + chronic_condition, data = pd, model = "within")

# Random effects model
re_model <- plm(total_annual_costs ~ age + number_of_visits + chronic_condition, data = pd, model = "random")

# Check for model suitability with a Hausman test
hausman_test <- phtest(fe_model, re_model)

# Summary of the preferred model based on test results
summary(fe_model) # or summary(re_model)
