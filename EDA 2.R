# Load necessary libraries
library(tidyverse)
library(readr)
library(ggplot2)
install.packages("GGally")
library(GGally)

# Importing data
healthcare_costs <- read_csv("insurance.csv")

# Data summary
glimpse(healthcare_costs)
summary(healthcare_costs)

# Basic EDA
healthcare_costs %>%
  ggplot(aes(x = age, y = charges)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Healthcare Costs by Age", x = "Age", y = "Annual Healthcare Costs")

# Checking for missing values
sum(is.na(healthcare_costs$charges))

# Histograms for numerical variables
healthcare_costs %>%
  select(age, bmi, children, charges) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Distributions of Numerical Variables")

# Boxplots for numerical variables by sex
healthcare_costs %>%
  select(age, bmi, children, charges, sex) %>%
  gather(key = "variable", value = "value", -sex) %>%
  ggplot(aes(x = variable, y = value, fill = sex)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme_minimal() +
  labs(title = "Boxplots of Numerical Variables by Sex")

# Count plot for categorical variables
healthcare_costs %>%
  select(sex, smoker, region) %>%
  gather(key = "variable", value = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_bar() +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Counts of Categorical Variables")

# Scatter plots for numerical variables against charges
ggpairs(healthcare_costs, columns = c("age", "bmi", "children", "charges"), aes(color = smoker))

# Correlation matrix for numerical variables
healthcare_costs %>%
  select(age, bmi, children, charges) %>%
  cor() %>%
  as.data.frame() %>%
  round(2)

# Save plots if necessary
ggsave("numerical_variable_distributions.png")
ggsave("numerical_variables_by_sex.png")
ggsave("categorical_variable_counts.png")
ggsave("scatter_plots.png")
