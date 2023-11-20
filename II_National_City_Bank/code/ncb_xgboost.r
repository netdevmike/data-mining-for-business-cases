library(xgboost)
library(dplyr)
library(readr)
library(ggplot2)

# Data Preprocessing
# Load data
current_customers <- read_csv("/Users/chararam/Documents/GitHub/data-mining-for-business-CSCI-E-96/cases/II National City Bank/training/CurrentCustomerMktgResults.csv")
household_data <- read_csv("/Users/chararam/Documents/GitHub/data-mining-for-business-CSCI-E-96/cases/II National City Bank/training/householdAxiomData.csv")
credit_data <- read_csv("/Users/chararam/Documents/GitHub/data-mining-for-business-CSCI-E-96/cases/II National City Bank/training/householdCreditData.csv")
vehicle_data <- read_csv("/Users/chararam/Documents/GitHub/data-mining-for-business-CSCI-E-96/cases/II National City Bank/training/householdVehicleData.csv")
prospective_customers <- read_csv("/Users/chararam/Documents/GitHub/data-mining-for-business-CSCI-E-96/cases/II National City Bank/ProspectiveCustomers.csv")

# Merge data
full_training_data <- current_customers %>%
  left_join(household_data, by = "HHuniqueID") %>%
  left_join(credit_data, by = "HHuniqueID") %>%
  left_join(vehicle_data, by = "HHuniqueID")

full_test_data <- prospective_customers %>%
  left_join(household_data, by = "HHuniqueID") %>%
  left_join(credit_data, by = "HHuniqueID") %>%
  left_join(vehicle_data, by = "HHuniqueID")

# Feature Engineering
# Convert character columns which are categorical to factors and then to numeric in training data
categorical_columns_train <- c("Communication", "LastContactMonth", "past_Outcome", "headOfhouseholdGender", "EstRace", "Job", "Marital", "Education", "carMake", "carModel")

full_training_data[categorical_columns_train] <- lapply(full_training_data[categorical_columns_train], function(x) as.numeric(as.factor(x)))

# Convert the target variable to binary in the training data
full_training_data$Y_AcceptedOffer <- ifelse(full_training_data$Y_AcceptedOffer == "Accepted", 1, 0)

# Exclude non-informative character columns if they can't be used as factors
full_training_data <- full_training_data %>% select(-c("HHuniqueID", "annualDonations")) # Assuming these are non-categorical


# Convert target variable to binary in training data
full_training_data$Y_AcceptedOffer <- ifelse(full_training_data$Y_AcceptedOffer == "Accepted", 1, 0)

# Repeat the same for test data (but without the target variable)
full_test_data[categorical_columns_train] <- lapply(full_test_data[categorical_columns_train], function(x) as.numeric(as.factor(x)))

# Exclude the same non-informative character columns
full_test_data <- full_test_data %>% select(-c("HHuniqueID", "annualDonations")) # Assuming these are non-categorical

# Model

# Prepare matrices for XGBoost, ensuring both have the same columns for features
common_cols <- setdiff(intersect(names(full_training_data), names(full_test_data)), "Y_AcceptedOffer")

train_matrix <- xgb.DMatrix(data = as.matrix(full_training_data[common_cols]), label = full_training_data$Y_AcceptedOffer)
test_matrix <- xgb.DMatrix(data = as.matrix(full_test_data[common_cols]))

# Train the model
params <- list(booster = "gbtree", objective = "binary:logistic", eta = 0.1, max_depth = 6)
model <- xgb.train(params, train_matrix, nrounds = 100)

# Predict on test data
predictions <- predict(model, test_matrix)

# Attach predictions to the test data
full_test_data$Y_AcceptedOffer_prob <- predictions

# Select top 100 prospects
top_prospects <- full_test_data %>%
  arrange(desc(Y_AcceptedOffer_prob)) %>%
  head(100)

# Output the top 100 prospects
write_csv(top_prospects, "top_100_prospects_xgboost.csv")

# Variable Importance
importance_matrix <- xgb.importance(model = model)
print(importance_matrix)

# Visualize the feature importance
xgb.plot.importance(importance_matrix)
