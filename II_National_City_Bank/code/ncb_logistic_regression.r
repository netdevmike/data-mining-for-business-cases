library(xgboost)
library(dplyr)
library(readr)
library(ggplot2)

# Data Preprocessing
# Load data
current_customers <- read_csv("/cases/II_National_City_Bank/training/CurrentCustomerMktgResults.csv")
household_data <- read_csv("/cases/II_National_City_Bank/training/householdAxiomData.csv")
credit_data <- read_csv("/cases/II_National_City_Bank/training/householdCreditData.csv")
vehicle_data <- read_csv("/cases/II_National_City_Bank/training/householdVehicleData.csv")
prospective_customers <- read_csv("/cases/II_National_City_Bank/ProspectiveCustomers.csv")

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

# Logistic Regression Model

train_data <- full_training_data[common_cols]
train_labels <- full_training_data$Y_AcceptedOffer

train_data <- cbind(train_data, Y_AcceptedOffer = train_labels)

# Fit the logistic regression model
logistic_model <- glm(Y_AcceptedOffer ~ ., data = train_data, family = binomial())

# Predict on test data
test_data <- full_test_data[common_cols]
test_data$Y_AcceptedOffer_prob <- predict(logistic_model, newdata = test_data, type = "response")

# Select top 100 prospects
top_prospects <- test_data %>%
  arrange(desc(Y_AcceptedOffer_prob)) %>%
  head(100)

# Output the top 100 prospects
write_csv(top_prospects, "top_100_prospects_logistic.csv")

coefficients <- summary(logistic_model)$coefficients
print(coefficients)

importance <- abs(coefficients[, "Estimate"])
names(importance) <- rownames(coefficients)
importance <- sort(importance, decreasing = TRUE)

# Display the sorted importance scores
print(importance)

library(ggplot2)
coefficients_df <- as.data.frame(coefficients)
coefficients_df$Feature <- rownames(coefficients_df)
ggplot(coefficients_df, aes(x = reorder(Feature, Estimate), y = Estimate)) +
  geom_col() +
  coord_flip() +  #
  labs(x = "Feature", y = "Coefficient", title = "Feature Importance from Logistic Regression") +
  theme_minimal()
