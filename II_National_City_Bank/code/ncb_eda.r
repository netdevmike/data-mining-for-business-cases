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

# EDA
# Overview of the dataset
categorical_features <- c("Communication", "LastContactMonth", "past_Outcome", "headOfhouseholdGender", "EstRace", "Job", "Marital", "Education", "carMake", "carModel")
# Create bar plots to show the frequency of categories for each variable.
for (feature in categorical_features) {
  plot <- full_training_data %>%
    ggplot(aes_string(x = feature, fill = 'factor(Y_AcceptedOffer)')) +
    geom_bar(position = 'dodge') +
    scale_fill_manual(values = c("#F8766D", "#00BFC4"), labels = c("Not Accepted", "Accepted")) +
    labs(fill = "Offer Status", x = feature, y = "Count", title = paste('Offer Acceptance by', feature)) +
    theme_minimal(base_size = 14) +
    theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(paste0("bar_plot_", feature, ".png"), plot = plot, device = "png", width = 12, height = 8)
}