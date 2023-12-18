# Install necessary libraries
install.packages(c('dplyr', 'corrplot', 'ggplot2', 'xgboost', 'tidyverse', 'caret', 'randomForest', 'glmnet', 'kableExtra'))

# Load necessary libraries
packages <- c('dplyr', 'corrplot', 'naniar', 'viridis', 'missForest', 'ggplot2', 
              'xgboost', 'tidyverse', 'caret', 'randomForest', 'glmnet', 'kableExtra')

for(pkg in packages) {
  library(pkg, character.only = TRUE)
}

setwd("/.../cases/III Household Spend")

read_csv_from_path <- function(filename) {
  base_path <- "/.../cases/III Household Spend/studentTables/"
  full_path <- paste0(base_path, filename)
  read.csv(full_path)
}

# Read the datasets
consumer_data <- read_csv_from_path("consumerData_training15K_studentVersion.csv")
donations_data <- read_csv_from_path("DonationsData_training15K_studentVersion.csv")
inHouse_data <- read_csv_from_path("inHouseData_training15K_studentVersion.csv")
magazine_data <- read_csv_from_path("magazineData_training15K_studentVersion.csv")
political_data <- read_csv_from_path("politicalData_training15K_studentVersion.csv")

# Read the testing datasets
testing_data_consumer <- read_csv_from_path("consumerData_testing5K_studentVersion.csv")
testing_data_donations <- read_csv_from_path("DonationsData_testing5K_studentVersion.csv")
testing_data_inHouse <- read_csv_from_path("inHouseData_testing5K_studentVersion.csv")
testing_data_magazine <- read_csv_from_path("magazineData_testing5K_studentVersion.csv")
testing_data_political <- read_csv_from_path("politicalData_testing5K_studentVersion.csv")

# Read the prospect datasets
prospect_data_consumer <- read_csv_from_path("consumerData_prospects6K_studentVersion.csv")
prospect_data_donations <- read_csv_from_path("DonationsData_prospects6K_studentVersion.csv")
prospect_data_inHouse <- read_csv_from_path("inHouseData_prospects6K_studentVersion.csv")
prospect_data_magazine <- read_csv_from_path("magazineData_prospects6K_studentVersion.csv")
prospect_data_political <- read_csv_from_path("politicalData_prospects6K_studentVersion.csv")


# Merge datasets using tmpID as the key
merged_training <- consumer_data %>%
  left_join(donations_data, by = "tmpID") %>%
  left_join(inHouse_data, by = "tmpID") %>%
  left_join(magazine_data, by = "tmpID") %>%
  left_join(political_data, by = "tmpID")

# View the number of rows and columns of merged dataset
dim(merged_training)
# Save the merged dataset to a CSV file
write.csv(merged_training, "merged_training_dataset.csv", row.names = FALSE)

# Merge testing datasets using tmpID as the key
merged_testing <- testing_data_consumer %>%
  left_join(testing_data_donations, by = "tmpID") %>%
  left_join(testing_data_inHouse, by = "tmpID") %>%
  left_join(testing_data_magazine, by = "tmpID") %>%
  left_join(testing_data_political, by = "tmpID")

# View the rows and columns of merged testing dataset
dim(merged_testing)

# Save the merged testing dataset to a CSV file
write.csv(merged_testing, "merged_testing_dataset.csv", row.names = FALSE)

# Merge prospect datasets using tmpID as the key
merged_prospects <- prospect_data_consumer %>%
  left_join(prospect_data_donations, by = "tmpID") %>%
  left_join(prospect_data_inHouse, by = "tmpID") %>%
  left_join(prospect_data_magazine, by = "tmpID") %>%
  left_join(prospect_data_political, by = "tmpID")

# View the number of columns and rows of the merged prospects dataset
dim(merged_prospects)

# Save the merged prospects dataset to a CSV file
write.csv(merged_prospects, "merged_prospects_dataset.csv", row.names = FALSE)

# Display summary statistics
summary(merged_training)

# Visualize distribution of a numerical variable 
hist(merged_training$MedianEducationYears, main = "MedianEducationYears")

# Visualize distribution of a numerical variable (replace 'variable_name' with an actual variable)
hist(merged_training$ISPSA, main = "ISPSA")

# Removing rows with null values from training data
complete_rows <- complete.cases(merged_training)

# Subset the dataset to keep only complete rows
training_cleaned_dataset <- merged_training[complete_rows, ]

dim(training_cleaned_dataset)

# Removing rows with null values from testing data
complete_rows <- complete.cases(merged_testing)

# Subset the dataset to keep only complete rows
testing_cleaned_dataset <- merged_testing[complete_rows, ]

dim(testing_cleaned_dataset)

# Removing rows with null values from prospects data
complete_rows <- complete.cases(merged_prospects)

# Subset the dataset to keep only complete rows
prospects_cleaned_dataset <- merged_prospects[complete_rows, ]

dim(prospects_cleaned_dataset)

# Checking the number of missing values in each row of training data
sum(rowSums(is.na(training_cleaned_dataset)))

# Find the number of missing values in each row of testing data
sum(rowSums(is.na(testing_cleaned_dataset)))

# Find the number of missing values in each row of prospects data
sum(rowSums(is.na(prospects_cleaned_dataset)))

# Exclude non-numeric columns
numeric_columns <- sapply(training_cleaned_dataset, is.numeric)
numeric_data <- training_cleaned_dataset[, numeric_columns]

# Display summary statistics
summary(numeric_data)

# Visualize correlation matrix
cor_matrix <- cor(numeric_data)
corrplot(cor_matrix)

# Bar Plot for a Categorical Variable
barplot(table(merged_training$NetWorth ), main = "Bar Plot", xlab = "NetWorth ")

# Boxplot for a Numeric Variable
boxplot(merged_training$storeVisitFrequency, main = "Boxplot", ylab = "storeVisitFrequency")

miss_forest_data <- missForest(numeric_data)$ximp
miss_forest_data <- data.frame(miss_forest_data)

gg_miss_var(miss_forest_data)

# Visualize the distribution of the target variable (household revenue)
ggplot(training_cleaned_dataset, aes(x = yHat)) +
  geom_histogram(binwidth = 100, fill = "blue", color = "black") +
  labs(title = "Distribution of average household spend with BBY in USD", x = "yHat", y = "Frequency")

# Standardizing prospect data 
# Loop through columns
for (col in names(prospects_cleaned_dataset)) {
  # Check if the column has character data type
  if (is.character(prospects_cleaned_dataset[[col]])) {
    # Label encode using factor
    prospects_cleaned_dataset[[col]] <- as.numeric(factor(prospects_cleaned_dataset[[col]]))
  }
}

# Check the updated dataset
str(prospects_cleaned_dataset)

# Standardizin g testing data 
# Loop through columns
for (col in names(testing_cleaned_dataset)) {
  # Check if the column has character data type
  if (is.character(testing_cleaned_dataset[[col]])) {
    # Label encode using factor
    testing_cleaned_dataset[[col]] <- as.numeric(factor(testing_cleaned_dataset[[col]]))
  }
}

# Check the updated dataset
str(testing_cleaned_dataset)

# Standardization 
# Loop through columns
for (col in names(training_cleaned_dataset)) {
  # Check if the column has character data type
  if (is.character(training_cleaned_dataset[[col]])) {
    # Label encode using factor
    training_cleaned_dataset[[col]] <- as.numeric(factor(training_cleaned_dataset[[col]]))
  }
}

# Check the updated dataset
str(training_cleaned_dataset)

# XGBoost Model

# Split the data into predictors (X) and response variable (y)
X_train <- training_cleaned_dataset %>% select(-c(tmpID, yHat))
y_train <- training_cleaned_dataset$yHat
X_test <- testing_cleaned_dataset %>% select(-c(tmpID, yHat))
X_prospect <- prospects_cleaned_dataset %>% select(-tmpID)

# Train the XGBoost model
model <- xgboost(data = as.matrix(X_train), 
                 label = y_train, 
                 nrounds = 100, 
                 objective = "reg:squarederror")

# Evaluate the model on the training set
train_predictions <- predict(model, as.matrix(X_train))
rmse_train <- sqrt(mean((train_predictions - y_train)^2))
cat("Root Mean Squared Error on the training set:", rmse_train, "\n")

# Feature Importance
importance <- xgb.importance(model = model)
print(importance)

# Plot Feature Importance
xgb.plot.importance(importance_matrix = importance)

# Make predictions on the test set
test_predictions <- predict(model, as.matrix(X_test))

# Evaluate the model on the test set 
test_actual <- testing_cleaned_dataset$yHat
rmse_test <- sqrt(mean((test_predictions - test_actual)^2))
cat("Root Mean Squared Error on the test set:", rmse_test, "\n")


# Predict household spend on the prospect's data
prospect_predictions <- predict(model, as.matrix(X_prospect))

# Save predictions to CSV
write.csv(data.frame(tmpID = prospects_cleaned_dataset$tmpID, PredictedSpend = prospect_predictions), "/.../cases/III Household Spend/data/ProspectPredictions.csv", row.names = FALSE)

# Train the Random Forest model
rf_model <- randomForest(x = X_train, y = y_train, ntree = 20)

# Evaluate the model on the training set
rf_train_predictions <- predict(rf_model, newdata = X_train)
rf_rmse_train <- sqrt(mean((rf_train_predictions - y_train)^2))
cat("Random Forest - Root Mean Squared Error on the training set:", rf_rmse_train, "\n")

# Make predictions on the test set
rf_test_predictions <- predict(rf_model, newdata = X_test)

# Evaluate the model on the test set 
rf_rmse_test <- sqrt(mean((rf_test_predictions - test_actual)^2))
cat("Random Forest - Root Mean Squared Error on the test set:", rf_rmse_test, "\n")

# Predict household spend on the prospect's data
rf_prospect_predictions <- predict(rf_model, newdata = X_prospect)

# Save predictions to CSV
write.csv(data.frame(tmpID = prospects_cleaned_dataset$tmpID, PredictedSpend = rf_prospect_predictions), "/.../cases/III Household Spend/data/ProspectPredictions_RF.csv", row.names = FALSE)

# Train the Linear Regression model
lr_model <- glmnet(x = as.matrix(X_train), y = y_train, alpha = 0.5)

# Evaluate the model on the training set
lr_train_predictions <- predict(lr_model, newx = as.matrix(X_train))
lr_rmse_train <- sqrt(mean((lr_train_predictions - y_train)^2))
cat("Linear Regression - Root Mean Squared Error on the training set:", lr_rmse_train, "\n")

# Make predictions on the test set
lr_test_predictions <- predict(lr_model, newx = as.matrix(X_test))

# Evaluate the model on the test set 
lr_rmse_test <- sqrt(mean((lr_test_predictions - test_actual)^2))
cat("Linear Regression - Root Mean Squared Error on the test set:", lr_rmse_test, "\n")

# Predict household spend on the prospect's data
lr_prospect_predictions <- predict(lr_model, newx = as.matrix(X_prospect))

# Save predictions to CSV
write.csv(data.frame(tmpID = prospects_cleaned_dataset$tmpID, PredictedSpend = lr_prospect_predictions), "/.../cases/III Household Spend/data/ProspectPredictions_LR.csv", row.names = FALSE)

# Create a data frame for the results
results_df <- data.frame(
  Model = c("XGBoost", "Random Forest", "Linear Regression"),
  Train_RMSE = c(rmse_train, rf_rmse_train, lr_rmse_train),
  Test_RMSE = c(rmse_test, rf_rmse_test, lr_rmse_test)
)

# Add Prospect predictions
results_df$Prospect_Predictions <- c(mean(prospect_predictions), mean(rf_prospect_predictions), mean(lr_prospect_predictions))

# Create a kable table
results_table <- kable(results_df, format = "html", 
                       caption = "Model Comparison Results", align = "c") %>%
  kable_styling(full_width = FALSE)

# Print the table to the console
print(results_table)
