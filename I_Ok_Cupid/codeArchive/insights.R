# Setting the working directory
setwd("/Users/chararam/Documents/GitHub/data-mining-for-business-CSCI-E-96/cases/I_Ok_Cupid")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggdark)
library(sf)
library(stringr)
library(forcats)
library(maps)



# Reading the data
profiles <- read.csv('profiles.csv')
latlon <- read.csv('LatLon.csv')
addr <- read.csv('addr.csv')
sharedCensus <- read.csv('sharedCensus2010Vars.csv')

# Merge dataframes based on common column 'location'
merged_data <- merge(profiles, latlon, by = "location", all.x = TRUE)
merged_data <- merge(merged_data, addr, by = "location", all.x = TRUE)
merged_data <- merge(merged_data, sharedCensus, by = "location", all.x = TRUE)
# Remove age outliers (109 and 110)
merged_data <- merged_data %>% filter(!(age %in% c(109, 110)))


# Insight 1: Distribution of users by age and orientation (Age 20-40)
age_orientation_plot_adjusted <- merged_data %>%
  filter(age >= 20 & age <= 40) %>%
  ggplot(aes(x = age, fill = orientation)) +
  geom_histogram(position = "stack", binwidth = 1) +  # Adjusted binwidth
  labs(title = "Adjusted Distribution of Users by Age and Orientation (Age 20-40)", x = "Age", y = "Count") +
  ggdark::dark_theme_gray()

print(age_orientation_plot_adjusted)

# Counting number of users for each age
age_counts <- profiles %>%
  group_by(age) %>%
  summarise(count = n())

# Print the results
print(age_counts, n=54)

# Insight 2: Splitting Relationship Status & Education against average Income

# Relationship Status vs. Income
income_status_plot <- merged_data %>%
  group_by(status) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(status, -avg_income), y = avg_income)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Income by Relationship Status", x = "Relationship Status", y = "Average Income") +
  ggdark::dark_theme_gray()

print(income_status_plot)

# Education vs. Income with abbreviated labels
merged_data$education <- fct_lump(merged_data$education, n = 10)
merged_data$education <- factor(merged_data$education, levels = rev(levels(merged_data$education)))

income_education_plot <- merged_data %>%
  group_by(education) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(education, -avg_income), y = avg_income)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Income by Education Level", x = "Education Level", y = "Average Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggdark::dark_theme_gray()

print(income_education_plot)

# Insight 3: Geographic Distribution of Users in the US
# Define latitude and longitude boundaries for continental US
lat_boundary <- c(24.396308, 49.384358)  # Lower and upper latitude boundaries
lon_boundary <- c(-125.001650, -66.934570)  # Left and right longitude boundaries

# Filter the data
filtered_data <- merged_data %>%
  filter(lat >= lat_boundary[1] & lat <= lat_boundary[2] & 
           lon >= lon_boundary[1] & lon <= lon_boundary[2])

# Create the map plot using the filtered data
us_map_filtered <- ggplot() +
  geom_polygon(data = us_boundaries, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = filtered_data, aes(x = lon, y = lat, color = orientation), alpha = 0.5) +
  labs(title = "Geographic Distribution of Users in the US") +
  ggdark::dark_theme_gray()

print(us_map_filtered)


# Insight 4: Distribution of Users by Orientation with a stacked bar chart for drinks category
# Filter and remove the "desperately" data point (assuming it's an error)
merged_data <- merged_data %>% filter(drinks != "desperately")

# Plotting the Distribution of Users by Orientation and Drinking Habits
orientation_drinks_plot <- merged_data %>%
  group_by(orientation, drinks) %>%
  tally() %>%
  arrange(orientation, -n) %>%
  ggplot(aes(x = orientation, y = n, fill = drinks)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribution of Users by Orientation and Drinking Habits",
       x = "Orientation",
       y = "Count") +
  scale_fill_brewer(palette = "Set3", 
                    breaks = c("socially", "often", "rarely", "very often", "not at all", "NA"),
                    labels = c("Socially", "Often", "Rarely", "Very Often", "Not at All", "Data NA")) +
  theme_minimal() +
  ggdark::dark_theme_gray()

print(orientation_drinks_plot)