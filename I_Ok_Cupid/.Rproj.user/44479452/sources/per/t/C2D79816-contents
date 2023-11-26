# Setting the working directory
setwd("/../I_Ok_Cupid")

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(sf)
library(stringr)
library(forcats)
library(maps)
# library(ggdark)

# Reading the data
profiles <- read.csv('profiles.csv')
latlon <- read.csv('LatLon.csv')
addr <- read.csv('addr.csv')
sharedCensus <- read.csv('sharedCensus2010Vars.csv')

# Merge dataframes based on common column 'location'
merged_data <- merge(profiles, latlon, by = "location", all.x = TRUE)
merged_data <- merge(merged_data, addr, by = "location", all.x = TRUE)
merged_data <- merge(merged_data, sharedCensus, by = "location", all.x = TRUE)

# Insight 1 Light: Distribution of users by age and orientation (Age 20-40)
# Remove age outliers (109 and 110)
merged_data <- merged_data %>% filter(!(age %in% c(109, 110)))

age_orientation_plot_adjusted <- merged_data %>%
  filter(age >= 20 & age <= 40) %>%
  ggplot(aes(x = age, fill = orientation)) +
  geom_histogram(position = "stack", binwidth = 1) +
  labs(title = "Adjusted Distribution of Users by Age and Orientation (Age 20-40)", x = "Age", y = "Count")

print(age_orientation_plot_adjusted)

# Counting number of users for each age
age_counts <- profiles %>%
  group_by(age) %>%
  summarise(count = n())

# Print the results
print(age_counts, n=54)

# Insight 2a: Average Income by Relationship Status with custom colors
# Define a custom color palette to ensure consistency
custom_palette <- c("available" = "#F8766D", "single" = "#00BA38", "seeing someone" = "#619CFF", 
                    "married" = "#F564E3", "unknown" = "#959595")

# Categorizing the education levels
merged_data$education <- case_when(
  str_detect(merged_data$education, "high school") ~ "High School",
  str_detect(merged_data$education, "college/university") ~ "College/University",
  str_detect(merged_data$education, "two-year college") ~ "Two-year College",
  str_detect(merged_data$education, "masters program") ~ "Masters and above",
  TRUE ~ "Other"
)

income_status_plot <- merged_data %>%
  group_by(status) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(status, -avg_income), y = avg_income, fill = status)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Income by Relationship Status", x = "Relationship Status", y = "Average Income") +
  scale_fill_manual(values = custom_palette) +
  theme_minimal()

print(income_status_plot)

# Insight 2b: Education vs. Income overlapping by Relationship Status with custom colors and reordered legend
# Setting factor levels for education in desired order
merged_data$education <- factor(merged_data$education, levels = c("High School", "Two-year College", "College/University", "Masters and above", "Other"))

income_education_status_overlapping_plot <- merged_data %>%
  group_by(education, status) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  arrange(desc(avg_income)) %>%  # Sorting by descending avg_income for plotting order
  ggplot(aes(x = education, y = avg_income, fill = status)) + 
  geom_bar(stat = "identity", position = "identity") +  # Using position = "identity" for overlapping
  labs(title = "Average Income by Education Level and Relationship Status", 
       x = "Education Level", 
       y = "Average Income") +
  scale_fill_manual(values = custom_palette, breaks = c("available", "single", "seeing someone", "married", "unknown")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  theme_minimal()

print(income_education_status_overlapping_plot)

#descepencey between 2a and 2b average income graphs
summary(merged_data$income)

count_data <- merged_data %>%
  group_by(education, status) %>%
  summarise(avg_income = mean(income, na.rm = TRUE), count = n())
print(count_data)

# For Insight 2a:
avg_income_status <- merged_data %>%
  group_by(status) %>%
  summarise(avg_income = mean(income, na.rm = TRUE))
print(avg_income_status)

# For Insight 2b:
avg_income_education_status <- merged_data %>%
  group_by(education, status) %>%
  summarise(avg_income = mean(income, na.rm = TRUE))
print(avg_income_education_status)

# data is correct, switching to using side-by-side bars
# Insight 2b: Using position = "dodge" for side-by-side bars
income_education_status_side_by_side_plot <- merged_data %>%
  group_by(education, status) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  ggplot(aes(x = education, y = avg_income, fill = status)) + 
  geom_bar(stat = "identity", position = "dodge", width=0.7) +  # Using position = "dodge" for side-by-side bars
  labs(title = "Average Income by Education Level and Relationship Status (Side by Side Bars)", 
       x = "Education Level", 
       y = "Average Income") +
  scale_fill_manual(values = custom_palette) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  theme_minimal()

print(income_education_status_side_by_side_plot)

# Insight 3: Geographic Distribution of Users in the US
# Define latitude and longitude boundaries for continental US
lat_boundary <- c(25, 50)  # Lower and upper latitude boundaries
lon_boundary <- c(-125, -67)  # Left and right longitude boundaries

# Filter the data
filtered_data <- merged_data %>%
  filter(lat >= lat_boundary[1] & lat <= lat_boundary[2] & 
           lon >= lon_boundary[1] & lon <= lon_boundary[2])

us_boundaries <- map_data("usa")
us_map_filtered <- ggplot() +
  geom_polygon(data = us_boundaries, aes(x = long, y = lat, group = group), fill = "white", color = "black") +
  geom_point(data = filtered_data, aes(x = lon, y = lat, color = orientation), alpha = 0.5) +
  labs(title = "Geographic Distribution of Users in the US") +
  theme_minimal()

print(us_map_filtered)

# Insight 4: Distribution of Users by Orientation with a stacked bar chart for drinks category
# Set the desired order for the drinks factor
merged_data$drinks <- factor(merged_data$drinks, 
                             levels = c("not at all", "rarely", "socially", "often", "very often"))

# Filter out rows with NA values in 'orientation' and 'drinks' columns
filtered_merged_data <- merged_data %>% filter(!is.na(orientation) & !is.na(drinks))

# Create the orientation_drinks_plot
orientation_drinks_plot <- filtered_merged_data %>%
  group_by(orientation, drinks) %>%
  tally() %>%
  filter(!is.na(n)) %>%  # filter out rows with NA values in 'n'
  arrange(orientation, -n) %>%
  ggplot(aes(x = orientation, y = n, fill = drinks)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Distribution of Users by Orientation and Drinking Habits", x = "Orientation", y = "Count") +
  scale_fill_brewer(palette = "Set3", 
                    breaks = c("not at all", "rarely", "socially", "often", "very often"),
                    labels = c("Not at All", "Rarely", "Socially", "Often", "Very Often")) +
  theme_minimal()

print(orientation_drinks_plot)
