# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggdark)
library(sf)
library(stringr)

# Set the working directory
setwd("/Users/chararam/Documents/GitHub/data-mining-for-business-CSCI-E-96/cases/I_Ok_Cupid")

# Load data
profiles <- read.csv('profiles.csv')
latlon <- read.csv('LatLon.csv')
addr <- read.csv('addr.csv')
sharedCensus <- read.csv('sharedCensus2010Vars.csv')

# Merge datasets based on 'location'
master_data <- merge(profiles, latlon, by = 'location')
master_data <- merge(master_data, addr, by = 'location')
master_data <- merge(master_data, sharedCensus, by = 'location')

# Insight 1: Distribution of Users by Age and Orientation (Zoomed in on age 20-40)
plot1 <- master_data %>%
  filter(age >= 20 & age <= 40) %>%
  ggplot(aes(x = age, fill = orientation)) +
  geom_histogram(position = "stack", bins = 20) +
  ggdark::dark_theme_gray() +
  labs(title = "Distribution of Users by Age and Orientation (Age 20-40)",
       x = "Age", y = "Count")

# Insight 2a: Relationship between Relationship Status and Income
plot2a <- master_data %>%
  group_by(status) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(status, -avg_income), y = avg_income)) +
  geom_bar(stat = 'identity') +
  ggdark::dark_theme_gray() +
  labs(title = "Average Income by Relationship Status",
       x = "Relationship Status", y = "Average Income")

# Insight 2b: Relationship between Education and Income
plot2b <- master_data %>%
  group_by(education) %>%
  summarise(avg_income = mean(income, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(education, -avg_income), y = avg_income)) +
  geom_bar(stat = 'identity') +
  ggdark::dark_theme_gray() +
  labs(title = "Average Income by Education",
       x = "Education Level", y = "Average Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Insight 3: Geographic Distribution of Users (Focusing on the US)
plot3 <- ggplot(master_data, aes(x = lon, y = lat, color = orientation)) +
  geom_point(alpha = 0.5) +
  ggdark::dark_theme_gray() +
  coord_sf(xlim = c(-130, -65), ylim = c(24, 50)) + # Focusing on US mainland
  labs(title = "Geographic Distribution of Users in the US")

# Insight 4: Distribution of Users by Orientation
plot4 <- master_data %>%
  ggplot(aes(x = orientation)) +
  geom_bar(aes(fill = orientation)) +
  ggdark::dark_theme_gray() +
  labs(title = "Distribution of Users by Orientation",
       x = "Orientation", y = "Count")


# Display plots
print(plot1)
print(plot2a)
print(plot2b)
print(plot3)
print(plot4)
