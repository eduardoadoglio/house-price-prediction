library(ggplot2)
library(openintro)
library(dplyr)
library(scales)
library(psych)

# Raw house data
data <- read.csv(file.choose(), header = TRUE)

# Get 5000 rows sample from original data
sampled_data <- data[sample(nrow(data), 5000),]

#Generating a new column with data that indicates how much time has passed since the 
#last renovation, i.e. cur_year - yr_renovated (if the house has never been renovated,
#yr_built replaces yr_renovated in the formula).
sampled_data$lst_renovation <- 2020 - sampled_data$yr_renovated
sampled_data$lst_renovation <- ifelse(sampled_data$lst_renovation == 2020, 2020 - sampled_data$yr_built, sampled_data$lst_renovation)

#Generating a new column with data to replace the basement area with a variable to
#to indicate a basement existence 
sampled_data$has_basement <- ifelse(sampled_data$sqft_basement >= 1, 1, 0)

# Remove unwanted columns
sampled_data <- sampled_data[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 20, 21, 22,23)]

# Get correlation matrix
cor(sampled_data$price, sampled_data$sqft_living)

# Multiple linear regression
fit <- lm(price ~ sqft_living + waterfront + view + condition + grade + sqft_living15, data=sampled_data)

fit <- lm(price ~ sqft_living, data=sampled_data)

fit <- lm(price ~ ., data=sampled_data)

# Summary of multiple linear regression results
summary(fit)

