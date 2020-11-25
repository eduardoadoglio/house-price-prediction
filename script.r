library(ggplot2)
library(openintro)
library(dplyr)
library(scales)
library(psych)

install.packages("psych")

setwd("C:/Users/eduar/Downloads/EACH EAD/20202/MQA/segundo_trabalho")

# Raw house data
data <- read.csv("kc_house_data.csv", header = TRUE)

# Get 5000 rows sample from original data
sampled_data <- data[sample(nrow(data), 5000),]
# Remove unwanted columns
sampled_data <- sampled_data[, c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 20, 21)]


# Get correlation matrix
cor(sampled_data$price, sampled_data$sqft_living)
# Multiple linear regression

fit <- lm(price ~ sqft_living + waterfront + view + condition + grade + sqft_living15, data=my_data)

fit <- lm(price ~ sqft_living, data=my_data)

fit <- lm(price ~ ., data=sampled_data)

summary(fit)

