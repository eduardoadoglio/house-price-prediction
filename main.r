library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(psych)

source("exploratory_analysis.r")
source("linear_regression.r")
source("utils.r")

#install.packages("reshape2")

# Remove scientific notation
options(scipen=999)

# Sets working directory to current directory, if you're using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# Raw house data
data <- read.csv(file.choose(), header = TRUE)
# Get 5000 rows sample from original data
reduced_data <- data[sample(nrow(data), 5000),]
write.csv(reduced_data, "C:/Users/eduar/Downloads/Projetos/house-price-prediction/data.csv", row.names = FALSE)
# Defining sample size
sample_size <- floor(0.70 * nrow(reduced_data))
# Set Seed so that same sample can be reproduced in future also
# Always set the seed before sampling
set.seed(101)
# Now Selecting 70% of data as sample from 5000 rows of the data
train_ind <- sample(seq_len(nrow(data)), size = sample_size)
train <- data[train_ind, ]
test <- data[-train_ind, ]
#Generating a new column with data that indicates how much time has passed since the 
#last renovation, i.e. cur_year - yr_renovated (if the house has never been renovated,
#yr_built replaces yr_renovated in the formula).
data$lst_renovation <- 2020 - data$yr_renovated
data$lst_renovation <- ifelse(data$lst_renovation == 2020, 2020 - data$yr_built, data$lst_renovation)

#Generating a new column with data to replace the basement area with a variable
#that indicates a basement existence 
data$has_basement <- ifelse(data$sqft_basement >= 1, 1, 0)

# Remove unwanted columns
data <- data[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 20, 21, 22,23)]

fit <- mlr(data)

# Summary of multiple linear regression results
summary(fit)

# Function calls
generate_lower_triangle_correlation_matrix_heatmap(data)

# Scatter plots
generate_price_by_sqft_living_scatter_plot(data)
generate_price_by_grade_scatter_plot(data)
generate_beeswarm_plus_boxplot_for_price_and_grade(data)
generate_price_by_bedroom_scatter_plot(data)
generate_price_by_bathroom_scatter_plot(data)

#Correlation
generate_correlation_matrix_heatmap(data)
generate_lower_triangle_correlation_matrix_heatmap(data)
