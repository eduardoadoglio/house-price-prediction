library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(psych)

source("exploratory_analysis.r")
#source("utils.r")

#install.packages("reshape2")

# Remove scientific notation
options(scipen=999)

# Sets working directory to current directory, if you're using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Raw house data
data <- read.csv(file.choose(), header = TRUE)

# Get 5000 rows sample from original data
sampled_data <- data[sample(nrow(data), 5000),]

#Generating a new column with data that indicates how much time has passed since the 
#last renovation, i.e. cur_year - yr_renovated (if the house has never been renovated,
#yr_built replaces yr_renovated in the formula).
sampled_data$lst_renovation <- 2020 - sampled_data$yr_renovated
sampled_data$lst_renovation <- ifelse(sampled_data$lst_renovation == 2020, 2020 - sampled_data$yr_built, sampled_data$lst_renovation)

#Generating a new column with data to replace the basement area with a variable
#that indicates a basement existence 
sampled_data$has_basement <- ifelse(sampled_data$sqft_basement >= 1, 1, 0)

# Remove unwanted columns
sampled_data <- sampled_data[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 20, 21, 22,23)]


fit <- mlr(sampled_data)

# Summary of multiple linear regression results
summary(fit)

# Function calls
generate_lower_triangle_correlation_matrix_heatmap(sampled_data)

# Scatter plots
generate_price_by_sqft_living_scatter_plot(sampled_data)
generate_price_by_grade_scatter_plot(sampled_data)
generate_beeswarm_plus_boxplot_for_price_and_grade(sampled_data)
generate_price_by_bedroom_scatter_plot(sampled_data)
generate_price_by_bathroom_scatter_plot(sampled_data)

#Correlation
generate_correlation_matrix_heatmap(sampled_data)
generate_lower_triangle_correlation_matrix_heatmap(sampled_data)
