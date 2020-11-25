library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(psych)

install.packages("reshape2")

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

# Multiple linear regression
fit <- lm(price ~ sqft_living + waterfront + view + condition + grade + sqft_living15, data=sampled_data)

fit <- lm(price ~ sqft_living, data=sampled_data)

fit <- lm(price ~ ., data=sampled_data)

# Summary of multiple linear regression results
summary(fit)

# Helper functions

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cor_mat){
  cor_mat[upper.tri(cor_mat)] <- NA
  return(cor_mat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cor_mat){
  cor_mat[lower.tri(cor_mat)]<- NA
  return(cor_mat)
}

reorder_cormat <- function(cor_mat){
  # Use correlation between variables as distance
  dd <- as.dist((1 - cor_mat) / 2)
  hc <- hclust(dd)
  cor_mat <-cor_mat[hc$order, hc$order]
}

# Begin exploratory analysis

# Scatter plots

generate_price_by_sqft_living_scatter_plot <- function(data){
  ggplot(data, aes(x = sqft_living, y = price)) +
    geom_point(aes(color= factor(grade))) +
    stat_smooth(method = "lm",
                col = "#C42126",
                se = TRUE,
                size = 1) +
    theme_minimal() +
    labs(
      x = "Tamanho do im�vel em p�s",
      y = "Pre�o",
      color = "Nota"
    )
}

generate_price_by_grade_scatter_plot <- function(data){
  ggplot(data, aes(x = grade, y = price)) +
    geom_point(aes(color= factor(grade))) +
    stat_smooth(method = "lm",
                col = "#C42126",
                se = FALSE,
                size = 1) +
    theme_minimal() +
    labs(
      x = "Nota do im�vel",
      y = "Pre�o",
      color = "Nota"
    )
}

generate_beeswarm_plus_boxplot_for_price_and_grade <- function(data) {
  ggplot(data, aes(x = factor(grade), y = price, color = grade, group = 1)) +
    geom_boxplot()+
    geom_point(size = 2, position = position_jitter(width = 0.2)) +
    stat_summary(fun.y = mean, geom = "point", shape = 20, size = 6, color = "blue")+
    theme_classic() +
    facet_grid(.~grade) +
    labs(
      x = "Notas",
      y = "Pre�o",
      color = "Nota"
    )
}

generate_price_by_bedroom_scatter_plot <- function(data){
  ggplot(data, aes(x = bedrooms, y = price)) +
    geom_point(aes(color= factor(grade))) +
    stat_smooth(method = "lm",
                col = "#C42126",
                se = FALSE,
                size = 1) +
    theme_minimal() +
    labs(
      x = "N�mero de quartos",
      y = "Pre�o",
      color = "Nota"
    )
}

generate_price_by_bathroom_scatter_plot <- function(data){
  ggplot(data, aes(x = bathrooms, y = price)) +
    geom_point(aes(color= factor(grade))) +
    stat_smooth(method = "lm",
                col = "#C42126",
                se = FALSE,
                size = 1) +
    theme_minimal() +
    labs(
      x = "N�mero de banheiros",
      y = "Pre�o",
      color = "Nota"
    )
}

# Correlation

# Get basic correlation matrix
cor(sampled_data, method=c("pearson"))

generate_correlation_matrix_heatmap <- function (data) {
  cor_matrix <- cor(sampled_data, method=c("pearson"))
  melted_data <- melt(cor_matrix)
  ggplot(data = melted_data, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile()
}

generate_lower_triangle_correlation_matrix_heatmap <- function (data) {
  cor_matrix <- cor(sampled_data, method=c("pearson"))
  cor_matrix <- reorder_cormat(cor_matrix)
  lower_tri <- get_lower_tri(cor_matrix)
  melted_data <- melt(lower_tri, na.rm = TRUE)
  gg_heatmap <- ggplot(data = melted_data, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correla��o de\nPearson") +
    theme_minimal()+ 
    coord_fixed()+ 
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, size = 6, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal") +
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
  
  gg_heatmap
}
generate_lower_triangle_correlation_matrix_heatmap(sampled_data)

# Function calls

# Scatter plots

generate_price_by_sqft_living_scatter_plot(sampled_data)

generate_price_by_grade_scatter_plot(sampled_data)

generate_beeswarm_plus_boxplot_for_price_and_grade(sampled_data)

generate_price_by_bedroom_scatter_plot(sampled_data)

generate_price_by_bathroom_scatter_plot(sampled_data)

# Correlation

generate_correlation_matrix_heatmap(sampled_data)

generate_lower_triangle_correlation_matrix_heatmap(sampled_data)
