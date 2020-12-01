library(ggplot2)
library(reshape2)
library(dplyr)
library(scales)
library(psych)

#install.packages("reshape2")

# Remove scientific notation
options(scipen=999)

# Sets working directory to current directory, if you're using RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# House data
data <- read.csv(file.choose(), header = TRUE)
# Get 5000 rows sample from original data
reduced_data <- data[sample(nrow(data), 5000),]
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

#Remove unwanted columns
data <- data[c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 20, 21, 22, 23)]
#Creating a dataset copy to plot correlation matrix without yr_build.
dataCor <- data[c(1, 2, 3, 4, 5, 6, 8 ,9 ,10, 11, 12, 13, 14, 15, 16)]
#Calculating houses' age
dataCor$age <- 2020 - dataCor$yr_built
#Deleting useless columns
dataCor <- dataCor[c(1, 2, 3, 4, 5, 6, 8 ,9 ,10, 12, 13, 14, 15, 16)]

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

lm_eqn <- function(df, dependent, independent){
  m <- lm(dependent ~ independent, df);
  eq <- substitute(y == a + b %.% x*","~~r^2~"="~r2, 
                   list(a = format(unname(coef(m)[1]), digits = 2),
                        b = format(unname(coef(m)[2]), digits = 2),
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));
}

# Begin exploratory analysis functions
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
      x = "Tamanho do imóvel em pés",
      y = "Preço",
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
      x = "Nota do imóvel",
      y = "Preço",
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
      y = "Preço",
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
      x = "Número de quartos",
      y = "Preço",
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
      x = "Número de banheiros",
      y = "Preço",
      color = "Nota"
    )
}

generate_price_by_waterfront_scatter_plot <- function(data){
  ggplot(data, aes(x = waterfront, y = price)) +
    geom_point(aes(color= factor(grade))) +
    stat_smooth(method = "lm",
                col = "#C42126",
                se = TRUE,
                size = 1) +
    theme_minimal() +
    labs(
      x = "Tamanho do imóvel em pés",
      y = "Preço",
      color = "Nota"
    )
}

generate_lower_triangle_correlation_matrix_heatmap <- function (data) {
  cor_matrix <- cor(data, method=c("pearson"))
  cor_matrix <- reorder_cormat(cor_matrix)
  lower_tri <- get_lower_tri(cor_matrix)
  melted_data <- melt(lower_tri, na.rm = TRUE)
  gg_heatmap <- ggplot(data = melted_data, aes(x = Var1, y = Var2, fill = value)) + 
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Correlação de\nPearson") +
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

grade_histogram <- function(data){
  ggplot(data, aes(x = grade)) +
    geom_histogram(binwidth = 0.5, colour = "steelblue", fill = "steelblue") +
    theme_minimal() +
    labs(
      x = "Nota",
      y = "Ocorrências"
    )
  
}

sqft_histogram <- function(data){
  ggplot(data, aes(x = sqft_living)) +
    geom_histogram(binwidth = 0.5, colour = "steelblue", fill = "steelblue") +
    theme_minimal() +
    labs(
      x = "Tamanho do imóvel em pés",
      y = "Ocorrências"
    )
  
}

waterfront_boxplot <- function(data) {
  ggplot(data, aes(x=waterfront, y=price, group=waterfront)) + 
    geom_boxplot(fill = "steelblue") +
    theme_minimal () +
    labs(
      x = "Beira-mar",
      y = "Preço"
    )
}

grade_boxplot <- function(data) {
  ggplot(data, aes(x=grade, y=price, group=grade)) + 
    geom_boxplot(fill = "steelblue") +
    theme_minimal () +
    labs(
      x = "Nota",
      y = "Preço"
    )
}


# Exploratory analysis function calls

# Scatter plots
generate_price_by_sqft_living_scatter_plot(data)
generate_price_by_grade_scatter_plot(data)
generate_beeswarm_plus_boxplot_for_price_and_grade(data)
generate_price_by_bedroom_scatter_plot(data)
generate_price_by_bathroom_scatter_plot(data)
generate_price_by_waterfront_scatter_plot(data)

# Histograms
grade_histogram(data)
sqft_histogram(data)

# Boxplots
waterfront_boxplot(data)
grade_boxplot(data)

#Correlation
generate_lower_triangle_correlation_matrix_heatmap(dataCor)

# Begin multiple linear regression

#Creating a multivariate linear model
fit <- lm(price ~ ., data=train)

#Summary of multivariate linear regression results
summary(fit)

#Creating refined multivariate linear model
fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + condition + grade + yr_built + sqft_living15 + lst_renovation, data=train)

#Summary of multivariate linear regression results
summary(fit)

#Plot linear regression result graph
plot(fit)

#Predicting house prices with the test dataset
prediction <- predict.lm(fit, test)

#Calculating the prediction error (real price - prediction)
predError <- test$price - prediction

#Calculating mean prediction error
mean(abs(predError))

#Getting highest prediction error
max(abs(predError))

#Getting smallest prediction error
min(abs(predError))
