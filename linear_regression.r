mlr <- function (data){
  #fit <- lm(price ~ sqft_living + waterfront + view + condition + grade + sqft_living15, data=data)
  fit <- lm(price ~ ., data=data)
  #fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + grade + sqft_above + sqft_living15 + has_basement, data=data)
  #fit <- lm(price ~ bedrooms + bathrooms + floors + waterfront + view + grade + has_basement, data=data)
  #fit <- lm(price ~ bedrooms + sqft_living + waterfront + grade + yr_built, data=data)
  step(fit, direction = "backward")
  #fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + condition + grade + sqft_above + yr_built + sqft_living15 + sqft_lot15, data=data)
  fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + 
              view + condition + grade + yr_built + sqft_living15 + sqft_lot15, data=data)
  
  return(fit)
}

test$diff <- test$price - test$valor_calculado

x <- predict.lm(fit, test)

x

test$valor_calculado <- x

test$valor_diff <- test$price - x

mean(abs(test$diff))

mean(test$price) - mean(test$valor_calculado)

sd(test$valor_diff)

test$error_percent <- (test$valor_calculado / test$price) * 100

ggplot(test, aes(x = valor_calculado, y = price)) +
  geom_point() +
  theme_minimal() +
  stat_smooth(method = "lm",
              col = "#C42126",
              se = FALSE,
              size = 1) +
  labs(
    x = "Tamanho do imóvel em pés",
    y = "Preço",
    color = "Nota"
  )

plot(x, y, xlab = "Predictor variable", ylab = "Predicted variable"); points(test$price, test$valor_calculado, col = "blue")
