mlr <- function (data){
  #fit <- lm(price ~ sqft_living + waterfront + view + condition + grade + sqft_living15, data=data)
  fit <- lm(price ~ ., data=data)
  #fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + grade + sqft_above + sqft_living15 + has_basement, data=data)
  #fit <- lm(price ~ bedrooms + bathrooms + floors + waterfront + view + grade + has_basement, data=data)
  #fit <- lm(price ~ bedrooms + sqft_living + waterfront + grade + yr_built, data=data)
  #step(fit, direction = "backward")
  #fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + condition + grade + sqft_above + yr_built + sqft_living15 + sqft_lot15, data=data)
  return(fit)
}

fit$coefficients

prediction <- predict.lm(fit, test)
test$valor_calculado <- prediction
test$diff <- test$price - test$valor_calculado
test

mean(test$price)

mean(abs(test$diff))

max(abs(test$diff))

min(abs(test$diff))

#test$diff <- test$price - test$valor_calculado

#mean(abs(test$diff))

#mean(test$price) - mean(test$valor_calculado)

# ggplot(test, aes(x = valor_calculado, y = price)) +
#   geom_point() +
#   stat_smooth(method = "lm",
#               col = "#C42126",
#               se = TRUE,
#               size = 1) +
#   theme_minimal() +
#   labs(
#     x = "Tamanho do imóvel em pés",
#     y = "Preço",
#     color = "Nota"
#   )