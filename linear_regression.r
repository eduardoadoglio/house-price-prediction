mlr <- function (data){
  fit <- lm(price ~ sqft_living + waterfront + view + condition + grade + sqft_living15, data=data)
  fit <- lm(price ~ ., data=data)
  #fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + grade + sqft_above + sqft_living15 + has_basement, data=data)
  #fit <- lm(price ~ bedrooms + bathrooms + floors + waterfront + view + grade + has_basement, data=data)
  #fit <- lm(price ~ bedrooms + sqft_living + waterfront + grade + yr_built, data=data)
  #step(fit, direction = "backward")
  fit <- lm(price ~ bedrooms + bathrooms + sqft_living + floors + waterfront + view + condition + grade + sqft_above + yr_built + sqft_living15 + sqft_lot15, data=data)
  return(fit)
}

test$valor_calculado <- 6180731.4193 + -32178.0030 * test$bedrooms + 38378.7663 * test$bathrooms + 158.6357 * test$sqft_living + 33029.2224 * test$floors + 603217.5739 * test$waterfront + 34487.5079 * test$view + 25694.3005 * test$condition + 127971.6764 * test$grade + -15.9173 * test$sqft_above + -3597.1888 * test$yr_built + 24.7221 * test$sqft_living15 + -0.7116 * test$sqft_lot15

test$diff <- test$price - test$valor_calculado

mean(abs(test$diff))

mean(test$price) - mean(test$valor_calculado)

ggplot(test, aes(x = valor_calculado, y = price)) +
  geom_point() +
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