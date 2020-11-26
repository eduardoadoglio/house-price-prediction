mlr <- function (data){
  fit <- lm(price ~ sqft_living + waterfront + view + condition + grade + sqft_living15, data=data)
  fit <- lm(price ~ sqft_living, data=data)
  fit <- lm(price ~ ., data=data)
  return(fit)
}

