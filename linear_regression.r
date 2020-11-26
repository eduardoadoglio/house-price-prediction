mlr <- function (sampled_data){
  fit <- lm(price ~ sqft_living + waterfront + view + condition + grade + sqft_living15, data=sampled_data)
  fit <- lm(price ~ sqft_living, data=sampled_data)
  fit <- lm(price ~ ., data=sampled_data)
  return(fit)
}