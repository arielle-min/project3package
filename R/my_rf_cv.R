#' Random Forest Cross-Validation Function
#'
#' This function predicts a variable using covariates from the same data set.
#'
#' @param k Numeric input of the number of folds.
#' @keywords prediction
#'
#' @return Numeric output of the cross-validation error.
#'
#' @examples
#' my_rf_cv(k = 5)
#'
#' @import dplyr
#'
#' @export
my_rf_cv <- function(k) {
  penguins <- palmerpenguins::penguins
  train <- penguins %>% dplyr::select(6, 3, 4, 5)

  # removes NA values
  train <- na.omit(train)

  # divides the data into folds
  fold <- sample(rep(1:k, length = nrow(train)))
  data <- data.frame("x" = train, "fold" = fold)

  # creates empty matrix to store values
  mse <- matrix(NA, k)

  # fills empty matrix with the mse for entire fold
  for (i in 1:k) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    model <- randomForest::randomForest(formula = x.body_mass_g ~
                                          x.bill_length_mm + x.bill_depth_mm +
                                          x.flipper_length_mm,
                                          data = data_train, ntree = 100)
    predictions <- predict(model, data_test)
    mse[i] <- (sum((data_test$x.body_mass_g - predictions)^2)) / (nrow(data_test))
  }
  mse_avg <- colMeans(mse)

  return(mse_avg)
}
