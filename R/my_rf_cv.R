#' Random Forest Cross-Validation Function
#'
#' This function ____.
#'
#' @param k Numeric input of the number of folds.
#'
#' @return Numeric output of the cross-validation error.
#'
#' @examples
#' library(palmerpenguins)
#' data(package = "palmerpenguins")
#' my_rf_cv(k = 5)
#'
#' @export
my_rf_cv <- function(k) {
  train <- penguins %>% select(6, 3, 4, 5)

  # removes NA values
  train <- na.omit(penguins)

  # divides the data into folds
  fold <- sample(rep(1:k, length = NROW(train)))
  data <- data.frame("x" = train, "fold" = fold)

  # creates empty matrix to store values
  mse <- matrix(NA, k)

  # fills empty matrix with the mse for entire fold
  for (i in 1:k) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    model <- randomForest(formula = x.body_mass_g ~
                            x.bill_length_mm + x.bill_depth_mm + x.flipper_length_mm,
                          data = data_train, ntree = 100)
    predictions <- predict(model, data_test[, -1])
    mse[i] <- (sum((data_test$x.body_mass_g - predictions)^2)) / (nrow(data_test))
  }
  mse_avg <- colMeans(mse)

  return(mse_avg)
}
