#' k-Nearest Neighbors Cross-Validation Function
#'
#' This function ____.
#'
#' @param train Data frame input.
#' @param cl Vector input containing the true class of the training data.
#' @param k_nn Numeric input of the k-nearest neighbors.
#' @param k_cv Numeric input of number of folds.
#'
#' @return List output containing predicted class and cross validation error.
#'
#' @examples
#' library(palmerpenguins)
#' library(dplyr)
#' data(package = "palmerpenguins")
#' penguins_omit <- na.omit(penguins)
#' my_cl <- penguins_omit %>% pull(species)
#' my_knn_cv(train = penguins_omit["bill_length_mm"], cl = my_cl,
#'   k_nn = 1, k_cv = 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  # divides the data into folds
  fold <- sample(rep(1:k_cv, length = NROW(train)))

  data <- data.frame("x" = train, "y" = cl, "fold" = fold)

  # creates empty matrices to store values
  class <- matrix(NA, NROW(train))
  cv_error <- matrix(NA, k_cv)

  j <- 0

  # fills an empty matrix with predicted class, repeats for each fold
  for (i in 1:k_cv) {
    data_train <- data %>% filter(fold != i)
    data_test <- data %>% filter(fold == i)
    cl_knn <- data_train$y
    data_train <- data_train %>% select(starts_with('x'))
    data_test <- data_test %>% select(starts_with('x'))
    knn <- class::knn(data_train, data_test, cl_knn, k = k_nn)
    knn <- as.matrix(knn)

    # fills the matrix with the predicted class for the entire fold
    for (k in 1:length(knn)) {
      class[j + k] <- knn[k]
    }

    cv_error[i] <- mean(class[j:(j + length(knn))] != cl[j:(j + length(knn))])
    j <- j + length(knn)

  }
  cv_err <- colMeans(cv_error)
  result <- list ("class" = class,
                  "cv error" = cv_err)
  return(result)
}
