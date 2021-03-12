#' Linear Model Function
#'
#' This function ____.
#'
#' @param formula Formula input of an output class and predictor variables
#'   from \code{data}.
#' @param data Data frame input.
#'
#' @return Matrix displaying the coefficient, standard error, observed
#'   t value, and p_value for the linear model.
#'
#' @examples
#' data(mtcars)
#' my_lm(formula = mpg ~ hp + wt, data = mtcars)
#'
#' @importFrom stats pt na.omit model.matrix model.frame model.response predict sd
#'
#' @export
my_lm <- function(formula, data) {
  x_input <- model.matrix(formula, data)
  model_frame <- model.frame(formula, data)
  y_output <- model.response(model_frame)
  df <- length(y_output) - ncol(x_input)
  inverse <- solve(t(x_input) %*% x_input)
  coef <- inverse %*% t(x_input) %*% y_output
  est <- sum(((y_output - (x_input %*% coef))^2)/df)
  se <- diag(sqrt(est * abs(inverse)))
  t_obs <- (coef - 0) / se
  p_value <- 2 * pt(abs(t_obs), df, lower.tail = FALSE)

  # creates a table for the summary values
  result <- matrix(c(coef, se, t_obs, p_value), nrow = ncol(x_input),
                   ncol = 4, byrow = FALSE)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  rownames(result) <- rownames(p_value)

  return(result)
}
