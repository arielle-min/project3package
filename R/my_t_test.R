#' T Test Function
#'
#' This function preforms a one sample t-test.
#'
#' @param x Numeric input of a vector containing a set of data.
#' @param alternative String input indicating the type of hypothesis test.
#' @param mu Numeric input representing the value for the null hypothesis of
#'   the mean, defaults to \code{0}.
#' @keywords inference
#'
#' @return List output containing the t-statistic, df, \code{alternative},
#'   and p-value.
#'
#' @examples
#' my_t_test(c(10, 15, 14, 16), alternative = "two.sided", mu = 13)
#'
#' @export
my_t_test <- function (x, alternative, mu) {
  est <- mean(x)
  df <- length(x) - 1
  se <- sd(x) / sqrt(length(x))
  t_obs <- (est - mu) / se

  # checks if alternative is either "two.sided", "less", or "greater"
  if (alternative != "two.sided" &&
      alternative != "less" &&
      alternative != "greater") {
    stop("alternative must be two.sided, less, or greater")
  } else {

    # checks the alternative to determine which method of finding the
    # p-value to use
    if (alternative == "less") {
      p_value <- pt(t_obs, df)
    } else if (alternative == "greater") {
      p_value <- pt(t_obs, df, lower.tail = FALSE)
    } else { # two sided t.test
      p_value <- 2 * pt(abs(t_obs), df, lower.tail = FALSE)
    }

    # creates a table for the summary values
    result <- list ("test_stat" = t_obs,
                    "df" = df,
                    "alternative" = alternative,
                    "p_val" = p_value)
    return(result)
  }

}
