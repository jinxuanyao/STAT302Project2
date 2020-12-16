#' my_lm function
#'
#' This is a function that fits a linear model in R.
#'
#' @param formula: a formula class object
#' @param data: input data frame.
#'
#' @keywords prediction
#'
#' @return a table with rows for each coefficient
#'   (including the (Intercept)!) and columns for
#'   \code{the Estimate}, \code{Std. Error},
#'   \code{t value}, and \code{Pr(>|t|)}.
#'
#' @examples data(mtcars)
#'  my_lm(mpg ~ hp + wt,mtcars)
#'
#' @export
my_lm <- function(formula, data) {
  model_frame <- model.frame(formula, data)
  bold_X <- model.matrix(formula, model_frame)
  bold_Y <- model.response(model_frame)
  # Find Estimate in matrix form with row name automatically
  estimate <- solve(t(bold_X) %*% bold_X) %*% t(bold_X) %*% bold_Y
  result <- estimate
  df <- nrow(data) - nrow(result)
  sigma_square <- sum((bold_Y - bold_X %*% result[, 1])^2 / df)
  # find the inverse matrix of X-transpose and X
  step_matrix <- solve(t(bold_X) %*% bold_X)
  # Find Std. Error for each coefficient
  std_error <- sqrt(sigma_square * diag(step_matrix))
  result <- cbind(result, std_error)
  # Find t value
  t_val <- (estimate - 0) / std_error
  result <- cbind(result, t_val)
  # Find Pr(>|t|)
  p_area <- 2 * pt(abs(t_val), df, lower.tail = FALSE)
  result <- cbind(result, p_area)
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  # convert result to a table
  result <- as.table(result)
  return(result)
}
