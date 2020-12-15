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
my_lm <- function(formula, data){
  # calculate the model matrix X
  X = model.matrix(formula, data)
  # calculate the model response Y
  Y = model.response(model.frame(formula, data))
  # calculate the b
  b = solve((t(X) %*% X)) %*% t(X) %*% Y
  # calculate the standard error
  df = nrow(mtcars) - nrow(b)
  var_b = (Y - X %*% b) ^ 2 / df
  sigma_2 = sum(var_b)
  se_b = sqrt(diag(sigma_2 * (solve(t(X) %*% X))))
  # calculate the t value
  t_val = (b - 0) / se_b
  # calculate the p value
  p_val = 2 * pt(abs(t_val), df, lower.tail = FALSE)
  # create the table
  # create a matrix
  output_matrix = matrix(c(b, se_b, t_val, p_val),
                         nrow = nrow(b),
                         ncol = 4,
                         byrow = FALSE)
  # name the matrix
  rownames(output_matrix) = c(rownames(b))
  colnames(output_matrix) = c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  #make a table
  my_tab <- data.frame(output_matrix)
  return(my_tab)
}
