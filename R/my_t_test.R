#' my_t_test function
#'
#' This is a function that performs a one sample t-test in R.
#'
#' @param x A numeric vector of data.
#' @param alternative A character string specifying the alternative hypothesis.
#'   This should only accept "two.sided", "less", or "greater".
#' @param mu A number indicating the null hypothesis value of the mean.
#'
#' @keywords inference
#'
#' @return A list with elements,which includes
#'   \code{test_stat}: the numeric test statistic.
#'   \code{df}: the degrees of freedom.
#'   \code{alternative}: the value of the parameter \code{alternative}.
#'   \code{p_val}: the numeric p-value.
#'
#' @examples
#' x <- rnorm(10, mean = 0, sd = 1)
#'   my_t_test(x, alternative = "two.sided" , mu = 5)
#' y <- rnorm(30, mean = 0, sd = 2)
#'   my_t_test(y, alternative = "greater" , mu = 10)
#'
#'
#' @export
my_t_test <- function(x, alternative, mu) {
  #make sure the input alternative is "two.sided", "less", or "greater".
  if (!(alternative =="two.sided" | alternative == "less" | alternative == "greater")){
    stop("The input alternative must be two.sided, less, or greater!" )
  }

  # calculate the test statistic
  test_stat = (mean(x) - mu)/(sd(x)/sqrt(length(x)))

  #calculate the df
  df = length(x) - 1

  #calculate the p_value
  if (alternative == "two.sided"){
    p_val = 2 * pt(abs(test_stat), df = df, lower.tail = FALSE)
  } else if (alternative == "less"){
    p_val = pt(test_stat, df = df, lower.tail = TRUE)
  } else if (alternative == "greater"){
    p_val = pt(test_stat, df = df, lower.tail = FALSE)
  }

  #return a list of value
  val_list = list("test_stat" = test_stat,
                  "df" = df,
                  "alternative" = alternative,
                  "p_val" = p_val)
  return(val_list)
}
