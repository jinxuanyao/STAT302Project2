#' k-Nearest Neighbor Classification
#'
#' This functions predicts output class using covariates by k-Nearest Neighbor
#'   classification and evaluate the cross-validation misclassification error.
#'
#' @param train a data frame contains covariates without missing values
#' @param cl true class value without NAs for the training data
#' @param k_nn integer representing number of neighbors
#' @param k_cv number of folds considered
#'
#' @keywords prediction
#'
#' @return a list with 2 objects:
#' \describe{
#'   \item{class}{a vector of the predicted class for all observations}
#'   \item{cv_err}{the cross-validation misclassification error}
#' }
#'
#' @import class magrittr stats
#'
#' @examples
#' penguins_data <- (na.omit(my_penguins))[3:6]
#' cl <- (na.omit(my_penguins))[,1]
#' my_knn_cv(penguins_data, cl, 1, 5)
#' @export
#'
my_knn_cv <- function(train, cl, k_nn, k_cv){
  # Split data in k parts, randomly
  inds <- sample(rep(1:k_cv, length = nrow(train)))
  data = data.frame(data = train, "split" = inds)
  cl = data.frame(data = cl, "split" = inds)
  mis_rate <- rep(NA, k_cv)
  data_withoutsplit <- data[,1:(ncol(data)-1)]
  cl_withoutsplit <- cl[,1]
  for (i in 1: k_cv){
    # predict the class
    data_train <- (data %>% dplyr::filter(split != i))[,1:(ncol(data)-1)]
    cl_train <- (cl %>% dplyr::filter(split != i))[,1]
    data_test <- (data %>% dplyr::filter(split == i))[,1:(ncol(data)-1)]

    data_test_predict <- knn(data_train, data_test, cl_train, k = k_nn)
    data_test_predict <- as.character(data_test_predict)

    # calculate the misclassification rate
    cl_test <- (cl %>% dplyr::filter(split == i))[,1]
    n = 0
    for (k in 1:length(cl_test)){
      if (cl_test[k] != data_test_predict[k]){
        n = n + 1
      }
    }
    mis_rate[i] = n/length(cl_test)
  }
  cv_error = mean(mis_rate)
  class <- knn(data_withoutsplit, data_withoutsplit, cl_withoutsplit, k_nn)
  class <- as.character(class)
  x <- list("class" = class, "cv_error" = cv_error)
  return(x)
}
