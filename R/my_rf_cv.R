#' Random Forest Cross-Validation
#'
#' This function tests how good the random forest model fits
#'   in the penguins dataset.
#'
#' @param k: number of folds
#'
#' @keywords inference
#' @keywords prediction
#'
#' @return a numeric with the cross-validation error
#'
#' @import randomForest
#'
#' @examples
#' my_rf_cv(5)
#'
#' @export
my_rf_cv <- function(k){
  # input data
  train <- (na.omit(my_penguins))[3:6]
  # Split data in k parts, randomly
  inds <- sample(rep(1:k, length = nrow(train)))
  data = data.frame(data = train, "split" = inds)
  MSE = rep(NA, k)
  for (i in 1: k){
    data_train <- data %>% dplyr::filter(split != i)
    data_test <- data %>% dplyr::filter(split == i)
    MODEL <- randomForest(data.body_mass_g ~ data.bill_length_mm +
                            data.bill_depth_mm +
                            data.flipper_length_mm,
                          data = data_train, ntree = 100)
    PREDICTIONS <- predict(MODEL, data_test[, -4])
    MSE[i] <- (sum((data_test[, 4] - PREDICTIONS)^2))/length(PREDICTIONS)
  }
  ave_MSE <- mean(MSE)
  return(ave_MSE)
}
