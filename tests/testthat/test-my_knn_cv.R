# within test-my_knn_cv.R

test_that("my_knn_cv_test returns a list", {
  penguins_data <- (na.omit(my_penguins))[3:6]
  cl <- (na.omit(my_penguins))[,1]
  expect_is(my_knn_cv(penguins_data, cl, 1, 5), "list")
})
