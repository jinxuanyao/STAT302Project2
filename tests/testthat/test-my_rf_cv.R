# within test-my_rf_cv.R
test_that("my_rf_cv_test returns a number", {
  expect_is(cv5MSE <- my_rf_cv(5), "numeric")
})
