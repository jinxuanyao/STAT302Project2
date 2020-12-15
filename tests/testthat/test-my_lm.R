# within test-my_lm.R
test_that("my lm returns a data frame", {
  expect_is(my_lm(mpg ~ hp + wt, mtcars),"data.frame")
})
