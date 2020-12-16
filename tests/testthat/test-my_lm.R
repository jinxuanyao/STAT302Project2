# within test-my_lm.R
test_that("my lm returns a table", {
  expect_is(my_lm(mpg ~ hp + wt, mtcars),"table")
})
