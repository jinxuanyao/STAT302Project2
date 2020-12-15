# within test-my_t_test.R
test_that("my_t_test only includes specified input for alternative", {
  expect_error(my_t_test(x = 1:100, alternative = "both", mu = 1))
})

test_that("my t.test returns a list", {
  expect_is(my_t_test(x = 1:100, alternative = "two.sided", mu = 1),"list")
})

test_that("my t.test returns a list", {
  expect_is(my_t_test(x = 1:100, alternative = "less", mu = 1),"list")
})

test_that("my t.test returns a list", {
  expect_is(my_t_test(x = 1:100, alternative = "greater", mu = 1),"list")
})
