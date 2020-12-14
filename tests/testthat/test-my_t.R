# within test-my_t.test.R
test_that("my_t.test only includes specified input for alternative", {
  expect_error(my_t.test(x = 1:100, alternative = "both", mu = 1))
})

test_that("my t.test returns a list", {
  expect_is(my_t.test(x = 1:100, alternative = "two.sided", mu = 1),"list")
})
