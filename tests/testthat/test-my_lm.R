data(mtcars)

test_that("my_lm works mathematically", {
  expect_is(my_lm(mpg ~ hp + wt, mtcars), "matrix")
})

test_that("non formula input throws error", {
  expect_error(my_lm(10, "data frame"))
})

test_that("non data frame input throws error", {
  expect_error(my_lm(mpg ~ hp + wt, 10))
})
