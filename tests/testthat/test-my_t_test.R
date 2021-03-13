test_that("my_t_test works with any alternative", {
  expect_is(my_t_test(c(10, 15, 14, 16), "two.sided", 13), "list")
})

test_that("my_t_test works with any alternative", {
  expect_is(my_t_test(c(10, 15, 14, 16), "greater", 13), "list")
})

test_that("my_t_test works with any alternative", {
  expect_is(my_t_test(c(10, 15, 14, 16), "less", 13), "list")
})

test_that("non numeric input throws error for x and mu", {
  expect_error(my_t_test("a string", "greater", "a string"))
})

test_that("non string input for alternative throws error", {
  expect_error(my_t_test(c(10, 15, 14, 16), 10, 13))
})
