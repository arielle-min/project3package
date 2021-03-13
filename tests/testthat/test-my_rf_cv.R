test_that("my_rf_cv works mathematically", {
  expect_is(my_rf_cv(5), "numeric")
})

test_that("non numeric input throws error", {
  expect_error(my_rf_cv("a string"))
})

