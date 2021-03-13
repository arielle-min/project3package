penguins_omit <- na.omit(palmerpenguins::penguins)
my_cl <- penguins_omit %>% dplyr::pull(species)

test_that("my_knn_cv works mathematically", {
  expect_type(my_knn_cv(penguins_omit$bill_length_mm, my_cl, 1, 5), "list")
})

test_that("non data frame or matrix input throws error for train or cl", {
  expect_error(my_knn_cv("a string", "a string", 1, 5))
})

test_that("non-numeric input throws error for k_nn or k_cv", {
  expect_error(my_knn_cv("data.frame", "factor", "a string", "a string"))
})
