test_that("calculate_effective_x_bins() works", {

  set.seed(123)
  data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
  expect_equal(calculate_effective_x_bins(data, x), 6)
})


test_that("calculate_effective_shape_value() works", {

  set.seed(123)
  data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
  expect_equal(calculate_effective_shape_value(data, x, y), 0.96955502)

  data <- data |> bind_rows(tibble(x = NA, y = 2.5))
  expect_error(calculate_effective_shape_value(data, x, y))

  data <- data |> bind_rows(tibble(x = 1.4, y = NA))
  expect_error(calculate_effective_shape_value(data, x, y))

})


