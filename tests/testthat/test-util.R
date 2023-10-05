test_that("calculate_effective_x_bins() works", {

  set.seed(123)
  data <- tibble::tibble(x = rnorm(300), y = rnorm(300))
  expect_equal(calculate_effective_x_bins(data, x), 6)
})
